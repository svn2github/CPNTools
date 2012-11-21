package org.cpntools.simulator.extensions.server;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import org.cpntools.accesscpn.engine.protocol.Packet;
import org.cpntools.accesscpn.engine.utils.BlockingQueue;
import org.cpntools.simulator.extensions.Channel;
import org.cpntools.simulator.extensions.Command;
import org.cpntools.simulator.extensions.Extension;
import org.cpntools.simulator.extensions.Option;

import dk.klafbang.tools.Pair;

/**
 * @author michael
 */
public class Handler implements Channel {

	class FeederThread extends Thread {
		public FeederThread(final String name) {
			super(name);
		}

		@Override
		public void run() {
			try {
				while (true) {
					final Packet p = new Packet();
					p.receive(in);
					packetQueue.put(p);
				}
			} catch (final IOException e) {
				// Add a fake termination packet
				packetQueue.put(new Packet(-1, EXTERNAL_COMMAND));
			}
		}
	}

	class DispatcherThread extends Thread {
		public DispatcherThread(final String name) {
			super(name);
		}

		@Override
		public void run() {
			while (true) {
				try {
					Packet p = packetQueue.get();
					switch (p.getOpcode()) {
					case 9: // extension
						p = handleCommand(p);
						sendLock.lock();
						p.send(out);
						sendLock.unlock();
						break;
					case 5: // GFC
						p = handleGFC(p);
						sendLock.lock();
						p.send(out);
						sendLock.unlock();
						break;
					case 12: // filter
						p = handleForward(p);
						if (p == null) {
							p = new Packet(7, 0);
						}
						sendLock.lock();
						p.send(out);
						sendLock.unlock();
						break;
					case 7: // invocation response
					case 3: // CB reponse
						final boolean delay = packetQueue.isEmpty();
						packetQueue.put(p);
						if (delay) {
							try {
								sleep(100);
							} catch (final InterruptedException ie) {
							}
						}
						break;
					case -1:
						return;
					default:
						assert false;
						// Ignore, this should never happen...
					}
				} catch (final IOException ioe) {
					return;
				}
			}
		}
	}

	private static final int EXTERNAL_COMMAND = 10000;

	public static Map<Integer, Extension> constructExtensionMap(final List<Extension> exns)
	        throws ConflictingExtensionsException {
		final Map<Integer, Extension> extensions = new HashMap<Integer, Extension>();
		for (final Extension e : exns) {
			if (extensions.containsKey(e.getIdentifier())) { throw new ConflictingExtensionsException(e,
			        extensions.get(e.getIdentifier())); }
			extensions.put(e.getIdentifier(), e);
		}
		return extensions;
	}

	public static Map<Integer, Map<Integer, List<Extension>>> constructSubscriptions(final List<Extension> extensions) {
		final Map<Integer, Map<Integer, List<Extension>>> subscriptions = new HashMap<Integer, Map<Integer, List<Extension>>>();
		for (final Extension e : extensions) { // We do this here as we are sure of no conflicts
			for (final Command c : e.getSubscriptions()) {
				Map<Integer, List<Extension>> command = subscriptions.get(c.getCommand());
				if (command == null) {
					command = new HashMap<Integer, List<Extension>>();
					subscriptions.put(c.getCommand(), command);
				}
				List<Extension> subcommand = command.get(c.getSubcommand());
				if (subcommand == null) {
					subcommand = new ArrayList<Extension>();
					command.put(c.getSubcommand(), subcommand);
				}
				subcommand.add(e);
			}
		}
		return subscriptions;
	}

	final DataInputStream in;
	final DataOutputStream out;
	private final Map<Integer, Extension> extensions;
	private final Map<Integer, Map<Integer, List<Extension>>> subscriptions;
	private final Map<Pair<Integer, String>, Option<?>> options;

	final Lock sendLock;

	private final Lock lock;

	final BlockingQueue<Packet> packetQueue;

	/**
	 * @param in
	 * @param out
	 * @param extensions
	 * @param string
	 * @throws ConflictingExtensionsException
	 * @throws ErrorInjectingException
	 * @throws IOException
	 */
	public Handler(final DataInputStream in, final DataOutputStream out, final List<Extension> extensions,
	        final String name) throws ConflictingExtensionsException, ErrorInjectingException, IOException {
		this.in = in;
		this.out = out;
		lock = new ReentrantLock();
		sendLock = new ReentrantLock();
		packetQueue = new BlockingQueue<Packet>();
		this.extensions = Handler.constructExtensionMap(extensions);
		subscriptions = Handler.constructSubscriptions(extensions);
		new FeederThread("Feeder " + name).start();

		doInjection(extensions);
		makeSubscriptions(subscriptions);
		options = new HashMap<Pair<Integer, String>, Option<?>>();
		registerExtensions(extensions);
		new DispatcherThread("Dispatcher " + name).start();
	}

	public Packet handleCommand(final Packet p) {
		assert p.getOpcode() == 9;
		assert p.getCommand() == Handler.EXTERNAL_COMMAND;
		p.reset();
		p.getInteger();
		final int handler = p.getInteger();
		if (handler < Extension.TESTING) {
			p.reset();
			return handleLocal(p);
		}
		final Extension extension = extensions.get(handler);
		if (extension == null) {
			final Packet error = new Packet(7, -1);
			error.addString("Extension not found!");
			return error;
		}
		try {
			final Packet result = extension.handle(p);
			return result;
		} catch (final Throwable t) {
			final Packet error = new Packet(7, -1);
			error.addString(t.getMessage());
			return error;
		}
	}

	public Packet handleForward(final Packet p) {
		final Packet request = getRequest(p);
		Packet response = getResponse(p);
		final Packet originalResponse = response;
		request.reset();
		request.getInteger();
		final int command = request.getCommand();
		final int subcommand = request.getInteger();
		final Map<Integer, List<Extension>> subscribers = subscriptions.get(command);
		if (subscribers == null) { return null; }
		final List<Extension> allSubscribers = subscribers.get(Command.ANY);
		if (allSubscribers != null) {
			for (final Extension e : allSubscribers) {
				final Packet newResponse = e.handle(request, response);
				if (newResponse != null) {
					response = newResponse;
				}
			}
		}
		final List<Extension> subcommandSubscribers = subscribers.get(subcommand);
		if (subcommandSubscribers != null) {
			for (final Extension e : subcommandSubscribers) {
				final Packet newResponse = e.handle(request, response);
				if (newResponse != null) {
					response = newResponse;
				}
			}
		}
		if (response == originalResponse) { return null; }
		return response;
	}

	public Packet handleGFC(final Packet p) {
		// TODO Auto-generated method stub
		return null;
	}

	public void lock() {
		lock.lock();
	}

	public void release() {
		lock.unlock();
	}

	/**
	 * @param p
	 * @return
	 * @throws IOException
	 */
	@Override
	public Packet send(final Packet p) throws IOException {
		lock();
		try {
			final int expectedCode;
			switch (p.getOpcode()) {
			case 9:
				expectedCode = 7;
				break;
			case 3:
				expectedCode = 3;
				break;
			default:
				expectedCode = 7;
				break;
			}
			sendLock.lock();
			try {
				p.send(out);
			} finally {
				sendLock.unlock();
			}
			Packet result;
			do {
				result = packetQueue.get();
				if (result.getOpcode() != expectedCode) {
					final boolean sleep = packetQueue.isEmpty();
					packetQueue.put(result);
					if (sleep) {
						try {
							Thread.sleep(100);
						} catch (final InterruptedException _) {
							// IGnore, we're just avoiding a busy wait
						}
					}
				}
			} while (result.getOpcode() != expectedCode);
			return result;
		} finally {
			release();
		}
	}

	private Packet createInjectPacket(final String s) {
		final Packet p = new Packet(Handler.EXTERNAL_COMMAND);
		p.addInteger(1);
		p.addString(s);
		return p;
	}

	private Packet createOptions(final Extension e) {
		final Packet p = new Packet(3, Handler.EXTERNAL_COMMAND);
		p.addInteger(101);
		p.addInteger(e.getIdentifier());
		p.addString(e.getName());
		final List<Option<?>> options = e.getOptions();
		if (options == null) {
			p.addInteger(0);
			return p;
		}
		p.addInteger(options.size());
		for (final Option<?> option : options) {
			this.options.put(Pair.createPair(e.getIdentifier(), option.getKey()), option);
			p.addInteger(option.getTypeId());
			p.addString(option.getName());
			p.addString(option.getKey());
		}
		return p;
	}

	private Packet createSubscriptionMessage(final int command, final int subcommand) {
		final Packet p = new Packet(Handler.EXTERNAL_COMMAND);
		p.addInteger(2); // Command = subscribe
		p.addInteger(1); // # subscriptions
		p.addInteger(command);
		p.addInteger(subcommand);
		return p;
	}

	private Packet getRequest(final Packet p) {
		assert p.getOpcode() == 12;
		p.reset();
		final int b = p.getInteger();
		final int i = p.getInteger();
		final int s = p.getInteger();
		p.getInteger();
		p.getInteger();
		p.getInteger(); // Ignore response
		final Packet result = new Packet(9, p.getInteger());
		for (int c = 0; c < b; c++) {
			result.addBoolean(p.getBoolean());
		}
		for (int c = 0; c < i - 1; c++) {
			result.addInteger(p.getInteger());
		}
		for (int c = 0; c < s; c++) {
			result.addString(p.getString());
		}
		return result;
	}

	private Packet getResponse(final Packet p) {
		assert p.getOpcode() == 12;
		p.reset();
		p.getInteger();
		p.getInteger();
		p.getInteger(); // Ignore request
		final int b = p.getInteger();
		final int i = p.getInteger();
		final int s = p.getInteger();
		final Packet result = new Packet(7, p.getInteger());
		for (int c = 0; c < b; c++) {
			result.addBoolean(p.getBoolean());
		}
		for (int c = 0; c < i - 1; c++) {
			result.addInteger(p.getInteger());
		}
		for (int c = 0; c < s; c++) {
			result.addString(p.getString());
		}
		return result;
	}

	private Map<Pair<Integer, String>, Option<?>> registerExtensions(final List<Extension> extensions)
	        throws IOException {
		for (final Extension e : extensions) {
			send(createOptions(e));
		}
		return options;
	}

	protected void doInjection(final List<Extension> extensions) throws ErrorInjectingException {
		for (final Extension e : extensions) {
			final String s = e.inject();
			if (s != null && !"".equals(s)) {
				try {
					final Packet p = send(createInjectPacket(s));
					if (p.getInteger() != 1) { throw new ErrorInjectingException(e, s, p.getString()); }
				} catch (final IOException ex) {
					throw new ErrorInjectingException(e, s, ex);
				}
			}
		}
	}

	protected Packet handleLocal(final Packet p) {
		assert p.getOpcode() == 9;
		assert p.getCommand() == Handler.EXTERNAL_COMMAND;
		p.reset();
		p.getInteger();
		final int command = p.getInteger();
		Packet result = null;
		switch (command) {
		case 1202:
			result = new Packet(7, 1);
			{
				final Calendar now = new GregorianCalendar();
				if (now.get(Calendar.MONTH) == Calendar.DECEMBER && now.get(Calendar.DAY_OF_MONTH) == 2) {
					result.addBoolean(true);
					result.addString("Happy birthday Britney!");
				} else {
					result.addBoolean(false);
				}

			}
			break;
		case 301:
			result = new Packet(7, 1);
			result.addString("Britney was here and broke the protocol but as nobody actually checks the result that's ok");
			break;
		case 201: {
			final int extension = p.getInteger();
			final Extension e = extensions.get(extension);
			if (e != null) {
				final int type = p.getInteger();
				final Option<?> option = options.get(Pair.createPair(extension, p.getString()));
				if (option != null) {
					switch (type) {
					case 0:
						e.setOption((Option<Integer>) option, p.getInteger());
						break;
					case 1:
						e.setOption((Option<Boolean>) option, p.getBoolean());
						break;
					case 2:
						e.setOption((Option<String>) option, p.getString());
						break;
					}
				}
			}
			return new Packet(7, 1);
		}
		}
		if (result == null) {
			result = new Packet(7, -1);
			result.addString("Unknown internal command!");
		}
		return result;
	}

	protected void makeSubscriptions(final Map<Integer, Map<Integer, List<Extension>>> subscriptions2)
	        throws IOException {
		for (final Entry<Integer, Map<Integer, List<Extension>>> entry : subscriptions2.entrySet()) {
			if (entry.getValue().containsKey(Command.ANY)) {
				send(createSubscriptionMessage(entry.getKey(), Command.ANY));
			} else {
				for (final int i : entry.getValue().keySet()) {
					send(createSubscriptionMessage(entry.getKey(), i));
				}
			}
		}
	}
}
