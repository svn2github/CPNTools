package org.cpntools.simulator.extensions.server;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import org.cpntools.accesscpn.engine.protocol.Packet;
import org.cpntools.accesscpn.engine.protocol.handler.GramHandler;
import org.cpntools.accesscpn.engine.utils.BlockingQueue;
import org.cpntools.simulator.extensions.Channel;
import org.cpntools.simulator.extensions.Command;
import org.cpntools.simulator.extensions.Extension;
import org.cpntools.simulator.extensions.Instrument;
import org.cpntools.simulator.extensions.NamedRPCHandler;
import org.cpntools.simulator.extensions.Option;
import org.cpntools.simulator.extensions.SubscriptionHandler;
import org.cpntools.simulator.extensions.scraper.DummyElement;
import org.cpntools.simulator.extensions.scraper.Element;
import org.cpntools.simulator.extensions.scraper.Scraper;

import dk.klafbang.tools.Pair;

/**
 * @author michael
 */
public class Handler implements Channel {

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
						try {
							p = handleCommand(p);
							sendLock.lock();
// System.out.println("SEnda " + p);
							p.send(out);
						} catch (final Exception e) {
							p = new Packet(7, -1);
							p.addBoolean(false);
							p.addString(e.toString());
							p.send(out);
						}
						sendLock.unlock();
						break;
					case 5: // GFC
						p = handleGFC(p);
						sendLock.lock();
// System.out.println("SEndb " + p);
						p.send(out);
						sendLock.unlock();
						break;
					case 12: // filter
						p = handleForward(p);
						if (p == null) {
							p = new Packet(7, 0);
						}
						sendLock.lock();
// System.out.println("SEndc " + p);
						p.send(out);
						sendLock.unlock();
						break;
					case 7: // invocation response
					case 3: // CB reponse
						final boolean delay = packetQueue.isEmpty();
						packetQueue.put(p);
						if (delay) {
							try {
								Thread.sleep(100);
							} catch (final InterruptedException ie) {
								// Ignore
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
					ioe.printStackTrace();
					return;
				}
			}
		}
	}

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
// System.out.println("Received " + p);
					packetQueue.put(p);
				}
			} catch (final IOException e) {
				// Add a fake termination packet
				packetQueue.put(new Packet(-1, Handler.EXTERNAL_COMMAND));
			}
		}
	}

	/**
	 * 
	 */
	public static final int EXTERNAL_COMMAND = 10000;

	/**
	 * @param exns
	 * @return
	 * @throws ConflictingExtensionsException
	 */
	public static Map<Integer, Extension> constructExtensionMap(final Collection<Extension> exns)
	        throws ConflictingExtensionsException {
		final Map<Integer, Extension> extensions = new HashMap<Integer, Extension>();
		for (final Extension e : exns) {
			if (extensions.containsKey(e.getIdentifier())) { throw new ConflictingExtensionsException(e,
			        extensions.get(e.getIdentifier())); }
			extensions.put(e.getIdentifier(), e);
		}
		return extensions;
	}

	/**
	 * @param extensions
	 * @return
	 */
	public static Map<Integer, Map<Integer, Boolean>> constructPrefilters(final Collection<Extension> extensions) {
		final Map<Integer, Map<Integer, Boolean>> prefilters = new HashMap<Integer, Map<Integer, Boolean>>();
		for (final Extension e : extensions) { // We do this here as we are sure of no conflicts
			for (final Command c : e.getSubscriptions()) {
				Map<Integer, Boolean> prefilter = prefilters.get(c.getCommand());
				if (prefilter == null) {
					prefilter = new HashMap<Integer, Boolean>();
					prefilters.put(c.getCommand(), prefilter);
				}
				if (c.isPrefilter()) {
					prefilter.put(c.getSubcommand(), true);
					prefilter.put(Command.ANY, true);
				}
			}
		}
		return prefilters;
	}

	/**
	 * @param extensions
	 * @return
	 */
	public static Map<Integer, Map<Integer, List<SubscriptionHandler>>> constructSubscriptions(
	        final Collection<Extension> extensions) {
		final Map<Integer, Map<Integer, List<SubscriptionHandler>>> subscriptions = new HashMap<Integer, Map<Integer, List<SubscriptionHandler>>>();
		for (final Extension e : extensions) { // We do this here as we are sure of no conflicts
			for (final Command c : e.getSubscriptions()) {
				Handler.addSingleSubscription(subscriptions, c, e);
			}
		}
		return subscriptions;
	}

	private static void addSingleSubscription(
	        final Map<Integer, Map<Integer, List<SubscriptionHandler>>> subscriptions, final Command c,
	        final SubscriptionHandler e) {
		Map<Integer, List<SubscriptionHandler>> command = subscriptions.get(c.getCommand());
		if (command == null) {
			command = new HashMap<Integer, List<SubscriptionHandler>>();
			subscriptions.put(c.getCommand(), command);
		}
		List<SubscriptionHandler> subcommand = command.get(c.getSubcommand());
		if (subcommand == null) {
			subcommand = new ArrayList<SubscriptionHandler>();
			command.put(c.getSubcommand(), subcommand);
		}
		subcommand.add(e);
	}

	private final Map<Integer, Extension> extensions;
	private final Map<Class<? extends Extension>, Extension> extensionTypes = new HashMap<Class<? extends Extension>, Extension>();
	private final Lock lock;
	private final Map<Pair<Integer, String>, Option<?>> options;
	private final Map<Pair<Integer, String>, Instrument> instruments;
	private final Map<Integer, Map<Integer, Packet>> prefilterPackages = new HashMap<Integer, Map<Integer, Packet>>();
	private final Map<Integer, Map<Integer, Boolean>> prefilters;
	private final Map<Integer, Map<Integer, List<SubscriptionHandler>>> subscriptions;

	final DataInputStream in;

	final DataOutputStream out;

	final BlockingQueue<Packet> packetQueue;

	final Lock sendLock;

	/**
	 * @param in
	 * @param out
	 * @param extensions
	 * @param name
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
		constructTypeMap(extensions);
		this.extensions = Handler.constructExtensionMap(extensionTypes.values());
		subscriptions = Handler.constructSubscriptions(extensionTypes.values());
		prefilters = Handler.constructPrefilters(extensionTypes.values());
		new FeederThread("Feeder " + name).start();

		registerRPC(extensionTypes.values());
		doInjection(extensionTypes.values());
		makeSubscriptions(subscriptions, prefilters);
		options = new HashMap<Pair<Integer, String>, Option<?>>();
		instruments = new HashMap<Pair<Integer, String>, Instrument>();
		registerExtensions(extensionTypes.values());
		new DispatcherThread("Dispatcher " + name).start();
	}

	private int structureindex = 0;

	private void registerRPC(final Collection<Extension> exts) throws ErrorInjectingException {
		for (final Extension e : exts) {
			final Object o = e.getRPCHandler();
			if (o != null) {
				String prefix = "";
				if (o instanceof NamedRPCHandler) {
					final NamedRPCHandler n = (NamedRPCHandler) o;
					prefix = n.structureName() + ".";
					g.addNamedHandler(n.structureName(), o);
				} else {
					g.addLocalHandler(o);
				}
				final int index = structureindex++;
				final StringBuilder s = new StringBuilder("local\nstructure CPN'RPCStructure'");
				s.append(index);
				s.append(" = struct\n");
				s.append("local open ExtExecute in\n");
				for (final Method m : o.getClass().getMethods()) {
					if (m.getDeclaringClass() != Object.class) {
						try {
							final StringBuilder sb = new StringBuilder("fun ");
							sb.append(m.getName());
							sb.append('(');
							int param = 0;
							final Class<?>[] parameterTypes = m.getParameterTypes();
							for (int i = 0; i < parameterTypes.length; i++) {
								if (param > 0) {
									sb.append(", ");
								}
								sb.append("CPN'param");
								sb.append(param++);
							}
							sb.append(") = ExtExecute.execute");
							if (m.getReturnType() == Void.class | m.getReturnType() == Void.TYPE) {
								// Do nothing
							} else if (m.getReturnType() == Boolean.class || m.getReturnType() == Boolean.TYPE) {
								sb.append("Bool");
							} else if (m.getReturnType() == Integer.class || m.getReturnType() == Integer.TYPE) {
								sb.append("Int");
							} else if (m.getReturnType() == String.class) {
								sb.append("String");
							} else {
								throw new Exception("Unknown return type (" + m.getReturnType() + ") for method "
								        + m.getName());
							}
							sb.append(" \"");
							sb.append(prefix);
							sb.append(m.getName());
							sb.append("\" [");
							param = 0;
							for (final Class<?> clazz : m.getParameterTypes()) {
								if (param > 0) {
									sb.append(", ");
								}
								if (clazz == Boolean.class || clazz == Boolean.TYPE) {
									sb.append("vBOOL (");
								} else if (clazz == Integer.class || clazz == Integer.TYPE) {
									sb.append("vINT (");
								} else if (clazz == String.class) {
									sb.append("vSTRING (");
								} else {
									throw new Exception("Unknown parameter type (" + clazz + ") for parameter "
									        + (param + 1) + " of method " + m.getName());
								}
								sb.append("CPN'param");
								sb.append(param++);
								sb.append(')');
							}
							sb.append("]\n");
							s.append(sb);
						} catch (final Exception _) {
							// Probably because wrong parameter types
						}
					}
				}
				s.append("end\nend\nin\n");
				if (o instanceof NamedRPCHandler) {
					final NamedRPCHandler n = (NamedRPCHandler) o;
					s.append("structure ");
					s.append(n.structureName());
					s.append(" = CPN'RPCStructure'");
					s.append(index);
				} else {
					s.append("open CPN'RPCStructure'");
					s.append(index);
				}
				s.append("\nend");
// System.out.println(s);
				inject(e, s.toString());
			}
		}
	}

	/**
	 * @param extensions
	 */
	public void constructTypeMap(@SuppressWarnings("hiding") final List<Extension> extensions) {
		extensionTypes.put(Scraper.class, new Scraper().start(this));
		for (final Extension e : extensions) {
			final Extension instance = e.start(this);
			extensionTypes.put(instance.getClass(), instance);
		}
	}

	/**
	 * @see org.cpntools.simulator.extensions.Channel#evaluate(java.lang.String)
	 */
	@Override
	public String evaluate(final String expresion) {
		return "val a = 3 : int\nval b = 5 : int\n";
	}

	/**
	 * @see org.cpntools.simulator.extensions.Channel#getExtension(java.lang.Class)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public <T extends Extension> T getExtension(final Class<T> clazz) {
		return (T) extensionTypes.get(clazz);
	}

	/**
	 * @param p
	 * @return
	 */
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
			if (result == null) { return new Packet(7, 1); }
			return result;
		} catch (final Throwable t) {
			final Packet error = new Packet(7, -1);
			error.addString(t.getMessage());
			return error;
		}
	}

	/**
	 * @param p
	 * @return
	 */
	public Packet handleForward(final Packet p) {
		final int serial = getSerial(p);
		Packet request = getRequest(p);
		Packet response = getResponse(p);
		request.reset();
		final int command = request.getInteger();
		final int subcommand = request.getInteger();
		final Map<Integer, List<SubscriptionHandler>> subscribers = subscriptions.get(command);
		if (subscribers == null) {
			if (response == null) {
				request.setOpcode(13);
				return request;
			}
			return response;
		}
		final List<SubscriptionHandler> subs = new ArrayList<SubscriptionHandler>();
		final List<SubscriptionHandler> allSubscribers = subscribers.get(Command.ANY);
		if (allSubscribers != null) {
			subs.addAll(allSubscribers);
		}
		final List<SubscriptionHandler> subcommandSubscribers = subscribers.get(subcommand);
		if (subcommandSubscribers != null) {
			subs.addAll(subcommandSubscribers);
		}
		if (response == null) {
			final Map<Integer, Packet> prefilterPackage = new HashMap<Integer, Packet>();
			prefilterPackages.put(serial, prefilterPackage);
			for (final SubscriptionHandler e : subs) {
				final Packet newRequest = e.prefilter(request);
				if (newRequest != null) {
					request = newRequest;
				}
				prefilterPackage.put(e.getIdentifier(), request);
			}
		} else {
			Collections.reverse(subs);
			final Map<Integer, Packet> prefilterPackage = prefilterPackages.remove(serial);
			for (final SubscriptionHandler e : subs) {
				final Packet newResponse = e.handle(
				        prefilterPackage == null ? request : prefilterPackage.get(e.getIdentifier()), response);
				if (newResponse != null) {
					response = newResponse;
				}
			}
		}
		if (response == null) {
			request.setOpcode(13);
			return request;
		} // Prefilter
// if (response == originalResponse) { return null; }
		return response;
	}

	private final GramHandler g = new GramHandler();

	/**
	 * @param p
	 * @return
	 */
	public Packet handleGFC(final Packet p) {
		p.getCommand(); // 2
		try {
			final Object result = g.handle(p.getParameters());
			if (p.getReturnType() != null && !p.getReturnType().equals(Void.class) && result != null
			        && p.getReturnType().isAssignableFrom(result.getClass())) { return new Packet(5,
			        Collections.singletonList(result)); }
		} catch (final Exception e) {
			// Ignore if we do not return normally
		}
		if (String.class.equals(p.getReturnType())) { return new Packet(5,
		        Collections.singletonList("Error in execution")); }
		if (Integer.class.equals(p.getReturnType())) { return new Packet(5, Collections.singletonList(-1)); }
		if (Boolean.class.equals(p.getReturnType())) { return new Packet(5, Collections.singletonList(false)); }
		return new Packet(5, Collections.emptyList());
	}

	/**
	 * 
	 */
	public void lock() {
		lock.lock();
	}

	/**
	 * 
	 */
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
// System.out.println("SEndd " + p);
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

	/**
	 * @see org.cpntools.simulator.extensions.Channel#subscribe(org.cpntools.simulator.extensions.Command,
	 *      org.cpntools.simulator.extensions.SubscriptionHandler)
	 */
	@Override
	public synchronized void subscribe(final Command command, final SubscriptionHandler h) throws IOException {
		final Map<Integer, List<SubscriptionHandler>> subs = subscriptions.get(command.getCommand());
		boolean subscribe = false;
		if (subs == null) {
			subscribe = true;
		} else {
			if (!subs.containsKey(command.getSubcommand()) && !subs.containsKey(Command.ANY)) {
				subscribe = true;
			}
		}
		Handler.addSingleSubscription(subscriptions, command, h);
		if (subscribe) {
			send(createSubscriptionMessage(command.getCommand(), command.getSubcommand(), command.isPrefilter()));
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
		@SuppressWarnings("hiding")
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

		p.addInteger(e.getInstruments().size());
		for (final Instrument i : e.getInstruments()) {
			instruments.put(Pair.createPair(e.getIdentifier(), i.getKey()), i);
			p.addInteger(i.getTargets());
			p.addString(i.getName());
			p.addString(i.getToolTip());
			p.addString(i.getKey());
			p.addString(i.getToolBox());
		}
		return p;
	}

	private Packet createSubscriptionMessage(final int command, final int subcommand, final Boolean prefilter) {
		final Packet p = new Packet(Handler.EXTERNAL_COMMAND);
		p.addInteger(2); // Command = subscribe
		p.addInteger(1); // # subscriptions
		p.addInteger(command);
		p.addInteger(subcommand);
		if (prefilter != null && prefilter.booleanValue()) {
			p.addBoolean(true);
		} else {
			p.addBoolean(false);
		}
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
		p.getInteger(); // Ignore serial
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
		final int ob = p.getInteger();
		final int oi = p.getInteger();
		final int os = p.getInteger();
		final int b = p.getInteger();
		final int i = p.getInteger();
		final int s = p.getInteger();
		p.getInteger(); // Ignore serial
		if (b < 0) { return null; // Prefilter
		}
		for (int j = 0; j < ob; j++) {
			p.getBoolean();
		}
		for (int j = 0; j < oi; j++) {
			p.getInteger();
		}
		for (int j = 0; j < os; j++) {
			p.getString();
		}
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

	private int getSerial(final Packet p) {
		assert p.getOpcode() == 12;
		p.reset();
		p.getInteger();
		p.getInteger();
		p.getInteger(); // Ignore request
		p.getInteger();
		p.getInteger();
		p.getInteger(); // Ignore response
		return p.getInteger();
	}

	private Map<Pair<Integer, String>, Option<?>> registerExtensions(
	        @SuppressWarnings("hiding") final Collection<Extension> extensions) throws IOException {
		for (final Extension e : extensions) {
			send(createOptions(e));
		}
		return options;
	}

	protected void doInjection(@SuppressWarnings("hiding") final Collection<Extension> extensions)
	        throws ErrorInjectingException {
		for (final Extension e : extensions) {
			final String s = e.inject();
			inject(e, s);
		}
	}

	private void inject(final Extension e, final String s) throws ErrorInjectingException {
		if (s != null && !"".equals(s)) {
			try {
				final Packet p = send(createInjectPacket(s));
// System.out.println(p);
				if (p.getInteger() != 1) { throw new ErrorInjectingException(e, s, p.getString()); }
			} catch (final IOException ex) {
				throw new ErrorInjectingException(e, s, ex);
			}
		}
	}

	@SuppressWarnings("unchecked")
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
		case 202:
			final int extension = p.getInteger();
			final Extension e = extensions.get(extension);
			if (e != null) {
				final Instrument i = instruments.get(Pair.createPair(extension, p.getString()));
				if (i != null) {
					final Scraper scraper = getExtension(Scraper.class);
					Element element = null;
					final String id = p.getString();
					if (scraper != null) {
						element = scraper.get(id);
					}
					if (element == null) {
						element = new DummyElement(id);
					}
					e.invokeInstrument(i, element);
				}
			}
			return new Packet(7, 1);
		}
		if (result == null) {
			result = new Packet(7, -1);
			result.addString("Unknown internal command!");
		}
		return result;
	}

	protected List<Extension> instantiate(@SuppressWarnings("hiding") final List<Extension> extensions) {
		final List<Extension> result = new ArrayList<Extension>(extensions.size());
		for (final Extension e : extensions) {
			result.add(e.start(this));
		}
		return result;
	}

	protected void makeSubscriptions(final Map<Integer, Map<Integer, List<SubscriptionHandler>>> subscriptions2,
	        final Map<Integer, Map<Integer, Boolean>> prefilters2) throws IOException {
		for (final Entry<Integer, Map<Integer, List<SubscriptionHandler>>> entry : subscriptions2.entrySet()) {
			if (entry.getValue().containsKey(Command.ANY)) {
				send(createSubscriptionMessage(entry.getKey(), Command.ANY,
				        prefilters2.get(entry.getKey()).get(Command.ANY)));
			} else {
				for (final int i : entry.getValue().keySet()) {
					send(createSubscriptionMessage(entry.getKey(), i, prefilters2.get(entry.getKey()).get(i)));
				}
			}
		}
	}
}
