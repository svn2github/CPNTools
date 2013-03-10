package org.cpntools.simulator.extensions.scraper;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

import org.cpntools.accesscpn.engine.protocol.Packet;
import org.cpntools.accesscpn.engine.utils.BlockingQueue;
import org.cpntools.simulator.extensions.AbstractExtension;
import org.cpntools.simulator.extensions.Channel;
import org.cpntools.simulator.extensions.Command;
import org.cpntools.simulator.extensions.Extension;

/**
 * @author michael
 */
public class Scraper extends AbstractExtension {
	/**
	 * @author michael
	 */
	public static class Added extends Event {
		/**
		 * @param elm
		 */
		public Added(final Element elm) {
			super(EventType.ADDED, elm);
		}
	}

	/**
	 * @author michael
	 */
	public static class Changed extends Event {
		/**
		 * @param elm
		 */
		public Changed(final Element elm) {
			super(EventType.CHANGED, elm);
		}
	}

	/**
	 * @author michael
	 */
	public abstract static class Event {
		private final Element elm;
		private final EventType type;

		protected Event(final EventType type, final Element elm) {
			this.type = type;
			this.elm = elm;
		}

		/**
		 * @return
		 */
		public Element getElm() {
			return elm;
		}

		/**
		 * @return
		 */
		public EventType getType() {
			return type;
		}
	}

	/**
	 * @author michael
	 */
	public static enum EventType {
		/**
		 * 
		 */
		ADDED, /**
		 * 
		 */
		CHANGED, /**
		 * 
		 */
		REMOVED;
	}

	/**
	 * @author michael
	 */
	public static class Removed extends Event {
		/**
		 * @param elm
		 */
		public Removed(final Element elm) {
			super(EventType.REMOVED, elm);
		}
	}

	/**
	 * 
	 */
	public static final int ID = 10002;

	/**
	 * 
	 */
	public static final Scraper INSTANCE = new Scraper();

	private final Map<String, Page> pages = new HashMap<String, Page>();

	final BlockingQueue<Packet> packets = new BlockingQueue<Packet>();

	private Scraper() {
		// Hide constructor
	}

	private Scraper(final boolean b) {
		addSubscription(new Command(400, 2)); // Syntax check page
		new Thread("Scraper worker") {
			{
				setDaemon(true);
			}

			@Override
			public void run() {
				while (true) {
					final Packet packet = packets.get();
					scrape(packet);
				}
			}
		}.start();
	}

	/**
	 * @param p
	 */
	public void add(final Page p) {
		pages.put(p.getId(), p);
	}

	/**
	 * @see org.cpntools.simulator.extensions.Extension#getIdentifier()
	 */
	@Override
	public int getIdentifier() {
		return Scraper.ID;
	}

	/**
	 * @see org.cpntools.simulator.extensions.Extension#getName()
	 */
	@Override
	public String getName() {
		return "Model Scraper";
	}

	/**
	 * @param id
	 * @return
	 */
	public Page getPage(final String id) {
		return pages.get(id);
	}

	/**
	 * @see org.cpntools.simulator.extensions.Extension#handle(org.cpntools.accesscpn.engine.protocol.Packet)
	 */
	@Override
	public Packet handle(final Packet p) {
		return null;
	}

	/**
	 * @see org.cpntools.simulator.extensions.AbstractExtension#handle(org.cpntools.accesscpn.engine.protocol.Packet,
	 *      org.cpntools.accesscpn.engine.protocol.Packet)
	 */
	@Override
	public Packet handle(final Packet p, final Packet r) {
		packets.put(p);
		return null;
	}

	/**
	 * @return
	 */
	public Iterable<Page> pages() {
		return pages.values();
	}

	/**
	 * @see org.cpntools.simulator.extensions.AbstractExtension#start(org.cpntools.simulator.extensions.Channel)
	 */
	@Override
	public Extension start(final Channel c) {
		final Scraper s = new Scraper(true);
		s.setChannel(c);
		return s;
	}

	private void handleSyntaxCheck(final Packet packet) {
		packet.reset();
		packet.getInteger(); // cmd
		packet.getInteger(); // subcmd
		boolean changed = false;
		boolean added = false;
		final String id = packet.getString();
		final String name = packet.getString();
		packet.getInteger(); // Prime multiplicity
		Page p = getPage(id);
		if (p == null) {
			added = true;
			p = new Page(id, name);
			add(p);
		} else {
			changed |= p.setName(name);
		}

		final List<String> keepers = new ArrayList<String>();
		for (int i = packet.getInteger(); i > 0; i--) {
			keepers.add(packet.getString());
		}
		// Keep places
		keepers.clear();

		for (int i = packet.getInteger(); i > 0; i--) {
			keepers.add(packet.getString());
		}
		final Map<String, Transition> removedT = p.retainTransitions(keepers);
		final HashSet<Transition> changedT = new HashSet<Transition>();
		final HashSet<Transition> addedT = new HashSet<Transition>();

		for (int i = packet.getInteger(); i > 0; i--) {
			packet.getString(); // id
			packet.getString(); // name
			packet.getString(); // colset
			packet.getString(); // initmark
		}
		// Ignore places

		for (int i = packet.getInteger(); i > 0; i--) {
			packet.getString(); // id
			packet.getString(); // fusid
			packet.getString(); // name
			packet.getString(); // colset
			packet.getString(); // initmark
		}
		// Ignore fusion places

		for (int i = packet.getInteger(); i > 0; i--) {
			packet.getString(); // id
			packet.getString(); // name
			packet.getString(); // subpageid
			for (int j = packet.getInteger(); j > 0; j--) {
				packet.getString(); // portid
				packet.getString(); // socketid
			}
		}
		// Ignore subst transitions

		for (int i = packet.getInteger(); i > 0; i--) {
			boolean localchanged = false;
			final String tid = packet.getString();
			final String tname = packet.getString();
			final String tguard = packet.getString();
			final String ttime = packet.getString();
			final String tcode = packet.getString();
			final String tchannel = packet.getString();
			final String tpriority = packet.getString();
			final boolean tcontrollable = packet.getBoolean();
			Transition t = removedT.remove(tid);
			if (t == null) {
				t = new Transition(tid, tname, p, tguard, tpriority, ttime, tcode, tchannel, tcontrollable);
				addedT.add(t);
			} else {
				localchanged |= t.setName(tname);
				localchanged |= t.setGuard(tguard);
				localchanged |= t.setTime(ttime);
				localchanged |= t.setCode(tcode);
				localchanged |= t.setChannel(tchannel);
				localchanged |= t.setPriority(tpriority);
				localchanged |= t.setControllable(tcontrollable);
				if (localchanged) {
					changedT.add(t);
				}
			}
			p.add(t);

			for (int j = packet.getInteger(); j > 0; j--) { // input
				packet.getString(); // id
				packet.getString(); // placeid
				packet.getString(); // expr
			}
			for (int j = packet.getInteger(); j > 0; j--) { // output
				packet.getString(); // id
				packet.getString(); // placeid
				packet.getString(); // expr
			}
			for (int j = packet.getInteger(); j > 0; j--) { // inoutput
				packet.getString(); // id
				packet.getString(); // placeid
				packet.getString(); // expr
			}
			for (int j = packet.getInteger(); j > 0; j--) { // inhibitor
				packet.getString(); // id
				packet.getString(); // placeid
				packet.getString(); // expr
			}
			for (int j = packet.getInteger(); j > 0; j--) { // reset
				packet.getString(); // id
				packet.getString(); // placeid
				packet.getString(); // expr
			}
		}

		changed |= !removedT.isEmpty();
		changed |= !addedT.isEmpty();
		changed |= !changedT.isEmpty();

		if (added) {
			assert changedT.isEmpty();
			assert removedT.isEmpty();
			notify(new Added(p));
			for (final Transition t : p.transitions()) {
				notify(new Added(t));
			}
		} else if (changed) {
			notify(new Changed(p));
			for (final Transition t : removedT.values()) {
				notify(new Removed(t));
			}
			for (final Transition t : addedT) {
				notify(new Added(t));
			}
			for (final Transition t : changedT) {
				notify(new Changed(t));
			}
		}
	}

	private void notify(final Event e) {
		setChanged();
		notifyObservers(e);
	}

	protected void scrape(final Packet packet) {
		packet.reset();
		final int command = packet.getInteger();
		switch (command) {
		case 400: {
			final int subcommand = packet.getInteger();
			switch (subcommand) {
			case 2:
				handleSyntaxCheck(packet);
			}
		}
			break;
		}

	}
}
