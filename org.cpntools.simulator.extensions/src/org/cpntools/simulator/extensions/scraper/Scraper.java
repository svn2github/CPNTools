package org.cpntools.simulator.extensions.scraper;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Observer;
import java.util.Set;

import org.cpntools.accesscpn.engine.protocol.Packet;
import org.cpntools.accesscpn.engine.utils.BlockingQueue;
import org.cpntools.simulator.extensions.AbstractExtension;
import org.cpntools.simulator.extensions.Channel;
import org.cpntools.simulator.extensions.Command;
import org.cpntools.simulator.extensions.Extension;
import org.cpntools.simulator.extensions.scraper.Arc.Type;

/**
 * @author michael
 */
public class Scraper extends AbstractExtension implements ElementDictionary {
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
	public static class ArcChanged extends Event {
		/**
		 * @param elm
		 */
		public ArcChanged(final Element elm) {
			super(EventType.ARC_CHANGED, elm);
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
		REMOVED, /** */
		ARC_CHANGED;
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

	private final Map<String, Page> pages = new HashMap<String, Page>();

	final BlockingQueue<Packet> packets = new BlockingQueue<Packet>();

	/**
	 * 
	 */
	public Scraper() {
	}

	private Scraper(final boolean b) {
		addLazySubscription(new Command(400, 2)); // Syntax check page
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
	 * @see java.util.Observable#addObserver(java.util.Observer)
	 */
	@Override
	public void addObserver(final Observer o) {
		makeLazySubscriptions();
		super.addObserver(o);
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
			p = new Page(this, id, name);
			add(p);
		} else {
			changed |= p.setName(name);
		}

		final List<String> pkeepers = new ArrayList<String>();
		for (int i = packet.getInteger(); i > 0; i--) {
			pkeepers.add(packet.getString());
		}
		// Keep places

		final List<String> tkeepers = new ArrayList<String>();
		for (int i = packet.getInteger(); i > 0; i--) {
			tkeepers.add(packet.getString());
		}

		final Map<String, Place> removedP = p.retainPlaces(pkeepers);
		final HashSet<Place> changedP = new HashSet<Place>();
		final HashSet<Place> addedP = new HashSet<Place>();
		final HashSet<Place> arcChangedP = new HashSet<Place>();
		for (int i = packet.getInteger(); i > 0; i--) {
			boolean localchanged = false;
			final String pid = packet.getString(); // id
			final String pname = packet.getString(); // name
			final String ptype = packet.getString(); // colset
			final String pinitmark = packet.getString(); // initmark
			Place pp = removedP.remove(pid);
			if (pp == null) {
				pp = new Place(this, pid, pname, p, ptype, pinitmark);
				addedP.add(pp);
			} else {
				localchanged |= pp.setName(pname);
				localchanged |= pp.setType(ptype);
				localchanged |= pp.setInitMark(pinitmark);
				if (localchanged) {
					changedP.add(pp);
				}
			}
			p.add(pp);
		}
		// Places

		for (int i = packet.getInteger(); i > 0; i--) {
			boolean localchanged = false;
			final String pid = packet.getString(); // id
			packet.getString(); // fusid TODO should be handled correctly instead!
			final String pname = packet.getString(); // name
			final String ptype = packet.getString(); // colset
			final String pinitmark = packet.getString(); // initmark
			Place pp = removedP.remove(pid);
			if (pp == null) {
				pp = new Place(this, pid, pname, p, ptype, pinitmark);
				addedP.add(pp);
			} else {
				localchanged |= pp.setName(pname);
				localchanged |= pp.setType(ptype);
				localchanged |= pp.setInitMark(pinitmark);
				if (localchanged) {
					changedP.add(pp);
				}
			}
			p.add(pp);
		}
		// Fusion places

		changed |= !removedP.isEmpty();
		changed |= !addedP.isEmpty();
		changed |= !changedP.isEmpty();

		final Map<String, Transition> removedT = p.retainTransitions(tkeepers);
		final HashSet<Transition> changedT = new HashSet<Transition>();
		final HashSet<Transition> addedT = new HashSet<Transition>();
		final HashSet<Transition> arcChangedT = new HashSet<Transition>();
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
				t = new Transition(this, tid, tname, p, tguard, tpriority, ttime, tcode, tchannel, tcontrollable);
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

			boolean arcChanged = false;
			t.prepareNewArcs();
			arcChanged |= doArcs(t, packet, Arc.Type.INPUT);
			arcChanged |= doArcs(t, packet, Arc.Type.OUTPUT);
			arcChanged |= doArcs(t, packet, Arc.Type.BOTHDIR);
			arcChanged |= doArcs(t, packet, Arc.Type.INHIBITOR);
			arcChanged |= doArcs(t, packet, Arc.Type.RESET);
			final Set<Place> finishNewArcs = t.finishNewArcs();
			arcChangedP.addAll(finishNewArcs);
			arcChanged |= !finishNewArcs.isEmpty();
			if (arcChanged) {
				arcChangedT.add(t);
			}
		}

		changed |= !removedT.isEmpty();
		changed |= !addedT.isEmpty();
		changed |= !changedT.isEmpty();

		changed |= !arcChangedP.isEmpty();
		changed |= !arcChangedT.isEmpty();

		if (added) {
			assert changedT.isEmpty();
			assert removedT.isEmpty();
			notify(new Added(p));
			for (final Place pp : p.places()) {
				notify(new Added(pp));
			}
			for (final Transition t : p.transitions()) {
				notify(new Added(t));
			}
		} else if (changed) {
			notify(new Changed(p));
			for (final Place pp : removedP.values()) {
				notify(new Removed(pp));
			}
			for (final Place pp : addedP) {
				notify(new Added(pp));
			}
			for (final Place pp : changedP) {
				notify(new Changed(pp));
			}
			for (final Transition t : removedT.values()) {
				notify(new Removed(t));
			}
			for (final Transition t : addedT) {
				notify(new Added(t));
			}
			for (final Transition t : changedT) {
				notify(new Changed(t));
			}
			for (final Place pp : arcChangedP) {
				notify(new ArcChanged(pp));
			}
			for (final Transition t : arcChangedT) {
				notify(new ArcChanged(t));
			}
		}
	}

	private boolean doArcs(final Transition t, final Packet packet, final Type type) {
		boolean localChanged = false;
		for (int j = packet.getInteger(); j > 0; j--) { // input
			final String id = packet.getString(); // id
			final String placeId = packet.getString(); // placeid
			final String expr = packet.getString(); // expr
			final Place p = t.getPage().getPlace(placeId);
			localChanged = new Arc(this, id, expr, type, t.getPage(), t, p).addToNodes() | localChanged;
		}
		return localChanged;
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

	/**
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		return 7;
	}

	/**
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(final Object o) {
		return o instanceof Scraper;
	}

	private final Map<String, Element> elements = new HashMap<String, Element>();

	/**
	 * @see org.cpntools.simulator.extensions.scraper.ElementDictionary#put(java.lang.String,
	 *      org.cpntools.simulator.extensions.scraper.Element)
	 */
	@Override
	public void put(final String id, final Element element) {
		elements.put(id, element);
	}

	/**
	 * @see org.cpntools.simulator.extensions.scraper.ElementDictionary#get(java.lang.String)
	 */
	@Override
	public Element get(final String id) {
		return elements.get(id);
	}
}
