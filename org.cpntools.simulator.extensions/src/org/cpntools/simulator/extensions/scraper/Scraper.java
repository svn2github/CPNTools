package org.cpntools.simulator.extensions.scraper;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
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
public class Scraper extends AbstractExtension implements ElementDictionary, Iterable<Page> {
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
		ADDED, /** */
		ARC_CHANGED, /**
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

	private final Map<String, Element> elements = new HashMap<String, Element>();

	private final Map<String, Page> pages = new HashMap<String, Page>();

	final BlockingQueue<Packet> packets = new BlockingQueue<Packet>();

	/**
	 * 
	 */
	public Scraper() {
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
	 * @see java.util.Observable#addObserver(java.util.Observer)
	 */
	@Override
	public void addObserver(final Observer o) {
		notifyMe(o);
		super.addObserver(o);
	}

	/**
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(final Object o) {
		return o instanceof Scraper;
	}

	/**
	 * @see org.cpntools.simulator.extensions.scraper.ElementDictionary#get(java.lang.String)
	 */
	@Override
	public Element get(final String id) {
		return elements.get(id);
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
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		return 7;
	}

	/**
	 * @see java.lang.Iterable#iterator()
	 */
	@Override
	public Iterator<Page> iterator() {
		return pages.values().iterator();
	}

	/**
	 * @param o
	 */
	public void notifyMe(final Observer o) {
		for (final Page p : pages.values()) {
			o.update(this, new Added(p));
			for (final Place place : p.places()) {
				o.update(this, new Added(place));
			}
			for (final Transition transition : p.transitions()) {
				o.update(this, new Added(transition));
			}
			for (final Place place : p.places()) {
				o.update(this, new ArcChanged(place));
			}
			for (final Transition transition : p.transitions()) {
				o.update(this, new ArcChanged(transition));
			}
		}
	}

	/**
	 * @return
	 */
	public Iterable<Page> pages() {
		return pages.values();
	}

	/**
	 * @see org.cpntools.simulator.extensions.scraper.ElementDictionary#put(java.lang.String,
	 *      org.cpntools.simulator.extensions.scraper.Element)
	 */
	@Override
	public void put(final String id, final Element element) {
		elements.put(id, element);
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

	private void handleSyntaxCheck(final Packet packet) {
		System.out.println(packet);
		packet.reset();
		packet.getInteger(); // cmd
		packet.getInteger(); // subcmd
		boolean changed = false;
		boolean added = false;
		final String id = packet.getString();
		final String name = packet.getString();
		final int prime = packet.getInteger(); // Prime multiplicity
		Page p = getPage(id);
		if (p == null) {
			added = true;
			p = new Page(this, id, name, prime > 0);
			add(p);
		} else {
			changed |= p.setName(name);
			changed |= p.setPrime(prime > 0);
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
			boolean localchanged = false;
			boolean justChanged = true;
			final String sid = packet.getString(); // id
			final String sname = packet.getString(); // name
			final String subpage = packet.getString(); // subpageid
			Transition s = removedT.remove(sid);
			if (s == null) {
				s = new Transition(this, sid, sname, p, "", "", "", "", "", subpage, true, true);
				addedT.add(s);
				justChanged = false;
			} else {
				localchanged |= s.setName(sname);
				localchanged |= s.setGuard("");
				localchanged |= s.setPriority("");
				localchanged |= s.setTime("");
				localchanged |= s.setCode("");
				localchanged |= s.setChannel("");
				localchanged |= s.setSubpage(subpage);
				localchanged |= s.setControllable(true);
				localchanged |= s.setSubstitution(true);
			}
			s.prepareNewArcs();
			s.prepareNewPortSocket();
			boolean arcChanged = false;
			int aid = 0;
			for (int j = packet.getInteger(); j > 0; j--) {
				final String port = packet.getString(); // portid
				final String socket = packet.getString(); // socketid
				localchanged |= s.addPortSocket(port, socket);
				arcChanged = new Arc(this, sid + "_" + aid++, "", Arc.Type.BOTHDIR, p, s, p.getPlace(socket))
				        .addToNodes() | arcChanged;
			}
			localchanged |= !s.finishNewPortSocket().isEmpty();
			final Set<Place> finishNewArcs = s.finishNewArcs();
			arcChangedP.addAll(finishNewArcs);
			arcChanged |= !finishNewArcs.isEmpty();
			if (localchanged && justChanged) {
				changedT.add(s);
			}
			if (arcChanged && justChanged) {
				arcChangedT.add(s);
			}
		}

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
				t = new Transition(this, tid, tname, p, tguard, tpriority, ttime, tcode, tchannel, "", tcontrollable,
				        false);
				addedT.add(t);
			} else {
				localchanged |= t.setName(tname);
				localchanged |= t.setGuard(tguard);
				localchanged |= t.setTime(ttime);
				localchanged |= t.setCode(tcode);
				localchanged |= t.setChannel(tchannel);
				localchanged |= t.setPriority(tpriority);
				localchanged |= t.setSubpage("");
				localchanged |= t.setControllable(tcontrollable);
				localchanged |= t.setSubstitution(false);
				t.prepareNewPortSocket();
				localchanged |= !t.finishNewPortSocket().isEmpty();
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

		for (final Transition t : removedT.values()) { // Remove arcs connected to removed transitions
			t.prepareNewArcs();
			arcChangedP.addAll(t.finishNewArcs());
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
