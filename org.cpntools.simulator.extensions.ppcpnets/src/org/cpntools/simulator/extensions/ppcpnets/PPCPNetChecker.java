package org.cpntools.simulator.extensions.ppcpnets;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Observable;
import java.util.Observer;
import java.util.Set;

import org.cpntools.accesscpn.engine.protocol.Packet;
import org.cpntools.simulator.extensions.AbstractExtension;
import org.cpntools.simulator.extensions.Channel;
import org.cpntools.simulator.extensions.Command;
import org.cpntools.simulator.extensions.Option;
import org.cpntools.simulator.extensions.scraper.Arc;
import org.cpntools.simulator.extensions.scraper.Element;
import org.cpntools.simulator.extensions.scraper.Node;
import org.cpntools.simulator.extensions.scraper.Page;
import org.cpntools.simulator.extensions.scraper.Place;
import org.cpntools.simulator.extensions.scraper.Scraper;
import org.cpntools.simulator.extensions.scraper.Scraper.Event;
import org.cpntools.simulator.extensions.scraper.Transition;
import org.cpntools.simulator.extensions.scraper.types.Bool;
import org.cpntools.simulator.extensions.scraper.types.Int;
import org.cpntools.simulator.extensions.scraper.types.Other;
import org.cpntools.simulator.extensions.scraper.types.Type;
import org.cpntools.simulator.extensions.scraper.types.Unit;
import org.cpntools.simulator.extensions.tools.labels.LabelManager;

import dk.klafbang.tools.Pair;

/**
 * @author michael
 */
public class PPCPNetChecker extends AbstractExtension implements Observer {
	/**
	 * 
	 */
	public static final int ID = 10007;
	private static final String PID = "pid";
	private boolean added = false, discovering = false;
	private final Map<String, Place> channelPlaces = new HashMap<String, Place>();

	@SuppressWarnings("unused")
	private final Option<Boolean> check;
	private final Option<Boolean> discover;
	private LabelManager lm;
	private final Map<String, Place> localOrInput = new HashMap<String, Place>();
	private final Map<String, Place> localPlaces = new HashMap<String, Place>();

	private final List<Page> pages = new ArrayList<Page>();

	private final Map<String, String> processGroups = new HashMap<String, String>();

	private final Map<String, Place> processPlaces = new HashMap<String, Place>();

	private final Set<String> processTypes = new HashSet<String>();

	private final Map<String, List<String>> productTypes = new HashMap<String, List<String>>();

	private final Map<String, Place> resourcePlaces = new HashMap<String, Place>();

	private final Map<String, Place> sharedPlaces = new HashMap<String, Place>();

	private final Map<String, Type> types = new HashMap<String, Type>();

	private final Map<String, Set<String>> variables = new HashMap<String, Set<String>>();

	/**
	 * 
	 */
	public PPCPNetChecker() {
		discover = Option.create("Discover", "discover", Boolean.class);
		check = Option.create("Check", "check", Boolean.class);
		processTypes.add("UNIT");
		addOption(discover);
		addSubscription(new Command(300, 1));
		addObserver(this);
	}

	/**
	 * @return
	 */
	public Iterable<Place> channelPlaces() {
		return channelPlaces.values();
	}

	/**
	 * 
	 */
	public void disable() {
		discovering = false;
		final Scraper s = channel.getExtension(Scraper.class);
		if (s != null) {
			for (final Page p : s) {
				for (final Place pp : p.places()) {
					removeLabel(pp);
				}
				for (final Transition tt : p.transitions()) {
					removeLabel(tt);
				}
				localOrInput.clear();
				localPlaces.clear();
				processGroups.clear();
				processPlaces.clear();
				sharedPlaces.clear();
				resourcePlaces.clear();
			}
		}
	}

	/**
	 * @return
	 */
	public boolean enable() {
		final boolean result = discovering;
		discovering = true;
		final Scraper s = channel.getExtension(Scraper.class);
		if (!added) {
			added = true;
			if (lm == null) {
				lm = new LabelManager(channel);
			}
			if (s != null) {
				s.addObserver(this);
			}
		}
		if (!result) {
			if (s != null) {
				s.notifyMe(this);
			}
		}
		return result;
	}

	/**
	 * @see org.cpntools.simulator.extensions.SubscriptionHandler#getIdentifier()
	 */
	@Override
	public int getIdentifier() {
		return PPCPNetChecker.ID;
	}

	/**
	 * @see org.cpntools.simulator.extensions.Extension#getName()
	 */
	@Override
	public String getName() {
		return "PP-CPN";
	}

	/**
	 * @param type
	 * @return
	 */
	public List<String> getProduct(final String type) {
		return productTypes.get(type);
	}

	/**
	 * @return
	 */
	public Map<String, Type> getTypes() {
		return types;
	}

	/**
	 * @see org.cpntools.simulator.extensions.Extension#handle(org.cpntools.accesscpn.engine.protocol.Packet)
	 */
	@Override
	public Packet handle(final Packet p, final Packet response) {
		p.reset();
		final int command = p.getInteger();
		if (command == 300) {
			final int subcommand = p.getInteger();
			if (subcommand == 1) {
				try {
					handleCheckDeclaration(p);
				} catch (final Exception e) {
					e.printStackTrace();
				}
			}
		}
		return null;
	}

	/**
	 * @param key
	 * @return
	 */
	public boolean isProcessPlace(final String key) {
		return processPlaces.containsKey(key);
	}

	/**
	 * @param type
	 * @return
	 */
	public boolean isProcessType(final String type) {
		return processTypes.contains(type);
	}

	/**
	 * @param type
	 * @param variable
	 * @return
	 */
	public boolean isVariable(final String type, final String variable) {
		if ("UNIT".equals(type)) {
			if ("()".equals(variable)) { return true; }
			if ("1`()".equals(variable)) { return true; }
		}
		if (!variables.containsKey(type)) { return false; }
		return variables.get(type).contains(variable);
	}

	/**
	 * @param tt
	 * @return
	 */
	public Iterable<Place> localPlacesOf(final Iterable<Transition> tt) {
		final List<Place> result = new ArrayList<Place>();
		for (final Place p : placesOf(tt)) {
			if (localPlaces.containsKey(p.getId())) {
				result.add(p);
			}
		}
		return result;
	}

	/**
	 * @return
	 */
	public Iterable<Set<Place>> partition() {
		final Map<String, Set<Place>> partitions = new HashMap<String, Set<Place>>();
		for (final Entry<String, Place> e : processPlaces.entrySet()) {
			final Set<Place> partition = new HashSet<Place>();
			partition.add(e.getValue());
			partitions.put(e.getKey(), partition);
		}
		boolean changed = true;
		while (changed) {
			changed = false;
			for (final String id : new ArrayList<String>(partitions.keySet())) {
				final Set<Place> activePartition = partitions.get(id);
				final Place activePlace = processPlaces.get(id);
				for (final Place n : neighbors(activePlace)) {
					final Set<Place> neighborPartition = partitions.get(n.getId());
					if (neighborPartition != activePartition) {
						changed = true;
						activePartition.addAll(neighborPartition);
						for (final Place neighbor : neighborPartition) {
							partitions.put(neighbor.getId(), activePartition);
						}
					}
				}
			}
		}
		return new HashSet<Set<Place>>(partitions.values());
	}

	/**
	 * @param tt
	 * @return
	 */
	public Iterable<Place> placesOf(final Iterable<Transition> tt) {
		final Map<String, Place> result = new HashMap<String, Place>();
		for (final Transition t : tt) {
			for (final Arc aa : t) {
				result.put(aa.getPlace().getId(), aa.getPlace());
			}
		}
		return result.values();
	}

	/**
	 * @param t
	 * @return
	 */
	public Pair<String, Place> prePlace(final Transition t) {
		for (final Arc a : t.in()) {
			if (processPlaces.containsKey(a.getPlace().getId())
			        && a.getPlace().getType().equals(processGroups.get(t.getId()))) { return Pair.createPair(
			        a.getInscription(), a.getPlace()); }
		}
		assert false;
		return null;
	}

	/**
	 * @return
	 */
	public Iterable<Place> resourcePlaces() {
		return resourcePlaces.values();
	}

	/**
	 * @see org.cpntools.simulator.extensions.AbstractExtension#setChannel(org.cpntools.simulator.extensions.Channel)
	 */
	@Override
	public void setChannel(final Channel c) {
		super.setChannel(c);
	}

	/**
	 * @return
	 */
	public Iterable<Place> sharedPlaces() {
		return sharedPlaces.values();
	}

	/**
	 * @param places
	 * @return
	 */
	public Iterable<Transition> transitionsOf(final Iterable<Place> places) {
		final Map<String, Transition> result = new HashMap<String, Transition>();
		for (final Place p : places) {
			for (final Arc a : p) {
				if (p.getType().equals(processGroups.get(a.getTransition().getId()))) {
					result.put(a.getTransition().getId(), a.getTransition());
				}
			}
		}
		return result.values();
	}

	/**
	 * @see java.util.Observer#update(java.util.Observable, java.lang.Object)
	 */
	@Override
	public void update(final Observable o, final Object arg) {
		try {
			if (discovering && o instanceof Scraper && arg instanceof Scraper.Event) {
				final Event e = (Event) arg;
				switch (e.getType()) {
				case ADDED:
					added(e.getElm());
					break;
				case CHANGED:
					changed(e.getElm());
					break;
				case REMOVED:
					removed(e.getElm());
					break;
				case ARC_CHANGED:
					arcChanged(e.getElm());
					break;
				}
			} else if (o == this && arg == discover) {
				if (getOption(discover)) {
					enable();
				} else {
					disable();
				}
			} else {
				System.out.println("Unhandled " + o + " - " + arg);
			}
		} catch (final Exception e) {
			e.printStackTrace();
		}
	}

	private void added(final Element elm) {
		if (elm instanceof Page) {
			pages.add((Page) elm);
		} else {
			changed(elm);
		}
	}

	private void addLabel(final Node elm, final String string) {
		try {
			lm.add(elm, this, string);
		} catch (final Exception e) { // Ignore
		}
	}

	private void addVariable(final String name, final String string) {
		Set<String> set = variables.get(name);
		if (set == null) {
			set = new HashSet<String>();
			variables.put(name, set);
		}
		set.add(string);
	}

	private void arcChanged(final Element elm) {
		if (elm instanceof Place) {
			arcChanged((Place) elm);
		} else if (elm instanceof Transition) {
			arcChanged((Transition) elm);
		}
	}

	private void arcChanged(final Place elm) {
		if (processPlaces.containsKey(elm.getId())) {
			boolean entry = false;
			boolean exit = false;
			if (!elm.isSocket() && !elm.out().iterator().hasNext()) {
				exit = true;
			}
			if (!elm.isSocket() && !elm.in().iterator().hasNext()) {
				entry = true;
			}
			if (!"".equals(elm.getInitMark())) {
				entry = true;
			}
			if (entry && exit) {
				addLabel(elm, "Entry/Exit");
			} else if (entry) {
				addLabel(elm, "Entry");
			} else if (exit) {
				addLabel(elm, "Exit");
			} else {
				removeLabel(elm);
			}
			return;
		}
		final Set<String> inTransitions = new HashSet<String>();
		final Set<String> outTransitions = new HashSet<String>();
		final Set<String> inTypes = new HashSet<String>();
		final Set<String> outTypes = new HashSet<String>();
		int in = 0, out = 0;
		for (final Arc a : elm.in()) {
			inTransitions.add(a.getTransition().getId());
			inTypes.add(processGroups.get(a.getTransition().getId()));
			in++;
		}
		for (final Arc a : elm.out()) {
			outTransitions.add(a.getTransition().getId());
			outTypes.add(processGroups.get(a.getTransition().getId()));
			out++;
		}
		if (localOrInput.containsKey(elm.getId())) {
			if (inTypes.size() == 1 && outTypes.size() == 1 && in == out && in == inTransitions.size()
			        && productTypes.get(elm.getType()).contains(inTypes.iterator().next())
			        && inTransitions.equals(outTransitions)) {
				local(elm);
			} else if (outTypes.size() == 1 && productTypes.get(elm.getType()).contains(outTypes.iterator().next())) {
				buffer(elm);
			} else {
				if (inTypes.size() == 1 && outTypes.size() == 1 && inTypes.equals(outTypes) && inTypes.contains("UNIT")) {
					local(elm);
				} else {
					error(elm);
				}
			}
		} else if (inTypes.size() == 1 && outTypes.size() == 1 && in == out && in == inTransitions.size()
		        && inTransitions.equals(outTransitions) && inTypes.contains("UNIT")) {
			local(elm); // UNIT
		} else if (outTypes.size() == 1 && outTypes.contains("UNIT")
		        && Collections.disjoint(inTransitions, outTransitions)) {
			if (types.get(elm.getType()) instanceof Unit && !"".equals(elm.getInitMark())) {
				resource(elm);
			} else {
				buffer(elm); // UNIT
			}
		} else {
			// A buffer out is an input somewhere else and not handled here
			if (types.get(elm.getType()) instanceof Unit && !"".equals(elm.getInitMark())) {
				resource(elm);
			} else if (in == out && in == inTransitions.size() && inTransitions.equals(outTransitions)
			        && !inTypes.contains(null)) {
				shared(elm);
			} else {
				error(elm);
			}
		}
	}

	private void arcChanged(final Transition elm) {
		final Set<String> inProcess = new HashSet<String>();
		final Set<String> outProcess = new HashSet<String>();
		new HashSet<String>();
		int in = 0, out = 0;
		for (final Arc a : elm.in()) {
			assert a.getPlace() != null;
			if (processPlaces.containsKey(a.getPlace().getId())) {
				inProcess.add(a.getPlace().getType());
				in++;
			}
		}
		for (final Arc a : elm.out()) {
			assert a.getPlace() != null;
			if (processPlaces.containsKey(a.getPlace().getId())) {
				outProcess.add(a.getPlace().getType());
				out++;
			}
		}
		if (in == 1 && out > 0 && inProcess.size() == 1 && outProcess.size() >= 1 && outProcess.containsAll(inProcess)) {
			final String type = inProcess.iterator().next();
			final String oldType = processGroups.put(elm.getId(), type);
			if (oldType == null) {
				typeAdded(elm);
			} else if (!oldType.equals(type)) {
				typeChanged(elm);
			}
			addLabel(elm, type);
		} else {
			if (processGroups.remove(elm.getId()) != null) {
				typeRemoved(elm);
			}
			removeLabel(elm);
		}
	}

	private void buffer(final Place elm) {
		addLabel(elm, "Buffer");
		if (channelPlaces.put(elm.getId(), elm) == null) {
			if (localPlaces.remove(elm.getId()) != null) {
				localRemoved(elm);
			}
			if (sharedPlaces.remove(elm.getId()) != null) {
				sharedRemoved(elm);
			}
			if (resourcePlaces.remove(elm.getId()) != null) {
				resourceRemoved(elm);
			}
			channelAdded(elm);
		}
	}

	private void changed(final Element elm) {
		if (elm instanceof Place) {
			changed((Place) elm);
		} else if (elm instanceof Transition) {
			changed((Transition) elm);
		}
	}

	private void changed(final Place elm) {
		if (processTypes.contains(elm.getType())) {
			processPlaces.put(elm.getId(), elm);
			arcChanged(elm);
			return;
		} else {
			processPlaces.remove(elm.getId());
		}
		if (productTypes.containsKey(elm.getType()) && hasProcessType(productTypes.get(elm.getType()))
		        && productTypes.get(elm.getType()).size() <= 2) {
			if (localOrInput.put(elm.getId(), elm) == null) {
// addLabel(elm, "Local");
			}
			return;
		} else {
			localOrInput.remove(elm.getId());
		}
// addLabel(elm, "Shared");
	}

	private void changed(final Transition elm) {
		// TODO needed?
	}

	private void channelAdded(final Place elm) {
		// TODO Auto-generated method stub

	}

	private void channelRemoved(final Place elm) {
		// TODO Auto-generated method stub

	}

	private void consume(final Packet p) {
		final int count = p.getInteger();
		for (int i = 0; i < count; i++) {
			p.getString();
		}
	}

	private void error(final Place elm) {
		addLabel(elm, "Error");
		if (channelPlaces.remove(elm.getId()) != null) {
			channelRemoved(elm);
		}
		if (localPlaces.remove(elm.getId()) != null) {
			localRemoved(elm);
		}
		if (sharedPlaces.remove(elm.getId()) != null) {
			sharedRemoved(elm);
		}
		if (resourcePlaces.remove(elm.getId()) != null) {
			resourceRemoved(elm);
		}
	}

	private void handleCheckDeclaration(final Packet p) {
		p.reset();
		p.getInteger(); // command
		p.getInteger(); // subcmd
		p.getString();
		final int type = p.getInteger();
		List<String> components = null;
		Type t = null;
		switch (type) {
		case 1: // unit
			p.getString();
			t = new Unit();
			break;
		case 2: // bool
			t = new Bool();
			p.getString();
			p.getString();
			break;
		case 3: // int
			t = new Int();
			p.getString();
			p.getString();
			break;
		case 4: // real
			p.getString();
			p.getString();
			break;
		case 5: // string
			t = new org.cpntools.simulator.extensions.scraper.types.String();
			p.getString();
			p.getString();
			p.getString();
			p.getString();
			break;
		case 7: // index
		case 8: // list
			p.getString();
			p.getString();
			p.getString();
			break;
		case 6: { // enum
			final int enums = p.getInteger();
			for (int i = 0; i < enums; i++) {
				p.getString();
			}
		}
			break;
		case 9: { // product
			final int comps = p.getInteger();
			components = new ArrayList<String>(comps);
			for (int i = 0; i < comps; i++) {
				components.add(p.getString());
			}
		}
			break;
		case 10: // record
		case 11: { // union
			final int comps = p.getInteger();
			for (int i = 0; i < comps; i++) {
				p.getString();
				p.getString();
			}
		}
			break;
		case 12: { // fn subset
			p.getString();
			p.getString();
		}
			break;
		case 13: { // list subset
			final int elems = p.getInteger();
			p.getString();
			for (int i = 0; i < elems; i++) {
				p.getString();
			}
		}
			break;
		case 14: // time
		case 22: // alias
			break;
		case 15: // duplicate (unused)
			final String alias = p.getString();
			t = types.get(alias);
			if (t instanceof Other) {
				t = null;
			}
			break;
		case 20: { // Variable
			final String name = p.getString();
			final int vars = p.getInteger();
			for (int i = 0; i < vars; i++) {
				addVariable(name, p.getString());
			}
		}
		//$FALL-THROUGH$
		default:
			return;
		}
		if (type == 22) {
			System.out.println(p);
			assert false;
		} else {
			final String name = p.getString();
			if (t == null) {
				t = new Other(name);
			}
			types.put(name, t);
			final int vars = p.getInteger();
			for (int i = 0; i < vars; i++) {
				addVariable(name, p.getString());
			}
			consume(p); // msvars
			consume(p); // aliases
			final Set<String> declares = new HashSet<String>();
			final int declare = p.getInteger();
			for (int i = 0; i < declare; i++) {
				declares.add(p.getString());
			}
			if (declares.contains(PPCPNetChecker.PID)) {
				if (processTypes.add(name)) {
					processTypeChanged(name);
				} else {
					processTypeAdded(name);
				}
			} else {
				if (!"UNIT".equals(name)) {
					if (processTypes.remove(name)) {
						processTypeRemoved(name);
					}
				}
			}
			if (productTypes.remove(name) != null && type != 9) {
				productTypeRemoved(name);
			}
			if (type == 9) {
				if (productTypes.put(name, components) == null) {
					productTypeAdded(name);
				} else {
					productTypeChanged(name);
				}
			}
		}
	}

	private boolean hasProcessType(final List<String> list) {
		for (final String component : list) {
			if (processTypes.contains(component)) { return true; }
		}
		return false;
	}

	private void local(final Place elm) {
		addLabel(elm, "Local");
		if (localPlaces.put(elm.getId(), elm) == null) {
			if (channelPlaces.remove(elm.getId()) != null) {
				channelRemoved(elm);
			}
			if (sharedPlaces.remove(elm.getId()) != null) {
				sharedRemoved(elm);
			}
			if (resourcePlaces.remove(elm.getId()) != null) {
				resourceRemoved(elm);
			}
			localAdded(elm);
		}
	}

	private void localAdded(final Place elm) {
		// TODO Auto-generated method stub

	}

	private void localRemoved(final Place elm) {
		// TODO Auto-generated method stub

	}

	private Iterable<Place> neighbors(final Place p) {
		final Map<String, Place> neighbors = new HashMap<String, Place>();
		for (final Place pp : placesOf(transitionsOf(Collections.singleton(p)))) {
			if (p.getType().equals(pp.getType())) {
				neighbors.put(pp.getId(), pp);
			}
		}
		neighbors.remove(p.getId());
		return neighbors.values();
	}

	private void processTypeAdded(final String name) {
		refreshPlaces(name);
	}

	private void processTypeChanged(final String name) {
		refreshPlaces(name);
	}

	private void processTypeRemoved(final String name) {
		refreshPlaces(name);
	}

	private void productTypeAdded(final String name) {
		refreshPlaces(name);
	}

	private void productTypeChanged(final String name) {
		refreshPlaces(name);
	}

	private void productTypeRemoved(final String name) {
		refreshPlaces(name);
	}

	private void refreshPlaces(final String type) {
		final Map<String, Transition> changed = new HashMap<String, Transition>();
		for (final Page p : pages) {
			for (final Place pp : p.places()) {
				if (pp.getType().equals(type)) {
					changed(pp);
					for (final Arc a : pp) {
						changed.put(a.getTransition().getId(), a.getTransition());
					}
				}
			}
		}
		for (final Transition t : changed.values()) {
			arcChanged(t);
		}
	}

	private void refreshPlaces(final Transition elm) {
		final Map<String, Place> places = new HashMap<String, Place>();
		for (final Arc a : elm) {
			places.put(a.getPlace().getId(), a.getPlace());
		}
		for (final Place p : places.values()) {
			arcChanged(p);
		}
	}

	private void removed(final Element elm) {
		if (elm instanceof Page) {
			pages.remove(elm);
		}
	}

	private void removeLabel(final Node elm) {
		try {
			lm.delete(elm, this);
		} catch (final Exception e) { // Ignore
		}
	}

	private void resource(final Place elm) {
		addLabel(elm, "Resource");
		if (resourcePlaces.put(elm.getId(), elm) == null) {
			if (sharedPlaces.remove(elm.getId()) != null) {
				sharedRemoved(elm);
			}
			if (channelPlaces.remove(elm.getId()) != null) {
				channelRemoved(elm);
			}
			if (localPlaces.remove(elm.getId()) != null) {
				localRemoved(elm);
			}
			sharedAdded(elm);
		}
	}

	private void resourceRemoved(final Place elm) {
		// TODO Auto-generated method stub

	}

	private void shared(final Place elm) {
		addLabel(elm, "Shared");
		if (sharedPlaces.put(elm.getId(), elm) == null) {
			if (resourcePlaces.remove(elm.getId()) != null) {
				resourceRemoved(elm);
			}
			if (channelPlaces.remove(elm.getId()) != null) {
				channelRemoved(elm);
			}
			if (localPlaces.remove(elm.getId()) != null) {
				localRemoved(elm);
			}
			sharedAdded(elm);
		}
	}

	private void sharedAdded(final Place elm) {
		// TODO Auto-generated method stub

	}

	private void sharedRemoved(final Place elm) {
		// TODO Auto-generated method stub

	}

	private void typeAdded(final Transition elm) {
		refreshPlaces(elm);
	}

	private void typeChanged(final Transition elm) {
		refreshPlaces(elm);
	}

	private void typeRemoved(final Transition elm) {
		refreshPlaces(elm);
	}
}
