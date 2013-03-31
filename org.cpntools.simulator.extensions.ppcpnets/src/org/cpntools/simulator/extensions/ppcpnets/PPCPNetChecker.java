package org.cpntools.simulator.extensions.ppcpnets;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
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
import org.cpntools.simulator.extensions.scraper.Place;
import org.cpntools.simulator.extensions.scraper.Scraper;
import org.cpntools.simulator.extensions.scraper.Scraper.Event;
import org.cpntools.simulator.extensions.scraper.Transition;
import org.cpntools.simulator.extensions.tools.labels.LabelManager;

/**
 * @author michael
 */
public class PPCPNetChecker extends AbstractExtension implements Observer {
	/**
	 * 
	 */
	public static final int ID = 10007;
	private static final String PID = "pid";
	private final Map<String, Place> channelPlaces = new HashMap<String, Place>();
	private final Option<Boolean> check;

	private final Option<Boolean> discover;
	private LabelManager lm;
	private final Map<String, Place> localOrInput = new HashMap<String, Place>();
	private final Map<String, Place> localPlaces = new HashMap<String, Place>();
	private final Map<String, String> processGroups = new HashMap<String, String>();

	private final Map<String, Place> processPlaces = new HashMap<String, Place>();

	private final Set<String> processTypes = new HashSet<String>();

	private final Map<String, List<String>> productTypes = new HashMap<String, List<String>>();

	private final Map<String, Place> sharedPlaces = new HashMap<String, Place>();

	private final Map<String, Set<String>> variables = new HashMap<String, Set<String>>();

	/**
	 * 
	 */
	public PPCPNetChecker() {
		discover = Option.create("Discover", "discover", Boolean.class);
		check = Option.create("Check", "check", Boolean.class);
		processTypes.add("UNIT");
		addOption(discover, check);
		addSubscription(new Command(300, 1));
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
	 * @see org.cpntools.simulator.extensions.AbstractExtension#setChannel(org.cpntools.simulator.extensions.Channel)
	 */
	@Override
	public void setChannel(final Channel c) {
		super.setChannel(c);
		final Scraper s = c.getExtension(Scraper.class);
		if (s != null) {
			s.addObserver(this);
		}
		lm = new LabelManager(channel);
	}

	/**
	 * @see java.util.Observer#update(java.util.Observable, java.lang.Object)
	 */
	@Override
	public void update(final Observable o, final Object arg) {
		try {
			if (o instanceof Scraper && arg instanceof Scraper.Event) {
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
			} else {
				System.out.println("Unhandled " + o + " - " + arg);
			}
		} catch (final Exception e) {
			e.printStackTrace();
		}
	}

	private void added(final Element elm) {
		changed(elm);
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
		if (processPlaces.containsKey(elm.getId())) { return; }
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
				addLabel(elm, "Local");
				if (localPlaces.put(elm.getId(), elm) == null) {
					if (channelPlaces.remove(elm.getId()) != null) {
						channelRemoved(elm);
					}
					if (sharedPlaces.remove(elm.getId()) != null) {
						sharedRemoved(elm);
					}
					localAdded(elm);
				}
			} else {
				addLabel(elm, "Buffer/unknown");
			}
		} else if (inTypes.size() == 1 && outTypes.size() == 1 && inTypes.equals(outTypes) && inTypes.contains("UNIT")) {
			addLabel(elm, "UNIT local/buffer");
		} else {
			if (in == out && in == inTransitions.size() && inTransitions.equals(outTransitions)
			        && !inTypes.contains(null)) {
				addLabel(elm, "Shared");
				if (sharedPlaces.put(elm.getId(), elm) == null) {
					if (channelPlaces.remove(elm.getId()) != null) {
						channelRemoved(elm);
					}
					if (localPlaces.remove(elm.getId()) != null) {
						localRemoved(elm);
					}
					sharedAdded(elm);
				}
			} else {
				if (channelPlaces.remove(elm.getId()) != null) {
					channelRemoved(elm);
				}
				if (localPlaces.remove(elm.getId()) != null) {
					localRemoved(elm);
				}
				if (sharedPlaces.remove(elm.getId()) != null) {
					sharedRemoved(elm);
				}
				addLabel(elm, "Error");
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
			removeLabel(elm);
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
			if ("".equals(elm.getInitMark())) {
				removeLabel(elm);
			} else {
				addLabel(elm, "Entry");
			}
			return;
		} else {
			processPlaces.remove(elm.getId());
		}
		if (productTypes.containsKey(elm.getType()) && hasProcessType(productTypes.get(elm.getType()))) {
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

	private void channelRemoved(final Place elm) {
		// TODO Auto-generated method stub

	}

	private void consume(final Packet p) {
		final int count = p.getInteger();
		for (int i = 0; i < count; i++) {
			p.getString();
		}
	}

	private void handleCheckDeclaration(final Packet p) {
		p.reset();
		p.getInteger(); // command
		p.getInteger(); // subcmd
		p.getString();
		final int type = p.getInteger();
		List<String> components = null;
		switch (type) {
		case 1: // unit
			p.getString();
			break;
		case 2: // bool
		case 3: // int
		case 4: // real
			p.getString();
			p.getString();
			break;
		case 5: // string
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
		case 15: // duplicate (unused)
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
		if (type == 22 || type == 15) { // alias
			assert type == 22;
		} else {
			final String name = p.getString();
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

	private void localAdded(final Place elm) {
		// TODO Auto-generated method stub

	}

	private void localRemoved(final Place elm) {
		// TODO Auto-generated method stub

	}

	private void processTypeAdded(final String name) {
		// TODO Auto-generated method stub

	}

	private void processTypeChanged(final String name) {
		// TODO Auto-generated method stub

	}

	private void processTypeRemoved(final String name) {
		// TODO Auto-generated method stub

	}

	private void productTypeAdded(final String name) {
		// TODO Auto-generated method stub

	}

	private void productTypeChanged(final String name) {
		// TODO Auto-generated method stub

	}

	private void productTypeRemoved(final String name) {
		// TODO Auto-generated method stub

	}

	private void removed(final Element elm) {
		// TODO only needed for check
	}

	private void removeLabel(final Node elm) {
		try {
			lm.delete(elm, this);
		} catch (final Exception e) { // Ignore
		}
	}

	private void sharedAdded(final Place elm) {
		// TODO Auto-generated method stub

	}

	private void sharedRemoved(final Place elm) {
		// TODO Auto-generated method stub

	}

	private void typeAdded(final Transition elm) {
		// TODO Auto-generated method stub

	}

	private void typeChanged(final Transition elm) {
		// TODO Auto-generated method stub

	}
}
