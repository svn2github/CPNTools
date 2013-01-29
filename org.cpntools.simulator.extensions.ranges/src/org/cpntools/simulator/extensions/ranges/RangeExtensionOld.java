package org.cpntools.simulator.extensions.ranges;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.cpntools.accesscpn.engine.protocol.Packet;
import org.cpntools.simulator.extensions.AbstractExtension;
import org.cpntools.simulator.extensions.Command;
import org.cpntools.simulator.extensions.scraper.Arc;
import org.cpntools.simulator.extensions.scraper.Place;
import org.cpntools.simulator.extensions.scraper.Transition;

import dk.klafbang.tools.Pair;

/**
 * @author michael
 * @param <T>
 */
public class RangeExtensionOld extends AbstractExtension {
	public static final int ID = 10005;

	public RangeExtensionOld() {
		addSubscription(new Command(400, 2, true), // Syntax check page
		        new Command(500, 3, true), // Generate instances
		        new Command(500, 12, true), // Execute transition
		        new Command(500, 13, true), // Check transition for enabledness
		        new Command(500, 14, true), // Checked enabledness without scheduler
		        new Command(500, 15, true), // Manual binding
		        new Command(500, 35, true), // Check enabling of list of transitions
		        new Command(500, 36, true) // Check enabling of transitions without scheduler
		);
	}

	@Override
	public Packet handle(final Packet p, final Packet response) {
		p.reset();
		final int command = p.getInteger();
		if (command == 400) {
			final int subcommand = p.getInteger();
			switch (subcommand) {
			case 2:
				return handleSyntaxCheck(p, response);
			}
		}
		if (command == 500) {
			final int subcommand = p.getInteger();
			switch (subcommand) {
			case 3:
				if (response == null) { return handleGenerateInstances(p); }
			case 12:
				if (response == null) { return execute(p); }
			case 13:
			case 14:
				return enabled(p, response);
// case 15:
			case 35:
			case 36:
				return multipleEnabled(p, response);
			}
		}
		return null;
	}

	@Override
	public Packet prefilter(final Packet p) {
		return handle(p, null);
	}

	private Packet handleGenerateInstances(final Packet packet) {
		final Packet filtered = new Packet(packet.getOpcode(), 500);
		packet.reset();
		packet.getInteger(); // cmd
		filtered.addInteger(packet.getInteger()); // subcmd
		filtered.addBoolean(packet.getBoolean()); // update-only
		final int placeno = packet.getInteger();
		filtered.addInteger(placeno);
		for (int i = 0; i < placeno; i++) { // changed-new-places
			filtered.addString(packet.getString()); // id
		}
		final int refno = packet.getInteger();
		filtered.addInteger(refno);
		for (int i = 0; i < refno; i++) { // changed-new-refs
			filtered.addString(packet.getString()); // id
		}
		int count = 0;
		for (int i = packet.getInteger(); i > 0; i--) { // changed-new-trans
			final String id = packet.getString();
			final List<String> unfolded = unfold.get(id);
			if (unfolded != null) {
				for (final String uid : unfolded) {
					filtered.addString(uid);
					count++;
				}
			} else {
				filtered.addString(id);
				count++;
			}
		}
		filtered.addInteger(count);
		return filtered;
	}

	final private Map<String, List<String>> unfold = new HashMap<String, List<String>>();
	final private Map<String, String> refold = new HashMap<String, String>();

	private Packet handleSyntaxCheck(final Packet packet, final Packet response) {
		if (response == null) {
			return filter(packet);
		} else {
			return fold(packet, response);
		}
	}

	private Packet fold(final Packet packet, final Packet response) {
		final Packet filtered = new Packet(7, 1);
		response.reset();
		response.getInteger(); // TERMTAG

		// Errors
		final int errors = response.getInteger();
		if (errors < 0) { return null; }
		String error = "";
		Set<String> refolding = Collections.emptySet();
		int count = 0;
		for (int i = 0; i < errors; i++) {
			final String id = response.getString();
			final String remap = refold.get(id);
			final String newError = response.getString();
			if (remap != null) {
				if (refolding.isEmpty()) {
					refolding = new HashSet<String>(unfold.get(remap));
				}
				if (refolding.remove(id)) {
					if (!newError.isEmpty() && error.isEmpty()) {
						error = newError;
					}
				} else {
					assert false;
				}
				if (refolding.isEmpty()) {
					filtered.addString(remap);
					filtered.addString(error);
					error = "";
					count++;
				}
			} else {
				filtered.addString(id);
				filtered.addString(newError);
				count++;
			}
		}
		filtered.addInteger(count);

		// Dependencies
		assert refolding.isEmpty();
		refolding = Collections.emptySet();
		count = 0;
		final List<Pair<String, Set<String>>> deps = new ArrayList<Pair<String, Set<String>>>();
		for (int i = response.getInteger(); i > 0; i--) {
			final String id = response.getString();
			final String remap = refold.get(id);
			final Set<String> dependencies = new HashSet<String>();
			for (int j = response.getInteger(); j > 0; j--) {
				dependencies.add(response.getString());
			}
			if (remap != null) {
				if (refolding.isEmpty()) {
					refolding = new HashSet<String>(unfold.get(remap));
				}
				if (!refolding.remove(id)) {
					assert false;
				}
				if (refolding.isEmpty()) {
					deps.add(Pair.createPair(remap, (Set<String>) new TreeSet<String>(dependencies)));
					dependencies.clear();
				}
			} else {
				deps.add(Pair.createPair(id, (Set<String>) new TreeSet<String>(dependencies)));
			}
		}
		filtered.addInteger(deps.size());
		for (final Pair<String, Set<String>> dep : deps) {
			filtered.addInteger(dep.getSecond().size());
			filtered.addString(dep.getFirst());
			for (final String d : dep.getSecond()) {
				filtered.addString(d);
			}
		}

		// ASTs
		// Errors
		final int asts = response.getInteger();
		assert refolding.isEmpty();
		refolding = Collections.emptySet();
		count = 0;
		for (int i = 0; i < asts; i++) {
			final String id = response.getString();
			final String remap = refold.get(id);
			final String ast = response.getString();
			if (remap != null) {
				if (refolding.isEmpty()) {
					refolding = new HashSet<String>(unfold.get(remap));
				}
				if (!refolding.remove(id)) {
					assert false;
				}
				if (refolding.isEmpty()) {
					filtered.addString(remap);
					filtered.addString(ast);
					count++;
				}
			} else {
				filtered.addString(id);
				filtered.addString(ast);
				count++;
			}
		}
		filtered.addInteger(count);

		return filtered;
	}

	private Packet filter(final Packet packet) {
		final Packet filtered = new Packet(packet.getOpcode(), 400);
		packet.reset();
		packet.getInteger(); // cmd
		filtered.addInteger(packet.getInteger()); // subcmd
		final String id = packet.getString();
		filtered.addString(id);
		final String name = packet.getString();
		filtered.addString(name);
		filtered.addInteger(packet.getInteger()); // Prime multiplicity
		filtered.addBoolean(packet.getBoolean()); // thisPage.isIncludedInSim
		int placeno = packet.getInteger();
		filtered.addInteger(placeno);
		for (int i = placeno; i > 0; i--) {
			filtered.addString(packet.getString());
		}

		int transno = packet.getInteger();
		final List<String> keepers = new ArrayList<String>();
		for (int i = transno; i > 0; i--) {
			keepers.add(packet.getString());
		}
		final List<String> filteredKeepers = keep(keepers);
		filtered.addInteger(filteredKeepers.size());
		for (final String tid : filteredKeepers) {
			filtered.addString(tid);
		}

		placeno = packet.getInteger();
		filtered.addInteger(placeno);
		for (int i = placeno; i > 0; i--) {
			filtered.addString(packet.getString()); // id
			filtered.addString(packet.getString()); // name
			filtered.addString(packet.getString()); // colset
			filtered.addString(packet.getString()); // initmark
		}
		// Ignore places

		placeno = packet.getInteger();
		filtered.addInteger(placeno);
		for (int i = placeno; i > 0; i--) {
			filtered.addString(packet.getString()); // id
			filtered.addString(packet.getString()); // fusid
			filtered.addString(packet.getString()); // name
			filtered.addString(packet.getString()); // colset
			filtered.addString(packet.getString()); // initmark
		}
		// Ignore fusion places

		transno = packet.getInteger();
		filtered.addInteger(transno);
		for (int i = transno; i > 0; i--) {
			filtered.addString(packet.getString()); // id
			filtered.addString(packet.getString()); // name
			filtered.addString(packet.getString()); // subpageid
			final int portno = packet.getInteger();
			filtered.addInteger(portno);
			for (int j = portno; j > 0; j--) {
				filtered.addString(packet.getString()); // portid
				filtered.addString(packet.getString()); // socketid
			}
		}
		// Ignore subst transitions

		List<Transition> ts = new ArrayList<Transition>();
		for (int i = packet.getInteger(); i > 0; i--) {
			final String tid = packet.getString();
			final String tname = packet.getString();
			final String tguard = packet.getString();
			final String ttime = packet.getString();
			final String tcode = packet.getString();
			final String tchannel = packet.getString();
			final String tpriority = packet.getString();
			final boolean tcontrollable = packet.getBoolean();
			final Transition t = new Transition(tid, tname, null, tguard, tpriority, ttime, tcode, tchannel,
			        tcontrollable);
			ts.add(t);

			for (int j = packet.getInteger(); j > 0; j--) { // input
				final String aid = packet.getString();
				final String placeId = packet.getString();
				final String inscription = packet.getString();
				t.addArc(new Arc(aid, inscription, Arc.Type.INPUT, null, t, new Place(placeId, null, null)));
			}
			for (int j = packet.getInteger(); j > 0; j--) { // output
				final String aid = packet.getString();
				final String placeId = packet.getString();
				final String inscription = packet.getString();
				t.addArc(new Arc(aid, inscription, Arc.Type.OUTPUT, null, t, new Place(placeId, null, null)));
			}
			for (int j = packet.getInteger(); j > 0; j--) { // inoutput
				final String aid = packet.getString();
				final String placeId = packet.getString();
				final String inscription = packet.getString();
				t.addArc(new Arc(aid, inscription, Arc.Type.BOTHDIR, null, t, new Place(placeId, null, null)));
			}
			for (int j = packet.getInteger(); j > 0; j--) { // inhibitor
				final String aid = packet.getString();
				final String placeId = packet.getString();
				final String inscription = packet.getString();
				t.addArc(new Arc(aid, inscription, Arc.Type.INHIBITOR, null, t, new Place(placeId, null, null)));
			}
			for (int j = packet.getInteger(); j > 0; j--) { // reset
				final String aid = packet.getString();
				final String placeId = packet.getString();
				final String inscription = packet.getString();
				t.addArc(new Arc(aid, inscription, Arc.Type.RESET, null, t, new Place(placeId, null, null)));
			}
		}
		ts = filter(ts);
		filtered.addInteger(ts.size());
		for (final Transition t : ts) {
			filtered.addString(t.getId());
			filtered.addString(t.getName());
			filtered.addString(t.getGuard());
			filtered.addString(t.getTime());
			filtered.addString(t.getCode());
			filtered.addString(t.getChannel());
			filtered.addString(t.getPriority());
			filtered.addBoolean(t.isControllable());

			for (final Arc.Type type : Arc.Type.values()) {
				int count = 0;
				for (final Arc a : t) {
					if (a.getType() == type) {
						count++;
						filtered.addString(a.getId());
						filtered.addString(a.getPlace().getId());
						filtered.addString(a.getInscription());
					}
				}
				filtered.addInteger(count);
			}
		}

		return filtered;
	}

	private static final Pattern constantRange = Pattern.compile("@ *\\[ *([0-9]+)* *, *([0-9]+) *\\]");
	private static final Pattern range = Pattern.compile("@ *(\\[.*,.*\\])");
	private static final Pattern result = Pattern.compile("val a = ([0-9]*) : int.*val b = ([0-9]*) : int",
	        Pattern.DOTALL);

	private List<Transition> filter(final List<Transition> ts) {
		final List<Transition> result = new ArrayList<Transition>();
		loop: for (final Transition t : ts) {
			final String time = t.getTime().trim();
			final Matcher m = range.matcher(time);
			final Matcher n = constantRange.matcher(time);
			if (m.matches() && false || n.matches()) {
				final List<String> oldMaps = unfold.remove(t.getId());
				if (oldMaps != null) {
					for (final String oldMap : oldMaps) {
						refold.remove(oldMap);
					}
				}
				final List<String> maps = new ArrayList<String>();
				int a, b;
				if (n.matches()) {
					a = Integer.parseInt(n.group(1));
					b = Integer.parseInt(n.group(2));
				} else {
					try {
						final String value = channel.evaluate("val [a, b]: int list = " + m.group(1)).trim();
						System.out.println(value);
						final Matcher r = RangeExtensionOld.result.matcher(value);
						if (!r.matches()) { throw new Exception("Bad result"); }
						a = Integer.parseInt(r.group(1));
						b = Integer.parseInt(r.group(2));
					} catch (final Exception _) {
						_.printStackTrace();
						t.setTime("@+(List.foldl (fn (a, b) => a + b) 0 (" + m.group(1) + "))");
						result.add(t);
						continue loop;
					}
				}
				for (int i = a; i <= b; i++) {
					final Transition clone = t.clone();
					clone.setName(t.getName() + "'" + i);
					final String newId = t.getId() + "_" + i;
					clone.setId(newId);
					maps.add(newId);
					refold.put(newId, t.getId());
					clone.setTime("@+" + i);
					result.add(clone);
				}
				unfold.put(t.getId(), maps);
			} else {
				result.add(t);
			}
		}
		return result;
	}

	private List<String> keep(final List<String> keepers) {
		new HashSet<String>(keepers);
		final List<String> result = new ArrayList<String>();
		final Set<String> remove = new HashSet<String>(unfold.keySet());
		remove.removeAll(keepers);
		for (final String r : remove) {
			refold.remove(unfold.remove(r));
		}
		for (final String keeper : keepers) {
			final List<String> unmap = unfold.get(keeper);
			if (unmap == null) {
				result.add(keeper);
			} else {
				result.addAll(unmap);
			}
		}
		return result;
	}

	private static final Random randomGenerator = new Random();

	private Packet execute(final Packet p) {
		p.reset();
		p.getInteger();
		p.getInteger(); // Ingore cmd and subcmd
		final int instance = p.getInteger();
		final String id = p.getString();
		final List<String> unfolded = unfold.get(id);
		if (unfolded == null) { return null; }
		final Packet filtered = new Packet(p.getOpcode(), 500);
		filtered.addInteger(12);
		filtered.addInteger(instance);
		filtered.addString(unfolded.get(randomGenerator.nextInt(unfolded.size())));
		return filtered;
// p.reset();
// Object task = tasks.get(p.getString());
// if (task == null) {
// task = Automaton.OTHERWISE;
// }
// for (final String pageId : new ArrayList<String>(automata.keySet())) {
// final Automaton a = automata.get(pageId);
// final int state = states.get(pageId);
// int next = a.next(state, task);
// if (next < 0) {
// next = a.next(state, Automaton.OTHERWISE);
// }
// states.put(pageId, next);
// }
	}

	private Packet multipleEnabled(final Packet p, final Packet response) {
		p.reset();
		if (response == null) {
			final Packet filtered = new Packet(p.getOpcode(), 500);
			p.getInteger(); // command
			filtered.addInteger(p.getInteger()); // subcmd
			final List<Pair<String, Integer>> instances = new ArrayList<Pair<String, Integer>>();
			for (int i = p.getInteger(); i > 0; i--) { // tis
				final int instance = p.getInteger();
				final String id = p.getString();
				final List<String> unfoldings = unfold.get(id);
				if (unfoldings != null) {
					for (final String unfolding : unfoldings) {
						instances.add(Pair.createPair(unfolding, instance));
					}
				} else {
					instances.add(Pair.createPair(id, instance));
				}
			}
			filtered.addInteger(instances.size());
			for (final Pair<String, Integer> instance : instances) {
				filtered.addInteger(instance.getSecond());
				filtered.addString(instance.getFirst());
			}
			return filtered;
		} else {
			// This assumes all folded transitions are sent together
			response.reset();
			if (response.getInteger() != 1) { return response; }
			final Packet result = new Packet(7, 1);
			p.getInteger();
			p.getInteger(); // Skip command and subcmd
			int count = 0;
			boolean enabled = false;
			String error = "";
			Set<String> refolding = Collections.emptySet();
			for (int i = p.getInteger(); i > 0; i--) { // tis
				final String id = p.getString();
				final String remap = refold.get(id);
				final boolean e = response.getBoolean();
				final String newError = response.getString();
				if (remap != null) {
					if (refolding.isEmpty()) {
						refolding = new HashSet<String>(unfold.get(remap));
					}
					if (refolding.remove(id)) {
						enabled |= e;
						if (!newError.isEmpty() && error.isEmpty()) {
							error = newError;
						}
					} else {
						assert false;
					}
					if (refolding.isEmpty()) {
						result.addBoolean(enabled);
						result.addString(error);
						enabled = false;
						error = "";
						count++;
					}
				} else {
					result.addBoolean(e);
					result.addString(newError);
					count++;
				}
			}
			if (count > 0) {
				result.addInteger(count);
			} else {
				result.addInteger(1);
				result.addBoolean(false);
				result.addString("");
			}
			return result;
		}
	}

	private Packet enabled(final Packet p, final Packet response) {
		if (response != null) { return null; }
		p.reset();
		final String id = p.getString();
		final List<String> unfoldings = unfold.get(id);
		if (unfoldings == null) { return null; }
		final Packet replacement = new Packet(p.getOpcode(), 500);
		p.getInteger(); // command
		replacement.addInteger(p.getInteger() + 22); // subcmd
		final int instance = p.getInteger();
		replacement.addInteger(unfoldings.size());
		for (final String unfolding : unfoldings) {
			p.addString(unfolding);
			p.addInteger(instance);
		}
		return replacement;
	}

	/**
	 * @see org.cpntools.simulator.extensions.Extension#getIdentifier()
	 */
	@Override
	public int getIdentifier() {
		return ID;
	}

	/**
	 * @see org.cpntools.simulator.extensions.Extension#getName()
	 */
	@Override
	public String getName() {
		return "Time Ranges";
	}

	@Override
	public Packet handle(final Packet p) {
		return null;
	}

}
