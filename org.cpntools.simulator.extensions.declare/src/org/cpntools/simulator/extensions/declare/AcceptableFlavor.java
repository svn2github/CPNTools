package org.cpntools.simulator.extensions.declare;

import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map.Entry;
import java.util.Set;

import ltl2aut.automaton.AcceptingFlavor;
import ltl2aut.automaton.Automaton;
import ltl2aut.automaton.Flavor;
import ltl2aut.automaton.scc.SCCGraph;
import ltl2aut.automaton.scc.SCCVisitor;

/**
 * @author michael
 */
public class AcceptableFlavor implements Flavor<Object> {
	/**
	 * @author michael
	 */
	public static class AcceptablePainter implements SCCVisitor<Automaton, Set<Object>> {
		private final Set<Object> allTransitions;

		AcceptablePainter(final Set<Object> allTransitions) {
			this.allTransitions = allTransitions;
		}

		/**
		 * @see ltl2aut.automaton.scc.SCCVisitor#color(ltl2aut.automaton.Automaton, java.util.List, java.util.List)
		 */
		@Override
		public Set<Object> color(final Automaton automaton, final List<Integer> list, final List<Set<Object>> value) {
			final Set<Object> result = new HashSet<Object>();
			for (final Set<Object> child : value) {
				if (child != null) {
					result.addAll(child);
					if (child == AcceptableFlavor.all) {
						break;
					} // Terminate early if some child accepts all
				}
			}

			if (result.contains(Automaton.OTHERWISE) || result.size() == allTransitions.size()) { return markAllAccepting(
			        automaton, list); }

			boolean hasAccepting = !result.isEmpty();
			for (final int state : list) {
				if (hasAccepting) {
					break;
				}
				if (AcceptingFlavor.isAccepting(automaton, state)) {
					hasAccepting = true;
				}
			}

			if (hasAccepting) {
				final Set<Integer> self = new HashSet<Integer>(list);
				for (final int s : list) {
					final int o = automaton.next(s, 0);
					if (self.contains(o) || o >= 0 && automaton.listColors(o, AcceptableFlavor.INSTANCE).size() > 0) {
						final Set<Object> next = new HashSet<Object>(allTransitions);
						next.removeAll(automaton.successors(s).keySet());
						result.addAll(next);
					}
					for (final Entry<Object, Integer> successor : automaton.successors(s).entrySet()) {
						if (successor.getKey() != Automaton.OTHERWISE) {
							if (self.contains(successor.getValue())
							        || automaton.listColors(successor.getValue(), AcceptableFlavor.INSTANCE).size() > 0) {
								result.add(successor.getKey());
							}
						}
					}
					if (result.size() == allTransitions.size()) { return markAllAccepting(automaton, list); }
				}
				assert !result.contains(Automaton.OTHERWISE);
				for (final int s : list) {
					for (final Object activity : result) {
						automaton.addColor(s, AcceptableFlavor.INSTANCE, activity);
					}
				}
			}
			return result;
		}

		/**
		 * @see ltl2aut.automaton.scc.SCCVisitor#init()
		 */
		@Override
		public Set<Object> init() {
			return Collections.emptySet();
		}

		/**
		 * @see ltl2aut.automaton.scc.SCCVisitor#visitFrom(ltl2aut.automaton.Automaton, java.util.List,
		 *      java.lang.Object)
		 */
		@Override
		public Set<Object> visitFrom(final Automaton automaton, final List<Integer> list, final Set<Object> value) {
			return null;
		}

		private Set<Object> markAllAccepting(final Automaton automaton, final List<Integer> list) {
			for (final Integer s : list) {
				automaton.addColor(s, AcceptableFlavor.INSTANCE, Automaton.OTHERWISE);
			}
			return AcceptableFlavor.all;
		}
	}

	/**
	 * 
	 */
	public static final AcceptableFlavor INSTANCE = new AcceptableFlavor();

	static final Set<Object> all = Collections.unmodifiableSet(Collections.singleton(Automaton.OTHERWISE));

	/**
	 * @param a
	 */
	public static void apply(final Automaton a) {
		AcceptableFlavor.apply(new SCCGraph<Automaton>(a));
	}

	/**
	 * @param sccGraph
	 */
	public static void apply(final SCCGraph<? extends Automaton> sccGraph) {
		final Set<Object> allTransitions = new HashSet<Object>(sccGraph.getUnderlying().getTransitions());
		allTransitions.remove(Automaton.OTHERWISE);
		sccGraph.color(new AcceptablePainter(allTransitions));
	}

	/**
	 * @see ltl2aut.automaton.Flavor#intersection(java.lang.Object[], java.lang.Object[])
	 */
	@Override
	public Task[] intersection(final Object[] c1, final Object[] c2) {
		// TODO Auto-generated method stub
		return null;
	}

	/**
	 * @see ltl2aut.automaton.Flavor#providesColor(java.lang.Object)
	 */
	@Override
	public boolean providesColor(final Object c) {
		if (c != null && c instanceof Task) { return true; }
		if (c == Automaton.OTHERWISE) { return true; }
		return false;
	}

	/**
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return "Acceptable";
	}

	/**
	 * @see ltl2aut.automaton.Flavor#union(java.lang.Object[], java.lang.Object[])
	 */
	@Override
	public Task[] union(final Object[] c1, final Object[] c2) {
		// TODO Auto-generated method stub
		return null;
	}

}
