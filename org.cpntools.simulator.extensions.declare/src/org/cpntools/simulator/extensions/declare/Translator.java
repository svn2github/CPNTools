package org.cpntools.simulator.extensions.declare;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.SortedSet;
import java.util.TreeSet;

import ltl2aut.APCollector;
import ltl2aut.LTL2Automaton;
import ltl2aut.automaton.AcceptabilityFlavor;
import ltl2aut.automaton.Automaton;
import ltl2aut.automaton.scc.SCCGraph;
import ltl2aut.cup_parser.CupParser;
import ltl2aut.formula.Atomic;
import ltl2aut.formula.Formula;
import ltl2aut.formula.Or;
import ltl2aut.formula.sugared.visitor.SimplifyVisitor;
import ltl2aut.formula.visitor.Mapper;

/**
 * @author michael
 */
public class Translator {
	/**
	 * 
	 */
	public static final Translator INSTANCE = new Translator();

	private Translator() {
	}

	/**
	 * @param aps
	 * @param c
	 * @return
	 */
	public Map<String, Collection<Task>> buildMap(final SortedSet<String> aps, final Constraint c) {
		assert aps.size() == c.parameterCount();
		final Map<String, Collection<Task>> result = new HashMap<String, Collection<Task>>();
		int i = 0;
		for (final String ap : aps) {
			result.put(ap, c.getParameters(i++));
		}
		return result;
	}

	/**
	 * @param a
	 */
	public void colorAutomaton(final Automaton a) {
		final SCCGraph<Automaton> sccGraph = new SCCGraph<Automaton>(a);
		AcceptabilityFlavor.apply(sccGraph);
		AcceptableFlavor.apply(sccGraph);

	}

	/**
	 * @param formula
	 * @return
	 */
	public <T> SortedSet<T> getAPs(final Formula<T> formula) {
		return new TreeSet<T>(APCollector.apply(formula));
	}

	/**
	 * @param c
	 * @return
	 * @throws Exception
	 */
	public Formula<Task> parse(final Constraint c) throws Exception {
		final Formula<String> formula = parse(c.getFormula());
		final SortedSet<String> aps = getAPs(formula);
		final Map<String, Collection<Task>> map = buildMap(aps, c);
		final Formula<Task> mappedFormula = replaceParameters(formula, map);
		return mappedFormula;
	}

	/**
	 * @param module
	 * @return
	 * @throws Exception
	 */
	public Map<Formula<Task>, Constraint> parse(final Module module) throws Exception {
		final Map<Formula<Task>, Constraint> result = new HashMap<Formula<Task>, Constraint>();
		for (final Constraint c : module.constraints()) {
			result.put(parse(c), c);
		}
		return result;
	}

	/**
	 * @param formula
	 * @return
	 * @throws Exception
	 */
	public Formula<String> parse(final String formula) throws Exception {
		final Formula<String> parsed = CupParser.parse(formula);
		final Formula<String> simplified = SimplifyVisitor.apply(parsed);
		return simplified;
	}

	/**
	 * @param f
	 * @param parameters
	 * @return
	 */
	public <S, T> Formula<T> replaceParameters(final Formula<S> f, final Map<S, Collection<T>> parameters) {
		return f.traverse(new Mapper<S, T>(null) {
			@Override
			public Formula<T> atomic(final S ap) {
				final Collection<T> tasks = parameters.get(ap);
				if (tasks == null || tasks.isEmpty()) { return t(); }
				Formula<T> result = null;
				for (final T activity : tasks) {
					if (result == null) {
						result = new Atomic<T>(activity);
					} else {
						result = new Or<T>(result, new Atomic<T>(activity));
					}
				}
				return result;
			}
		});
	}

	/**
	 * @param f
	 * @param parameters
	 * @return
	 */
	public <S, T> Formula<T> replaceParametersPrimitive(final Formula<S> f, final Map<S, T> parameters) {
		return f.traverse(new Mapper<S, T>(null) {
			@Override
			public Formula<T> atomic(final S ap) {
				final T tasks = parameters.get(ap);
				if (tasks == null) { return t(); }
				return new Atomic<T>(tasks);
			}
		});
	}

	/**
	 * @param formulas
	 * @return
	 * @throws Exception
	 */
	public <AP> Automaton translateRaw(final Map<Formula<AP>, Constraint> formulas) throws Exception {
		return LTL2Automaton.INSTANCE.translate(formulas.keySet());
	}

}
