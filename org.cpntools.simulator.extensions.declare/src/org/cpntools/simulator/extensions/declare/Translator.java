package org.cpntools.simulator.extensions.declare;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.SortedSet;
import java.util.TreeSet;

import ltl2aut.RegExp2Automaton;
import ltl2aut.automaton.AcceptabilityFlavor;
import ltl2aut.automaton.Automaton;
import ltl2aut.automaton.scc.SCCGraph;
import ltl2aut.regexp.RegExp;
import ltl2aut.regexp.conjunction.FormulaTools;
import ltl2aut.regexp.cup_parser.CupParser;

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
	public Map<Character, Collection<Task>> buildMap(final SortedSet<Character> aps, final Constraint c) {
		assert aps.size() == c.parameterCount();
		final Map<Character, Collection<Task>> result = new HashMap<Character, Collection<Task>>();
		int i = 0;
		for (final Character ap : aps) {
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
// AcceptableFlavor.apply(sccGraph);
	}

	/**
	 * @param formula
	 * @return
	 */
	public <T> SortedSet<T> getAPs(final RegExp<T> formula) {
		return new TreeSet<T>(FormulaTools.getAtomicPropositions(formula));
	}

	/**
	 * @param c
	 * @return
	 * @throws Exception
	 */
	public RegExp<Task> parse(final Constraint c) throws Exception {
		System.out.println(c.getFormula());
		final RegExp<Character> formula = parse(c.getFormula());
		System.out.println(formula);
		final SortedSet<Character> aps = getAPs(formula);
		System.out.println(aps);
		final Map<Character, Collection<Task>> map = buildMap(aps, c);
		System.out.println(map);
		final RegExp<Task> mappedRegExp = RegExp2Automaton.INSTANCE.replaceParameters(formula, map);
		System.out.println(mappedRegExp);
		return mappedRegExp;
	}

	/**
	 * @param module
	 * @return
	 * @throws Exception
	 */
	public Map<RegExp<Task>, Constraint> parse(final Module module) throws Exception {
		final Map<RegExp<Task>, Constraint> result = new HashMap<RegExp<Task>, Constraint>();
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
	public RegExp<Character> parse(final String formula) throws Exception {
		final RegExp<Character> parsed = CupParser.parse(formula);
		return parsed;
	}

	/**
	 * @param formulas
	 * @return
	 * @throws Exception
	 */
	public <AP> Automaton translateRaw(final Map<RegExp<AP>, Constraint> formulas) throws Exception {
		return RegExp2Automaton.INSTANCE.translate(formulas.keySet());
	}

}
