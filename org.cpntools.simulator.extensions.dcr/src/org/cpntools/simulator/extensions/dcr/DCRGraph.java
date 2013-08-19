package org.cpntools.simulator.extensions.dcr;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

import dk.klafbang.tools.Pair;

public class DCRGraph {
	public HashSet<Pair<String, String>> conditions = new HashSet<Pair<String, String>>();
	public HashSet<String> events = new HashSet<String>();
	public HashSet<Pair<String, String>> excludes = new HashSet<Pair<String, String>>();
	public HashSet<Pair<String, String>> includes = new HashSet<Pair<String, String>>();
	public HashSet<Pair<String, String>> milestones = new HashSet<Pair<String, String>>();
	public HashMap<String, Pair<Integer, Pair<String, String>>> relationID = new HashMap<String, Pair<Integer, Pair<String, String>>>();

	public HashSet<Pair<String, String>> responses = new HashSet<Pair<String, String>>();

	public Boolean Enabled(final DCRMarking m, final String e) {
		if (!events.contains(e)) { return true; }
		// check included
		if (!m.included.contains(e)) { return false;
		// check conditions
		}

		// if (!m.executed.containsAll(RelationsFor(conditions, e)))
		final Set<String> inccon = new HashSet<String>(RelationsFor(conditions, e));
		inccon.retainAll(m.included);
		if (!m.executed.containsAll(inccon)) { return false; }

		// check milestones

		final Set<String> incmil = new HashSet<String>(RelationsFor(milestones, e));
		incmil.retainAll(m.included);

		for (final String p : m.pending) {
			if (incmil.contains(p)) { return false; }
		}
		return true;
	}

	public DCRMarking Execute(final DCRMarking m, final String e) {
		if (!events.contains(e)) { return m; }

		if (!Enabled(m, e)) { return m; }

		// DCRMarking result = new DCRMarking();

		// for (String s : m.executed)

		m.executed.add(e);

		m.pending.remove(e);
		m.pending.addAll(RelationsOf(responses, e));
		m.included.removeAll(RelationsOf(excludes, e));
		m.included.addAll(RelationsOf(includes, e));

		// TODO
		return m;
	}

	public DCRMarking InitialMarking() {
		final DCRMarking result = new DCRMarking();
		for (final String e : events) {
			result.included.add(e);
		}
		return result;
	}

	public void RemoveRealtion(final String ID) {
		if (relationID.containsKey(ID)) {
			final Integer relationType = relationID.get(ID).getFirst();
			final Pair<String, String> relation = relationID.get(ID).getSecond();
			if (relationType == 1) {
				conditions.remove(relation);
			}
			if (relationType == 2) {
				responses.remove(relation);
			}
			if (relationType == 3) {
				includes.remove(relation);
			}
			if (relationType == 4) {
				excludes.remove(relation);
			}
			if (relationType == 5) {
				milestones.remove(relation);
			}
		}
	}

	@Override
	public String toString() {
		final StringBuilder result = new StringBuilder();
		final String NEW_LINE = System.getProperty("line.separator");

		result.append(this.getClass().getName() + " DCR Graph {" + NEW_LINE);
		result.append(" Events: ");
		for (final String e : events) {
			result.append(e + "; ");
		}
		result.append(NEW_LINE);

		result.append(" Responses: ");
		for (final Pair<String, String> r : responses) {
			result.append(r.getFirst() + " *-> " + r.getSecond() + ";");
		}
		result.append(NEW_LINE);

		result.append(" Conditions: ");
		for (final Pair<String, String> r : conditions) {
			result.append(r.getFirst() + " ->* " + r.getSecond() + ";");
		}
		result.append(NEW_LINE);

		// Note that Collections and Maps also override toString
		// result.append(" RelationID: " + relationID.toString() + NEW_LINE);
		result.append("}");

		return result.toString();
	}

	private HashSet<String> RelationsFor(final HashSet<Pair<String, String>> hs, final String e) {
		final HashSet<String> result = new HashSet<String>();

		for (final Pair<String, String> r : hs) {
			if (r.getSecond().equals(e)) {
				result.add(r.getFirst());
			}
		}
		return result;
	}

	private HashSet<String> RelationsOf(final HashSet<Pair<String, String>> hs, final String e) {
		final HashSet<String> result = new HashSet<String>();

		for (final Pair<String, String> r : hs) {
			if (r.getFirst().equals(e)) {
				result.add(r.getSecond());
			}
		}
		return result;
	}

}
