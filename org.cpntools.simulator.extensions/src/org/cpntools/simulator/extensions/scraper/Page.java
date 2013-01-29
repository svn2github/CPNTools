package org.cpntools.simulator.extensions.scraper;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * @author michael
 */
public class Page extends HasName {
	private final Map<String, Transition> transitions = new HashMap<String, Transition>();

	/**
	 * @param id
	 * @param name
	 */
	public Page(final String id, final String name) {
		super(id, name);
	}

	public Map<String, Transition> retainTransitions(final Collection<String> ids) {
		final Set<String> idset = new HashSet<String>(ids);
		final Map<String, Transition> result = new HashMap<String, Transition>();
		for (final String id : new ArrayList<String>(transitions.keySet())) {
			if (!idset.contains(id)) {
				result.put(id, transitions.remove(id));
			}
		}
		return result;
	}

	public void add(final Transition t) {
		transitions.put(t.getId(), t);
	}

	public Transition getTransition(final String id) {
		return transitions.get(id);
	}

	public Iterable<Transition> transitions() {
		return transitions.values();
	}
}
