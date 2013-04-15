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
	private final Map<String, Place> places = new HashMap<String, Place>();
	private boolean prime;
	private final Map<String, Transition> transitions = new HashMap<String, Transition>();

	/**
	 * @param dictionary
	 * @param id
	 * @param name
	 * @param prime
	 */
	public Page(final ElementDictionary dictionary, final String id, final String name, final boolean prime) {
		super(dictionary, id, name);
		this.prime = prime;
	}

	/**
	 * @param pp
	 */
	public void add(final Place pp) {
		places.put(pp.getId(), pp);
	}

	/**
	 * @param t
	 */
	public void add(final Transition t) {
		transitions.put(t.getId(), t);
	}

	/**
	 * @param id
	 * @return
	 */
	public Place getPlace(final String id) {
		return places.get(id);
	}

	/**
	 * @param id
	 * @return
	 */
	public Transition getTransition(final String id) {
		return transitions.get(id);
	}

	/**
	 * @return
	 */
	public boolean isPrime() {
		return prime;
	}

	/**
	 * @return
	 */
	public Iterable<Place> places() {
		return places.values();
	}

	/**
	 * @param ids
	 * @return
	 */
	public Map<String, Place> retainPlaces(final Collection<String> ids) {
		return retain(ids, places);
	}

	/**
	 * @param ids
	 * @return
	 */
	public Map<String, Transition> retainTransitions(final Collection<String> ids) {
		return retain(ids, transitions);
	}

	/**
	 * @param prime
	 * @return
	 */
	public boolean setPrime(final boolean prime) {
		if (prime == this.prime) { return false; }
		this.prime = prime;
		return true;
	}

	/**
	 * @return
	 */
	public Iterable<Transition> transitions() {
		return transitions.values();
	}

	private <E extends Element> Map<String, E> retain(final Collection<String> ids, final Map<String, E> nodes) {
		final Set<String> idset = new HashSet<String>(ids);
		final Map<String, E> result = new HashMap<String, E>();
		for (final String id : new ArrayList<String>(nodes.keySet())) {
			if (!idset.contains(id)) {
				result.put(id, nodes.remove(id));
			}
		}
		return result;
	}

}
