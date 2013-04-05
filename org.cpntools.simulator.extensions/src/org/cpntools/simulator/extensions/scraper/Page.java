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
	private final Map<String, Transition> transitions = new HashMap<String, Transition>();
	private final Map<String, Subpage> subpages = new HashMap<String, Subpage>();

	/**
	 * @param dictionary
	 * @param id
	 * @param name
	 */
	public Page(final ElementDictionary dictionary, final String id, final String name) {
		super(dictionary, id, name);
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
	 * @param s
	 */
	public void add(final Subpage s) {
		subpages.put(s.getId(), s);
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
	 * @param id
	 * @return
	 */
	public Subpage getSubpage(final String id) {
		return subpages.get(id);
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
	 * @return
	 */
	public Iterable<Transition> transitions() {
		return transitions.values();
	}

	/**
	 * @return
	 */
	public Iterable<Place> sockets() {
		final HashSet<Place> sockets = new HashSet<Place>();
		for (final Subpage s : subpages()) {
			for (final Assignment a : s.assignments()) {
				sockets.add(a.getSocket());
			}
		}
		return sockets;
	}

	/**
	 * @return
	 */
	public Iterable<Subpage> subpages() {
		return subpages.values();
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

	/**
	 * @param ids
	 * @return
	 */
	public Map<String, Subpage> retainSubpages(final Collection<String> ids) {
		return retain(ids, subpages);
	}
}
