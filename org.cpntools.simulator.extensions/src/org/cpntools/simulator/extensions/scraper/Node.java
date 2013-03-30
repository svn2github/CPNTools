package org.cpntools.simulator.extensions.scraper;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

/**
 * @author michael
 */
public class Node extends HasName implements Iterable<Arc>, Cloneable {
	private final Page page;

	/**
	 * @param dictionary
	 * @param id
	 * @param name
	 * @param page
	 */
	public Node(final ElementDictionary dictionary, final String id, final String name, final Page page) {
		super(dictionary, id, name);
		this.page = page;
	}

	protected Map<String, Arc> inArcs = new HashMap<String, Arc>();
	protected Map<String, Arc> outArcs = new HashMap<String, Arc>();
	protected Map<String, Arc> testArcs = new HashMap<String, Arc>();
	protected Map<String, Arc> resetArcs = new HashMap<String, Arc>();
	protected Map<String, Arc> inhibitorArcs = new HashMap<String, Arc>();

	/**
	 * @param oldArc
	 */
	public void removeArc(final Arc oldArc) {
		inArcs.remove(oldArc.getId());
		outArcs.remove(oldArc.getId());
		testArcs.remove(oldArc.getId());
		resetArcs.remove(oldArc.getId());
		inhibitorArcs.remove(oldArc.getId());
	}

	/**
	 * @param a
	 * @return
	 */
	public boolean addArc(final Arc a) {
		switch (a.getType()) {
		case INPUT:
			if (this == a.getTransition()) {
				inArcs.put(a.getId(), a);
			} else {
				outArcs.put(a.getId(), a);
			}
			break;
		case OUTPUT:
			if (this == a.getTransition()) {
				outArcs.put(a.getId(), a);
			} else {
				inArcs.put(a.getId(), a);
			}
			break;
		case BOTHDIR:
			inArcs.put(a.getId(), a);
			outArcs.put(a.getId(), a);
			testArcs.put(a.getId(), a);
			break;
		case INHIBITOR:
			inhibitorArcs.put(a.getId(), a);
			break;
		case RESET:
			resetArcs.put(a.getId(), a);
			break;
		}
		return false;
	}

	/**
	 * @see java.lang.Object#clone()
	 */
	@Override
	public Node clone() {
		try {
			final Node clone = (Node) super.clone();
			clone.inArcs = new HashMap<String, Arc>(inArcs);
			clone.outArcs = new HashMap<String, Arc>(outArcs);
			clone.testArcs = new HashMap<String, Arc>(testArcs);
			clone.resetArcs = new HashMap<String, Arc>(resetArcs);
			clone.inhibitorArcs = new HashMap<String, Arc>(inhibitorArcs);
			return clone;
		} catch (final CloneNotSupportedException e) {
			assert false;
		}
		return null;
	}

	/**
	 * @return
	 */
	public Page getPage() {
		return page;
	}

	/**
	 * @see java.lang.Iterable#iterator()
	 */
	@Override
	public Iterator<Arc> iterator() {
		final Set<Arc> arcs = new HashSet<Arc>();
		arcs.addAll(inArcs.values());
		arcs.addAll(outArcs.values());
		arcs.addAll(inhibitorArcs.values());
		arcs.addAll(resetArcs.values());
		return arcs.iterator();
	}

	/**
	 * @return
	 */
	public Iterable<Arc> in() {
		return inArcs.values();
	}

	/**
	 * @return
	 */
	public Iterable<Arc> out() {
		return outArcs.values();
	}

	/**
	 * @return
	 */
	public Iterable<Arc> test() {
		return testArcs.values();
	}

	/**
	 * @return
	 */
	public Iterable<Arc> reset() {
		return resetArcs.values();
	}

	/**
	 * @return
	 */
	public Iterable<Arc> inhibitor() {
		return inhibitorArcs.values();
	}
}
