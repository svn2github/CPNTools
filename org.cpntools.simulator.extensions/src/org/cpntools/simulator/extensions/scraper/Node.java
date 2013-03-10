package org.cpntools.simulator.extensions.scraper;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/**
 * @author michael
 */
public class Node extends HasName implements Iterable<Arc>, Cloneable {
	private List<Arc> arcs = new ArrayList<Arc>();

	private final Page page;

	/**
	 * @param id
	 * @param name
	 * @param page
	 */
	public Node(final String id, final String name, final Page page) {
		super(id, name);
		this.page = page;
	}

	/**
	 * @param a
	 */
	public void addArc(final Arc a) {
		arcs.add(a);
	}

	/**
	 * @see java.lang.Object#clone()
	 */
	@Override
	public Node clone() {
		try {
			final Node clone = (Node) super.clone();
			clone.arcs = new ArrayList<Arc>(arcs);
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
		return arcs.iterator();
	}
}
