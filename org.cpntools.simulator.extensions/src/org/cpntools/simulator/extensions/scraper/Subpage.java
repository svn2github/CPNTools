package org.cpntools.simulator.extensions.scraper;

import java.util.HashSet;
import java.util.Set;

/**
 * @author michael
 */
public class Subpage extends Node {
	private final Set<Assignment> assignments = new HashSet<Assignment>();
	private final String subpage;

	/**
	 * @param dictionary
	 * @param id
	 * @param name
	 * @param page
	 * @param subpage
	 */
	public Subpage(final ElementDictionary dictionary, final String id, final String name, final Page page,
	        final String subpage) {
		super(dictionary, id, name, page);
		this.subpage = subpage;
	}

	/**
	 * @return
	 */
	public Iterable<Assignment> assignments() {
		return assignments;
	}

	/**
	 * @return
	 */
	public String getSubpage() {
		return subpage;
	}

	private final Set<Assignment> oldAssignments = new HashSet<Assignment>();

	void prepareNewAssignments() {
		oldAssignments.addAll(assignments);
	}

	boolean addAssignment(final Assignment a) {
		final boolean oldAssignment = oldAssignments.remove(a);
		if (oldAssignment) { return false; }
		assignments.add(a);
		return true;
	}

	boolean finishNewAssignments() {
		if (!oldAssignments.isEmpty()) {
			assignments.removeAll(oldAssignments);
			return true;
		}
		return false;
	}
}
