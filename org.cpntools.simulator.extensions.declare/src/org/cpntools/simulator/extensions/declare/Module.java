package org.cpntools.simulator.extensions.declare;

import java.util.HashMap;
import java.util.Map;

/**
 * @author michael
 */
public class Module {
	private final Map<String, Constraint> constraints = new HashMap<String, Constraint>();

	/**
	 * 
	 */
	public Module() {
	}

	/**
	 * @param id
	 * @param c
	 */
	public void addConstraint(final String id, final Constraint c) {
		constraints.put(id, c);
	}

	/**
	 * @return
	 */
	public Iterable<Constraint> constraints() {
		return constraints.values();
	}

	/**
	 * 
	 */
	public void removeAllConstraints() {
		constraints.clear();
	}

	/**
	 * @param id
	 */
	public void removeConstraint(final String id) {
		constraints.remove(id);
	}

	/**
	 * @return
	 */
	public int count() {
		return constraints.size();
	}
}
