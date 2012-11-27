package org.cpntools.simulator.extensions.declare;

import java.util.HashSet;
import java.util.Set;

/**
 * @author michael
 */
public class Module {
	private final Set<Constraint> constraints = new HashSet<Constraint>();

	public Module() {
	}

	public void addConstraint(final Constraint c) {
		constraints.add(c);
	}

	public void removeConstraint(final Constraint c) {
		constraints.remove(c);
	}

	public Iterable<Constraint> constraints() {
		return constraints;
	}

	public void removeAllConstraints() {
		constraints.clear();
	}
}
