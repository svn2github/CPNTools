package org.cpntools.simulator.extensions.declare;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

/**
 * @author michael
 */
public class Constraint {
	private final String formula;

	private final String name;

	private final Set<Task>[] parameters;

	/**
	 * @param name
	 * @param formula
	 * @param parameters
	 */
	@SuppressWarnings("unchecked")
	public Constraint(final String name, final String formula, final int parameters) {
		this.name = name;
		this.formula = formula;
		this.parameters = new Set[parameters];
		for (int i = 0; i < this.parameters.length; i++) {
			this.parameters[i] = new HashSet<Task>();
		}
	}

	/**
	 * @param i
	 * @param task
	 */
	public void addParameter(final int i, final Task task) {
		parameters[i].add(task);
	}

	/**
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(final Object obj) {
		if (this == obj) { return true; }
		if (obj == null) { return false; }
		if (!(obj instanceof Constraint)) { return false; }
		final Constraint other = (Constraint) obj;
		if (getFormula() == null) {
			if (other.getFormula() != null) { return false; }
		} else if (!getFormula().equals(other.getFormula())) { return false; }
		if (name == null) {
			if (other.name != null) { return false; }
		} else if (!name.equals(other.name)) { return false; }
		if (!Arrays.equals(parameters, other.parameters)) { return false; }
		return true;
	}

	/**
	 * @return
	 */
	public String getFormula() {
		return formula;
	}

	/**
	 * @param i
	 * @return
	 */
	public Collection<Task> getParameters(final int i) {
		return parameters[i];
	}

	/**
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + (getFormula() == null ? 0 : getFormula().hashCode());
		result = prime * result + (name == null ? 0 : name.hashCode());
		result = prime * result + Arrays.hashCode(parameters);
		return result;
	}

	/**
	 * @return
	 */
	public int parameterCount() {
		return parameters.length;
	}

	/**
	 * @return
	 */
	public Iterable<Set<Task>> parameters() {
		return Arrays.asList(parameters);
	}

	/**
	 * @param i
	 * @param tasks
	 */
	public void setParameters(final int i, final Task... tasks) {
		parameters[i].clear();
		for (final Task task : tasks) {
			addParameter(i, task);
		}
	}
}
