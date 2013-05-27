package org.cpntools.simulator.extensions.ppcpnets.java;

import java.util.List;

/**
 * @author michael
 */
public class Process {
	private ASTNode entry;
	private final List<String> initial;
	private final List<Variable> locals;
	private final List<Lock> locks;
	private final String name;
	private final List<Variable> parameters;

	/**
	 * @param name
	 * @param entry
	 * @param locals
	 * @param parameters
	 * @param locks
	 * @param initial
	 */
	public Process(final String name, final ASTNode entry, final List<Variable> locals,
	        final List<Variable> parameters, final List<Lock> locks, final List<String> initial) {
		this.name = name;
		this.entry = entry;
		this.locals = locals;
		this.parameters = parameters;
		this.locks = locks;
		this.initial = initial;
	}

	/**
	 * @return
	 */
	public ASTNode getEntry() {
		return entry;
	}

	/**
	 * @return
	 */
	public List<String> getInitial() {
		return initial;
	}

	/**
	 * @return
	 */
	public List<Variable> getLocals() {
		return locals;
	}

	/**
	 * @return
	 */
	public List<Lock> getLocks() {
		return locks;
	}

	/**
	 * @return
	 */
	public String getName() {
		return name;
	}

	/**
	 * @return
	 */
	public List<Variable> getParameters() {
		return parameters;
	}

	/**
	 * @param entry
	 */
	public void setEntry(final ASTNode entry) {
		this.entry = entry;
	}
}
