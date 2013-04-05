package org.cpntools.simulator.extensions.ppcpnets.java;

import java.util.List;

/**
 * @author michael
 */
public class Process {
	private ASTNode entry;
	private final List<Variable> locals;
	private final String name;
	private final List<Variable> parameters;

	/**
	 * @param name
	 * @param entry
	 * @param locals
	 * @param parameters
	 */
	public Process(final String name, final ASTNode entry, final List<Variable> locals, final List<Variable> parameters) {
		this.name = name;
		this.entry = entry;
		this.locals = locals;
		this.parameters = parameters;
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
	public List<Variable> getLocals() {
		return locals;
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
