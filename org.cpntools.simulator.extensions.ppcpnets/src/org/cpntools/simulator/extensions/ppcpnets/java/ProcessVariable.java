package org.cpntools.simulator.extensions.ppcpnets.java;

/**
 * @author michael
 */
public class ProcessVariable extends Variable {

	private final Launch process;
	private final Variable variable;

	/**
	 * @param process
	 * @param variable
	 */
	public ProcessVariable(final Launch process, final Variable variable) {
		super(null, null, variable.getType(), variable.getOriginalType());
		this.process = process;
		this.variable = variable;
	}

	/**
	 * @return
	 */
	public Launch getProcess() {
		return process;
	}

	/**
	 * @return
	 */
	public Variable getVariable() {
		return variable;
	}

}
