package org.cpntools.simulator.extensions.ppcpnets.java;

/**
 * @author michael
 */
public class Assignment {

	private final String read;
	private final Variable variable;
	private final String write;

	/**
	 * @param read
	 * @param write
	 * @param v
	 */
	public Assignment(final String read, final String write, final Variable v) {
		this.read = read;
		this.write = write;
		variable = v;
	}

	/**
	 * @return
	 */
	public String getRead() {
		return read;
	}

	/**
	 * @return
	 */
	public Variable getVariable() {
		return variable;
	}

	/**
	 * @return
	 */
	public String getWrite() {
		return write;
	}

}
