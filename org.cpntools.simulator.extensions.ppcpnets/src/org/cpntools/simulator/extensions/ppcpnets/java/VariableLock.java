package org.cpntools.simulator.extensions.ppcpnets.java;

/**
 * @author michael
 */
public class VariableLock extends Lock {

	private final Variable v;

	/**
	 * @param v
	 */
	public VariableLock(final Variable v) {
		super(null, 1);
		this.v = v;
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Lock#getJavaName()
	 */
	@Override
	public String getJavaName() {
		return "this.globals." + getV().getJavaName();
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Lock#getName()
	 */
	@Override
	public String getName() {
		return "[VARIABLE] " + getV().getName();
	}

	/**
	 * @return
	 */
	public Variable getV() {
		return v;
	}
}
