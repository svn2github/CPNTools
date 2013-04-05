package org.cpntools.simulator.extensions.ppcpnets.java;

/**
 * @author michael
 */
public class Receive extends Expression {
	private final Channel c;

	/**
	 * @param c
	 */
	public Receive(final Channel c) {
		this.c = c;

	}

	/**
	 * @return
	 */
	@Override
	public boolean equals(final Object other) {
		return false;
	}

	/**
	 * @return
	 */
	public Channel getC() {
		return c;
	}

	/**
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		return super.hashCode();
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Expression#simplify()
	 */
	@Override
	public Expression simplify() {
		return this;
	}
}
