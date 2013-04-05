package org.cpntools.simulator.extensions.ppcpnets.java;

/**
 * @author michael
 */
public class VariableExpression extends Expression {

	private final Variable v;

	/**
	 * @param v
	 */
	public VariableExpression(final Variable v) {
		this.v = v;
	}

	/**
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(final Object obj) {
		if (this == obj) { return true; }
		if (obj == null) { return false; }
		if (!(obj instanceof VariableExpression)) { return false; }
		final VariableExpression other = (VariableExpression) obj;
		if (v == null) {
			if (other.v != null) { return false; }
		} else if (!v.equals(other.v)) { return false; }
		return true;
	}

	/**
	 * @return
	 */
	public Variable getV() {
		return v;
	}

	/**
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + (v == null ? 0 : v.hashCode());
		return result;
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Expression#simplify()
	 */
	@Override
	public Expression simplify() {
		return this;
	}

}
