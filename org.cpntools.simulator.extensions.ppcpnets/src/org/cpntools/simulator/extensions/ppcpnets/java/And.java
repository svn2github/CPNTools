package org.cpntools.simulator.extensions.ppcpnets.java;

/**
 * @author michael
 */
public class And extends Expression {
	private Expression e1;

	private Expression e2;

	/**
	 * @param e1
	 * @param e2
	 */
	public And(final Expression e1, final Expression e2) {
		if (e1 == null || e2 == null) { throw new NullPointerException(); }
		this.e1 = e1;
		this.e2 = e2;
	}

	/**
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(final Object obj) {
		if (this == obj) { return true; }
		if (obj == null) { return false; }
		if (!(obj instanceof And)) { return false; }
		final And other = (And) obj;
		if (e1 == null) {
			if (other.e1 != null) {
				if (other.e2 != null) { return false; }
				return other.e1.equals(e2);
			}
		}
		if (e2 == null) {
			if (other.e2 != null) {
				if (other.e1 != null) { return false; }
				return other.e2.equals(e1);
			}
		}
		if (e1 == null && e2 == null) { return true; }
		if (e1 == null) { return e2.equals(other.e2); }
		if (e2 == null) { return e1.equals(other.e1); }
		return e1.equals(other.e1) && e2.equals(other.e2) || e2.equals(other.e1) && e1.equals(other.e2);
	}

	/**
	 * @return
	 */
	public Expression getE1() {
		return e1;
	}

	/**
	 * @return
	 */
	public Expression getE2() {
		return e2;
	}

	/**
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + (e1 == null ? 0 : e1.hashCode());
		result = prime * result + (e2 == null ? 0 : e2.hashCode());
		return result;
	}

	/**
	 * @param e1
	 */
	public void setE1(final Expression e1) {
		this.e1 = e1;
	}

	/**
	 * @param e2
	 */
	public void setE2(final Expression e2) {
		this.e2 = e2;
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Expression#simplify()
	 */
	@Override
	public Expression simplify() {
		final Expression newe1 = e1.simplify();
		final Expression newe2 = e2.simplify();
		if (newe1.equals(new True())) { return newe2; }
		if (newe2.equals(new True())) { return newe1; }
		if (newe1 == e1 && newe2 == e2) { return this; }
		return new And(newe1, newe2);
	}
}
