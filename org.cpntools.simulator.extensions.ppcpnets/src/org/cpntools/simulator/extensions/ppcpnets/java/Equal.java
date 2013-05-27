package org.cpntools.simulator.extensions.ppcpnets.java;

/**
 * @author michael
 */
public class Equal extends Expression {

	private Expression e1;
	private Expression e2;

	/**
	 * @param e1
	 * @param e2
	 */
	public Equal(final Expression e1, final Expression e2) {
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
		if (!(obj instanceof Equal)) { return false; }
		final Equal other = (Equal) obj;
		if (e1 == null) {
			if (other.e1 != null) { return false; }
		} else if (!e1.equals(other.e1)) { return false; }
		if (e2 == null) {
			if (other.e2 != null) { return false; }
		} else if (!e2.equals(other.e2)) { return false; }
		return true;
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
		if (newe1.equals(newe2)) { return new True(); }
		if (newe1.equals(new True())) { return newe2; }
		if (newe2.equals(new True())) { return newe1; }
		if (newe1 instanceof Whatever) {
			final Whatever w = (Whatever) newe1;
			if ("true".equals(w.getE())) { return newe2; }
			if ("false".equals(w.getE())) { return new Not(newe2).simplify(); }
		}
		if (newe2 instanceof Whatever) {
			final Whatever w = (Whatever) newe2;
			if ("true".equals(w.getE())) { return newe1; }
			if ("false".equals(w.getE())) { return new Not(newe1).simplify(); }
		}
		if (newe1 == e1 && newe2 == e2) { return this; }
		return new And(newe1, newe2);
	}

}
