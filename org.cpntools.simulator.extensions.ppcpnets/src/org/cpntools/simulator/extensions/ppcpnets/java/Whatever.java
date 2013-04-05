package org.cpntools.simulator.extensions.ppcpnets.java;

/**
 * @author michael
 */
public class Whatever extends Expression {
	private String e;

	/**
	 * @param expression
	 */
	public Whatever(final String expression) {
		e = expression;

	}

	/**
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(final Object obj) {
		if (this == obj) { return true; }
		if (obj == null) { return false; }
		if (!(obj instanceof Whatever)) { return false; }
		final Whatever other = (Whatever) obj;
		if (e == null) {
			if (other.e != null) { return false; }
		} else if (!e.equals(other.e)) { return false; }
		return true;
	}

	/**
	 * @return
	 */
	public String getE() {
		return e;
	}

	/**
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + (e == null ? 0 : e.hashCode());
		return result;
	}

	/**
	 * @param e
	 */
	public void setE(final String e) {
		this.e = e;
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Expression#simplify()
	 */
	@Override
	public Expression simplify() {
		if ("true".equals(e)) { return new True(); }
		if ("".equals(e)) { return new True(); }
		return this;
	}
}
