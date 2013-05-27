package org.cpntools.simulator.extensions.ppcpnets.java;

/**
 * @author michael
 */
public class Not extends Expression {

	private Expression e;

	/**
	 * @param e
	 */
	public Not(final Expression e) {
		this.e = e;
	}

	/**
	 * @return
	 */
	public Expression getE() {
		return e;
	}

	/**
	 * @param e
	 */
	public void setE(final Expression e) {
		this.e = e;
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Expression#simplify()
	 */
	@Override
	public Expression simplify() {
		final Expression newe = e.simplify();
		if (e instanceof Not) {
			final Not n = (Not) e;
			return n.getE().simplify();
		}
		if (e == newe) { return this; }
		return new Not(newe);
	}

}
