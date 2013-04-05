package org.cpntools.simulator.extensions.ppcpnets.java;

/**
 * @author michael
 */
public class Send extends ASTNode {

	private final Channel c;
	private Expression e;

	/**
	 * @param next
	 * @param c
	 * @param e
	 */
	public Send(final ASTNode next, final Channel c, final Expression e) {
		super(next);
		this.c = c;
		this.e = e;
	}

	/**
	 * @return
	 */
	public Channel getC() {
		return c;
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

}
