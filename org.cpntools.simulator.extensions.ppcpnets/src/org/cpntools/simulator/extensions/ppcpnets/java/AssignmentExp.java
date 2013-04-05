package org.cpntools.simulator.extensions.ppcpnets.java;

/**
 * @author michael
 */
public class AssignmentExp extends ASTNode {
	private Expression e;
	private final Variable v;

	/**
	 * @param next
	 * @param v
	 * @param e
	 */
	public AssignmentExp(final ASTNode next, final Variable v, final Expression e) {
		super(next);
		this.v = v;
		this.e = e;
	}

	/**
	 * @return
	 */
	public Expression getE() {
		return e;
	}

	/**
	 * @return
	 */
	public Variable getV() {
		return v;
	}

	/**
	 * @param e
	 */
	public void setE(final Expression e) {
		this.e = e;
	}
}
