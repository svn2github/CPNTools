package org.cpntools.simulator.extensions.ppcpnets.java;

/**
 * @author michael
 */
public class Declaration extends ASTNode {

	private final Variable v;

	/**
	 * @param next
	 * @param v
	 */
	public Declaration(final ASTNode next, final Variable v) {
		super(next);
		this.v = v;
	}

	/**
	 * @return
	 */
	public Variable getV() {
		return v;
	}

}
