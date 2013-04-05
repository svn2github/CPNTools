package org.cpntools.simulator.extensions.ppcpnets.java;

/**
 * @author michael
 */
public class Jump extends ASTNode {
	private Label jump;

	/**
	 * @param next
	 * @param jump
	 */
	public Jump(final ASTNode next, final Label jump) {
		super(next);
		this.jump = jump;
	}

	/**
	 * @return
	 */
	public Label getJump() {
		return jump;
	}

	/**
	 * @param jump
	 */
	public void setJump(final Label jump) {
		this.jump = jump;
	}
}
