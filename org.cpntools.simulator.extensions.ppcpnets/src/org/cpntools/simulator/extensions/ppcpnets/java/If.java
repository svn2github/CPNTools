package org.cpntools.simulator.extensions.ppcpnets.java;

/**
 * @author michael
 */
public class If extends ASTNode {

	private Expression condition;
	private ASTNode elseBranch;
	private ASTNode thenBranch;

	/**
	 * @param next
	 * @param condition
	 * @param thenBranch
	 * @param elseBranch
	 */
	public If(final ASTNode next, final Expression condition, final ASTNode thenBranch, final ASTNode elseBranch) {
		super(next);
		this.condition = condition;
		this.thenBranch = thenBranch;
		setElseBranch(elseBranch);
	}

	/**
	 * @return
	 */
	public Expression getCondition() {
		return condition;
	}

	/**
	 * @return
	 */
	public ASTNode getElseBranch() {
		return elseBranch;
	}

	/**
	 * @return
	 */
	public ASTNode getThenBranch() {
		return thenBranch;
	}

	/**
	 * @param condition
	 */
	public void setCondition(final Expression condition) {
		this.condition = condition;
	}

	/**
	 * @param elseBranch
	 */
	public void setElseBranch(final ASTNode elseBranch) {
		this.elseBranch = elseBranch;
	}

	/**
	 * @param thenBranch
	 */
	public void setThenBranch(final ASTNode thenBranch) {
		this.thenBranch = thenBranch;
	}

}
