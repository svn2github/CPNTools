package org.cpntools.simulator.extensions.ppcpnets.java;

/**
 * @author michael
 */
public class Conditional extends Jump {

	private Expression condition;

	/**
	 * @param condition
	 * @param next
	 * @param jump
	 */
	public Conditional(final Expression condition, final ASTNode next, final Label jump) {
		super(next, jump);
		this.condition = condition;
	}

	/**
	 * @return
	 */
	public Expression getCondition() {
		return condition;
	}

	/**
	 * @param condition
	 */
	public void setCondition(final Expression condition) {
		this.condition = condition;
	}

}
