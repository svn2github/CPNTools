package org.cpntools.simulator.extensions.ppcpnets.java;

/**
 * @author michael
 */
public class DoWhile extends ASTNode {

	private Expression condition;
	private ASTNode inner;

	/**
	 * @param next
	 * @param inner
	 * @param condition
	 */
	public DoWhile(final ASTNode next, final ASTNode inner, final Expression condition) {
		super(next);
		setInner(inner);
		setCondition(condition);
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
	public ASTNode getInner() {
		return inner;
	}

	/**
	 * @param condition
	 */
	public void setCondition(final Expression condition) {
		this.condition = condition;
	}

	/**
	 * @param inner
	 */
	public void setInner(final ASTNode inner) {
		this.inner = inner;
	}

}
