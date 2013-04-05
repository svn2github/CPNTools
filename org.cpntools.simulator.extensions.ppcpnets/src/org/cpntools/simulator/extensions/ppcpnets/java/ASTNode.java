package org.cpntools.simulator.extensions.ppcpnets.java;

/**
 * @author michael
 */
public abstract class ASTNode {
	private ASTNode next;

	/**
	 * @param next
	 */
	public ASTNode(final ASTNode next) {
		this.next = next;
	}

	/**
	 * @return
	 */
	public ASTNode getNext() {
		return next;
	}

	/**
	 * @param next
	 */
	public void setNext(final ASTNode next) {
		this.next = next;
	}
}
