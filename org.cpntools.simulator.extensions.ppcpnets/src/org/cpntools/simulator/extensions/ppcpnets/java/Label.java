package org.cpntools.simulator.extensions.ppcpnets.java;

/**
 * @author michael
 */
public class Label extends ASTNode {

	private final String label;

	/**
	 * @param next
	 * @param label
	 */
	public Label(final ASTNode next, final String label) {
		super(next);
		this.label = label;
	}

	/**
	 * @return
	 */
	public String getLabel() {
		return label;
	}

}
