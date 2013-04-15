package org.cpntools.simulator.extensions.ppcpnets.java;

/**
 * @author michael
 */
public class Comment extends ASTNode {

	private String comment;

	/**
	 * @param comment
	 */
	public Comment(final String comment) {
		super(null);
		setComment(comment);
	}

	/**
	 * @return
	 */
	public String getComment() {
		return comment;
	}

	/**
	 * @param comment
	 */
	public void setComment(final String comment) {
		this.comment = comment;
	}

}
