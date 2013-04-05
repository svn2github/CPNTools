package org.cpntools.simulator.extensions.ppcpnets.java;

import java.util.HashSet;
import java.util.Set;

/**
 * @author michael
 */
public class SimplifyExpressions extends Visitor<Object, Object, Object, Object> {
	private Set<Label> labels;

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.AssignmentExp)
	 */
	@Override
	public Object visit(final AssignmentExp entry) {
		entry.setE(entry.getE().simplify());
		return null;
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.ASTNode)
	 */
	@Override
	public Object visit(final ASTNode entry) {
		if (entry == null) { return null; }
		super.visit(entry);
		visit(entry.getNext());
		return null;
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.AssignmentExp)
	 */
	@Override
	public Object visit(final Conditional entry) {
		entry.setCondition(entry.getCondition().simplify());
		if (!labels.contains(entry.getJump())) {
			visit((ASTNode) entry.getJump());
		}
		return null;
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.DoWhile)
	 */
	@Override
	public Object visit(final DoWhile entry) {
		entry.setCondition(entry.getCondition().simplify());
		visit(entry.getInner());
		return null;
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.If)
	 */
	@Override
	public Object visit(final If entry) {
		entry.setCondition(entry.getCondition().simplify());
		visit(entry.getThenBranch());
		visit(entry.getElseBranch());
		return null;
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.Jump)
	 */
	@Override
	public Object visit(final Jump e) {
		if (!labels.contains(e.getJump())) {
			visit((ASTNode) e.getJump());
		}
		return null;
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.Label)
	 */
	@Override
	public Object visit(final Label e) {
		labels.add(e);
		return null;
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.Process)
	 */
	@Override
	public Object visit(final Process p) {
		labels = new HashSet<Label>();
		return super.visit(p);
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.Send)
	 */
	@Override
	public Object visit(final Send entry) {
		entry.setE(entry.getE().simplify());
		return null;
	}
}
