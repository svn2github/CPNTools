package org.cpntools.simulator.extensions.ppcpnets.java;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author michael
 */
public class SimplifyVariables extends Visitor<Object, Object, ASTNode, Expression> {
	private boolean changed;
	private Set<Label> labels;

	Map<Variable, Variable> renamings;

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.And)
	 */
	@Override
	public Expression visit(final And e) {
		e.setE1(visit(e.getE1()));
		e.setE2(visit(e.getE2()));
		return e;
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.AssignmentExp)
	 */
	@Override
	public ASTNode visit(final AssignmentExp e) {
		final Expression value = visit(e.getE());
		if (value instanceof VariableExpression) {
			final VariableExpression ve = (VariableExpression) value;
			if (e.getV().equals(ve.getV())) { return visit(e.getNext()); }
			if (ve.getV() instanceof Local || ve.getV() instanceof Temporary) {
				renamings.put(e.getV(), ve.getV());
			}
		}
		e.setE(value);
		e.setNext(visit(e.getNext()));
		return e;
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.Conditional)
	 */
	@Override
	public ASTNode visit(final Conditional e) {
		e.setNext(visit(e.getNext()));
		e.setJump(visit(e.getJump()));
		return e;
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.Declaration)
	 */
	@Override
	public ASTNode visit(final Declaration e) {
		e.setNext(super.visit(e.getNext()));
		return e;
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.DoWhile)
	 */
	@Override
	public ASTNode visit(final DoWhile entry) {
		throw new UnsupportedOperationException("Please do not use this pattern with structured programs");
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.Equal)
	 */
	@Override
	public Expression visit(final Equal e) {
		e.setE1(visit(e.getE1()));
		e.setE2(visit(e.getE2()));
		return e;
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.If)
	 */
	@Override
	public ASTNode visit(final If entry) {
		throw new UnsupportedOperationException("Please do not use this pattern with structured programs");
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.Jump)
	 */
	@Override
	public ASTNode visit(final Jump e) {
		e.setNext(visit(e.getNext()));
		e.setJump(visit(e.getJump()));
		return e;
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.Label)
	 */
	@Override
	public Label visit(final Label e) {
		if (labels.add(e)) {
			renamings = new HashMap<Variable, Variable>();
			e.setNext(visit(e.getNext()));
		}
		return e;
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.Not)
	 */
	@Override
	public Expression visit(final Not e) {
		e.setE(visit(e.getE()));
		return e;
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.Process)
	 */
	@Override
	public Object visit(final Process p) {
		do {
			renamings = new HashMap<Variable, Variable>();
			changed = false;
			labels = new HashSet<Label>();
			p.setEntry(visit(p.getEntry()));
		} while (changed);
		return null;
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.Receive)
	 */
	@Override
	public Expression visit(final Receive e) {
		return e;
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.Send)
	 */
	@Override
	public ASTNode visit(final Send e) {
		e.setNext(visit(e.getNext()));
		return e;
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.Skip)
	 */
	@Override
	public ASTNode visit(final Skip e) {
		e.setNext(visit(e.getNext()));
		return e;
	}

	/**
	 * @param e
	 * @return
	 */
	@Override
	public Expression visit(final True e) {
		return e;
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.VariableExpression)
	 */
	@Override
	public Expression visit(final VariableExpression e) {
		if (renamings.containsKey(e.getV())) {
			changed = true;
			return new VariableExpression(renamings.get(e.getV()));
		}
		return e;
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.Whatever)
	 */
	@Override
	public Expression visit(final Whatever e) {
		final String expr = e.getE().trim();
		for (final Entry<Variable, Variable> entry : renamings.entrySet()) {
			if (entry.getKey().getName().equals(expr)) { return new VariableExpression(entry.getValue()); }
		}
		String tmp = " " + expr + " ";
		@SuppressWarnings("hiding")
		boolean changed;
		do {
			changed = false;
			for (final Entry<Variable, Variable> entry : renamings.entrySet()) {
				boolean c = true;
				final Pattern p = Pattern.compile("(.*[^\\p{Alnum}_])" + entry.getKey().getJavaName()
				        + "([^\\p{Alnum}_].*)");
				while (c) {
					final Matcher m = p.matcher(tmp);
					if (m.matches()) {
						if (entry.getValue() instanceof Temporary) {
							tmp = m.group(1) + entry.getValue().getJavaName() + m.group(2);
							changed = true;
						} else if (entry.getValue() instanceof Local) {
							tmp = m.group(1) + "this." + entry.getValue().getJavaName() + m.group(2);
							changed = true;
						}
					} else {
						c = false;
					}
				}
			}
		} while (changed);
		e.setE(tmp.trim());
		return e;
	}
}
