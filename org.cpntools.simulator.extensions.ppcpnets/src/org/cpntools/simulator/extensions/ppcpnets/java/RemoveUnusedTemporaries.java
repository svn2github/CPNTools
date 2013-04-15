package org.cpntools.simulator.extensions.ppcpnets.java;

import java.util.HashSet;
import java.util.Set;
import java.util.regex.Pattern;

/**
 * @author michael
 */
public class RemoveUnusedTemporaries extends Visitor<Object, Object, ASTNode, Boolean> {
	private Pattern p;
	private Set<Label> seen;
	private String symbol;
	private Set<String> used;

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.AcquireLock)
	 */
	@Override
	public ASTNode visit(final AcquireLock e) {
		e.setNext(visit(e.getNext()));
		return e;
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.And)
	 */
	@Override
	public Boolean visit(final And e) {
		return visit(e.getE1()) || visit(e.getE2());
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.AssignmentExp)
	 */
	@Override
	public ASTNode visit(final AssignmentExp e) {
		if (e.getV() instanceof Temporary) {
			if (used(e.getV().getJavaName(), e.getNext())) {
				used.add(e.getV().getJavaName());
			} else {
				return visit(e.getNext());
			}
		}
		e.setNext(visit(e.getNext()));
		return e;
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.Comment)
	 */
	@Override
	public ASTNode visit(final Comment e) {
		e.setNext(visit(e.getNext()));
		return e;
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.Conditional)
	 */
	@Override
	public ASTNode visit(final Conditional e) {
		e.setJump(visit(e.getJump()));
		e.setNext(visit(e.getNext()));
		return e;
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.Declaration)
	 */
	@Override
	public ASTNode visit(final Declaration e) {
		e.setNext(visit(e.getNext()));
		if (used.contains(e.getV().getJavaName())) { return e; }
		return e.getNext();
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
	public Boolean visit(final Equal e) {
		return visit(e.getE1()) || visit(e.getE2());
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
		e.setJump(visit(e.getJump()));
		e.setNext(visit(e.getNext()));
		return e;
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.Label)
	 */
	@Override
	public Label visit(final Label e) {
		if (seen.add(e)) {
			e.setNext(visit(e.getNext()));
		}
		return e;
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.Not)
	 */
	@Override
	public Boolean visit(final Not e) {
		return visit(e.getE());
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.Process)
	 */
	@Override
	public Object visit(@SuppressWarnings("hiding") final Process p) {
		seen = new HashSet<Label>();
		used = new HashSet<String>();
		p.setEntry(visit(p.getEntry()));
		return null;
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.Receive)
	 */
	@Override
	public Boolean visit(final Receive e) {
		return false;
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.ReleaseLock)
	 */
	@Override
	public ASTNode visit(final ReleaseLock e) {
		e.setNext(visit(e.getNext()));
		return e;
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.Return)
	 */
	@Override
	public ASTNode visit(final Return e) {
		e.setNext(visit(e.getNext()));
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
	public Boolean visit(final True e) {
		return false;
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.VariableExpression)
	 */
	@Override
	public Boolean visit(final VariableExpression e) {
		if (e.getV() instanceof Temporary) { return e.getV().getJavaName().equals(symbol); }
		return false;
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.Whatever)
	 */
	@Override
	public Boolean visit(final Whatever e) {
		return p.matcher(" " + e.getE() + " ").matches();
	}

	private boolean used(final String javaName, final ASTNode next) {
		ASTNode node = next;
		setSymbol(javaName);
		while (node != null && !(node instanceof Label)) {
			if (node instanceof AssignmentExp) {
				final AssignmentExp e = (AssignmentExp) node;
				if (e.getV() instanceof Temporary && e.getV().getName().equals(javaName)) { return false; }
				if (visit(e.getE())) { return true; }
			} else if (node instanceof Conditional) {
				final Conditional e = (Conditional) node;
				if (visit(e.getCondition())) { return true; }
			} else if (node instanceof Send) {
				final Send e = (Send) node;
				if (visit(e.getE())) { return true; }
			}
			node = node.getNext();
		}
		return false;
	}

	void setSymbol(final String symbol) {
		this.symbol = symbol;
		p = Pattern.compile("(.*[^\\p{Alnum}_.])" + symbol + "([^\\p{Alnum}_].*)");
	}
}
