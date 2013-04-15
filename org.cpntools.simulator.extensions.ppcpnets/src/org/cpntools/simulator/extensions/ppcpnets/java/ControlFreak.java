package org.cpntools.simulator.extensions.ppcpnets.java;

import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Map;
import java.util.Set;

/**
 * @author michael
 */
public class ControlFreak extends Visitor<Object, Object, ASTNode, Object> {
	private boolean changed;
	private Map<Label, Integer> counts;
	private Set<Label> labels;
	private Set<Label> seen;

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.AcquireLock)
	 */
	@Override
	public ASTNode visit(final AcquireLock e) {
		e.setNext(visit(e.getNext()));
		return e;
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.AssignmentExp)
	 */
	@Override
	public ASTNode visit(final AssignmentExp entry) {
		entry.setNext(visit(entry.getNext()));
		return entry;
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
	public ASTNode visit(final Conditional entry) {
		if (!seen.contains(entry.getJump()) && counts.get(entry.getJump()) == 1) {
			final Label nextGoto = getGoto(entry.getNext());
			if (nextGoto != null) {
				final Label jumpGoto = getGoto(entry.getJump().getNext());
				if (jumpGoto == nextGoto || isReturn(entry.getJump().getNext())) {
					unhinge(entry.getJump());
					changed = true;
					return new If(unhinge(entry), entry.getCondition(), entry.getJump().getNext(), entry.getNext());
				}
			}
		}
		entry.setJump((Label) visit(entry.getJump()));
		entry.setNext(visit(entry.getNext()));
		return entry;
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.Declaration)
	 */
	@Override
	public ASTNode visit(final Declaration entry) {
		entry.setNext(visit(entry.getNext()));
		return entry;
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.DoWhile)
	 */
	@Override
	public ASTNode visit(final DoWhile entry) {
		entry.setInner(visit(entry.getInner()));
		entry.setNext(visit(entry.getNext()));
		return entry;
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.If)
	 */
	@Override
	public ASTNode visit(final If entry) {
		if (entry.getThenBranch() == null) {
			changed = true;
			if (entry.getElseBranch() == null) { return visit(entry.getNext()); } // Kill empty if statements
			if (entry.getCondition() instanceof Not) { // Flip if statements with empty then part
				entry.setCondition(((Not) entry.getCondition()).getE());
			} else {
				entry.setCondition(new Not(entry.getCondition()));
			}
			entry.setThenBranch(entry.getElseBranch());
			entry.setElseBranch(null);
		}
		if (entry.getCondition() instanceof Not && entry.getThenBranch() != null && entry.getElseBranch() != null) {
			changed = true; // Flip negated if statements
			final ASTNode tmp = entry.getThenBranch();
			entry.setThenBranch(entry.getElseBranch());
			entry.setElseBranch(tmp);
			entry.setCondition(((Not) entry.getCondition()).getE());
		}
		entry.setThenBranch(visit(entry.getThenBranch()));
		entry.setElseBranch(visit(entry.getElseBranch()));
		entry.setNext(visit(entry.getNext()));
		return entry;
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.Jump)
	 */
	@Override
	public ASTNode visit(final Jump entry) {
		entry.setJump((Label) visit(entry.getJump()));
		entry.setNext(visit(entry.getNext()));
		return entry;
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.Label)
	 */
	@Override
	public ASTNode visit(final Label entry) {
		if (labels.contains(entry)) { return entry; }
		seen.add(entry);
		labels.add(entry);

		ASTNode node = entry.getNext();
		ASTNode previous = entry;
		out: while (node != null) {
			if (node instanceof Jump) {
				final Jump jump = (Jump) node;
				if (jump.getJump() == entry) {
					changed = true;
					final DoWhile result = new DoWhile(node.getNext(), entry.getNext(), new True());
					previous.setNext(null);
					if (jump instanceof Conditional) {
						result.setCondition(((Conditional) jump).getCondition());
					}
					entry.setNext(result);
				}
				break out;
			}
			if (node instanceof Label) {
				break out;
			}
			previous = node;
			node = node.getNext();
		}
		entry.setNext(visit(entry.getNext()));
		seen.remove(entry);
		return entry;
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.Process)
	 */
	@Override
	public Object visit(final Process p) {
		do {
			changed = false;
			labels = new HashSet<Label>();
			seen = new HashSet<Label>();
			counts = new HashMap<Label, Integer>();
			final LinkedList<ASTNode> todo = new LinkedList<ASTNode>();
			todo.add(p.getEntry());
			if (p.getEntry() instanceof Label) {
				counts.put((Label) p.getEntry(), 0);
			}
			while (!todo.isEmpty()) {
				ASTNode n = todo.removeFirst();
				while (n != null) {
					if (n instanceof Label) {
						if (!counts.containsKey(n)) {
							counts.put((Label) n, 0);
						}
					} else if (n instanceof Jump) {
						final Jump j = (Jump) n;
						if (!counts.containsKey(j.getJump())) {
							counts.put(j.getJump(), 0);
							todo.add(j.getJump());
						}
						counts.put(j.getJump(), counts.get(j.getJump()) + 1);
					}
					n = n.getNext();
				}
			}
			p.setEntry(visit(p.getEntry()));
			new SimplifyJumps().visit(p);
		} while (changed);
		return null;
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
	public ASTNode visit(final Return entry) {
		entry.setNext(visit(entry.getNext()));
		return entry;
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.Send)
	 */
	@Override
	public ASTNode visit(final Send entry) {
		entry.setNext(visit(entry.getNext()));
		return entry;
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.Skip)
	 */
	@Override
	public ASTNode visit(final Skip entry) {
		entry.setNext(visit(entry.getNext()));
		return entry;
	}

	private Label getGoto(final ASTNode next) {
		if (next == null) { return null; }
		if (next instanceof Conditional) { return null; }
		if (next instanceof Label) { return null; }
		if (next instanceof Jump) {
			if (next.getNext() == null) { return ((Jump) next).getJump(); }
		}
		return getGoto(next.getNext());
	}

	private boolean isReturn(final ASTNode next) {
		if (next == null) { return false; }
		if (next instanceof Return) { return true; }
		if (next instanceof Jump) { return false; }
		if (next instanceof Label) { return false; }
		return isReturn(next.getNext());
	}

	private ASTNode unhinge(final ASTNode next) {
		final ASTNode n = next.getNext();
		if (n instanceof Jump) {
			assert n.getNext() == null;
			next.setNext(null);
			return n;
		}
		if (n instanceof Return) { return null; }
		return unhinge(n);
	}

}
