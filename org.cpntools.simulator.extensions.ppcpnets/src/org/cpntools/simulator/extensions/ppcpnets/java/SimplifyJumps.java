package org.cpntools.simulator.extensions.ppcpnets.java;

import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Map;
import java.util.Set;

/**
 * @author michael
 */
public class SimplifyJumps extends Visitor<Object, Object, ASTNode, Object> {
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
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.ASTNode)
	 */
	@Override
	public ASTNode visit(final ASTNode entry) {
		if (entry == null) { return null; }
		return super.visit(entry);
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
		if (entry.getCondition().equals(new True())) { return visit(new Jump(entry.getNext(), entry.getJump())); }
		if (entry.getJump() == entry.getNext()) {
			assert !seen.contains(entry.getJump());
			decrement(entry.getJump());
			return visit(entry.getNext());
		}
		entry.setNext(visit(entry.getNext()));
		entry.setJump((Label) visit(entry.getJump()));
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
		// The inner part cannot (by construction) contain jumps
		entry.setNext(visit(entry.getNext()));
		return entry;
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.If)
	 */
	@Override
	public ASTNode visit(final If entry) {
		// The inner part cannot (by construction) contain jumps
		entry.setNext(visit(entry.getNext()));
		return entry;
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.Jump)
	 */
	@Override
	public ASTNode visit(final Jump entry) {
		if (entry.getJump() == entry.getNext()) {
			assert !seen.contains(entry.getJump());
			decrement(entry.getJump());
			return visit(entry.getJump());
		}
		if (entry.getNext() == null && counts.get(entry.getJump()) == 1 && !seen.contains(entry.getJump())) {
			decrement(entry.getJump());
			return visit(entry.getJump());
		}
		entry.setNext(visit(entry.getNext()));
		entry.setJump((Label) visit(entry.getJump()));
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
		if (counts.get(entry) == 0) { return visit(entry.getNext()); }
		entry.setNext(visit(entry.getNext()));
		seen.remove(entry);
		return entry;
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.Launch)
	 */
	@Override
	public ASTNode visit(final Launch e) {
		e.setNext(visit(e.getNext()));
		return e;
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.Process)
	 */
	@Override
	public Object visit(final Process p) {
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
		return visit(entry.getNext());
	}

	private void decrement(final Label jump) {
		counts.put(jump, counts.get(jump) - 1);
	}

}
