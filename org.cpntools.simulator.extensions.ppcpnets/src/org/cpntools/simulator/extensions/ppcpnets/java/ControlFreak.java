package org.cpntools.simulator.extensions.ppcpnets.java;

/**
 * @author michael
 */
public class ControlFreak extends Visitor<Object, Object, ASTNode, Object> {
	private boolean changed;

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.AssignmentExp)
	 */
	@Override
	public ASTNode visit(final AssignmentExp entry) {
		entry.setNext(visit(entry.getNext()));
		return entry;
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.Conditional)
	 */
	@Override
	public ASTNode visit(final Conditional entry) {
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
		return entry;
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.If)
	 */
	@Override
	public ASTNode visit(final If entry) {
		entry.setNext(visit(entry.getNext()));
		return entry;
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.Jump)
	 */
	@Override
	public ASTNode visit(final Jump entry) {
		return entry;
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.Label)
	 */
	@Override
	public ASTNode visit(final Label entry) {
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
		return entry;
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.Process)
	 */
	@Override
	public Object visit(final Process p) {
		do {
			changed = false;
			p.setEntry(visit(p.getEntry()));
		} while (changed);
		return null;
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

}
