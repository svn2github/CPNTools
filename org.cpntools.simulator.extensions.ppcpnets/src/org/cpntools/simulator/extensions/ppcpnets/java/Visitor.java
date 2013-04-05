package org.cpntools.simulator.extensions.ppcpnets.java;

/**
 * @author michael
 * @param <T>
 * @param <P>
 * @param <N>
 * @param <E>
 */
public abstract class Visitor<T, P, N, E> {
	/**
	 * @param ast
	 * @return
	 */
	public T visit(final AbstractSyntaxTree ast) {
		for (final Process process : ast.getProcesses()) {
			visit(process);
		}
		return null;
	}

	/**
	 * @param e
	 * @return
	 */
	public E visit(final And e) {
		return null;
	}

	/**
	 * @param entry
	 * @return
	 */
	public N visit(final AssignmentExp entry) {
		return null;
	}

	/**
	 * @param entry
	 * @return
	 */
	public N visit(final ASTNode entry) {
		if (entry == null) { return null; }
		if (entry instanceof AssignmentExp) {
			return visit((AssignmentExp) entry);
		} else if (entry instanceof Declaration) {
			return visit((Declaration) entry);
		} else if (entry instanceof Conditional) {
			return visit((Conditional) entry);
		} else if (entry instanceof Jump) {
			return visit((Jump) entry);
		} else if (entry instanceof Label) {
			return visit((Label) entry);
		} else if (entry instanceof Send) {
			return visit((Send) entry);
		} else if (entry instanceof Skip) {
			return visit((Skip) entry);
		} else if (entry instanceof If) {
			return visit((If) entry);
		} else if (entry instanceof DoWhile) {
			return visit((DoWhile) entry);
		} else {
			assert false;
		}
		return null;
	}

	/**
	 * @param entry
	 * @return
	 */
	public N visit(final Conditional entry) {
		return null;
	}

	/**
	 * @param entry
	 * @return
	 */
	public N visit(final Declaration entry) {
		return null;
	}

	/**
	 * @param entry
	 * @return
	 */
	public N visit(final DoWhile entry) {
		return null;
	}

	/**
	 * @param e
	 * @return
	 */
	public E visit(final Equal e) {
		return null;
	}

	/**
	 * @param e
	 * @return
	 */
	public E visit(final Expression e) {
		if (e instanceof And) {
			return visit((And) e);
		} else if (e instanceof Equal) {
			return visit((Equal) e);
		} else if (e instanceof Receive) {
			return visit((Receive) e);
		} else if (e instanceof VariableExpression) {
			return visit((VariableExpression) e);
		} else if (e instanceof True) {
			return visit((True) e);
		} else if (e instanceof Not) {
			return visit((Not) e);
		} else if (e instanceof Whatever) {
			return visit((Whatever) e);
		} else {
			assert false;
		}
		return null;
	}

	/**
	 * @param entry
	 * @return
	 */
	public N visit(final If entry) {
		return null;
	}

	/**
	 * @param entry
	 * @return
	 */
	public N visit(final Jump entry) {
		return null;
	}

	/**
	 * @param entry
	 * @return
	 */
	public N visit(final Label entry) {
		return null;
	}

	/**
	 * @param entry
	 * @return
	 */
	public E visit(final Not entry) {
		return null;
	}

	/**
	 * @param process
	 * @return
	 */
	public P visit(final Process process) {
		visit(process.getEntry());
		return null;
	}

	/**
	 * @param e
	 * @return
	 */
	public E visit(final Receive e) {
		return null;
	}

	/**
	 * @param entry
	 * @return
	 */
	public N visit(final Send entry) {
		return null;
	}

	/**
	 * @param entry
	 * @return
	 */
	public N visit(final Skip entry) {
		return null;
	}

	/**
	 * @param e
	 * @return
	 */
	public E visit(final True e) {
		return null;
	}

	/**
	 * @param e
	 * @return
	 */
	public E visit(final VariableExpression e) {
		return null;
	}

	/**
	 * @param e
	 * @return
	 */
	public E visit(final Whatever e) {
		return null;
	}

}