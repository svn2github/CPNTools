package org.cpntools.simulator.extensions.ppcpnets.java;

import java.util.ArrayList;
import java.util.List;

/**
 * @author michael
 */
public class Launch extends ASTNode {

	private final List<Expression> parameters;
	private final String procedure;

	/**
	 * @param next
	 * @param procedure
	 * @param list
	 */
	public Launch(final ASTNode next, final String procedure, final List<Expression> list) {
		super(next);
		this.procedure = procedure;
		parameters = new ArrayList<Expression>(list);
	}

	/**
	 * @return
	 */
	public List<Expression> getParameters() {
		return parameters;
	}

	/**
	 * @return
	 */
	public String getProcedure() {
		return procedure;
	}

}
