package org.cpntools.simulator.extensions.ppcpnets.java;

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import dk.klafbang.tools.Pair;

/**
 * @author michael
 */
public class CFGNode {

	private static int counter = 0;

	private final Map<Variable, Assignment> assignments = new HashMap<Variable, Assignment>();

	private final String guard;
	private final int id = CFGNode.counter++;
	private final String name;

	private final Map<Channel, String> receive = new HashMap<Channel, String>();

	private final Map<Channel, String> send = new HashMap<Channel, String>();

	private final Map<Pair<String, String>, CFGNode> successors = new HashMap<Pair<String, String>, CFGNode>();
	private final String type;

	private final String variable;

	/**
	 * @param name
	 * @param guard
	 * @param variable
	 * @param type
	 */
	public CFGNode(final String name, final String guard, final String variable, final String type) {
		this.name = name;
		this.guard = guard;
		this.variable = variable;
		this.type = type;
	}

	/**
	 * @param read
	 * @param write
	 * @param v
	 */
	public void addAssignment(final String read, final String write, final Variable v) {
		final Assignment old = assignments.put(v, new Assignment(read, write, v));
		assert old == null;
	}

	/**
	 * @param function
	 * @param successor
	 */
	public void addSuccessor(final String function, final CFGNode successor) {
		final CFGNode old = successors.put(Pair.createPair(function, successor.guard), successor);
		assert old == null;
	}

	/**
	 * @return
	 */
	public Iterable<Assignment> assignments() {
		return assignments.values();
	}

	/**
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(final Object obj) {
		if (this == obj) { return true; }
		if (obj == null) { return false; }
		if (!(obj instanceof CFGNode)) { return false; }
		final CFGNode other = (CFGNode) obj;
		if (id != other.id) { return false; }
		return true;
	}

	/**
	 * @return
	 */
	public String getGuard() {
		return guard;
	}

	/**
	 * @return
	 */
	public String getName() {
		return name;
	}

	/**
	 * @return
	 */
	public String getType() {
		return type;
	}

	/**
	 * @return
	 */
	public String getVariable() {
		return variable;
	}

	/**
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + id;
		return result;
	}

	/**
	 * @param c
	 * @param value
	 */
	public void receive(final Channel c, final String value) {
		final String old = receive.put(c, value);
		assert old == null;
	}

	/**
	 * @return
	 */
	public Iterable<Entry<Channel, String>> receives() {
		return receive.entrySet();
	}

	/**
	 * @param c
	 * @param value
	 */
	public void send(final Channel c, final String value) {
		final String old = send.put(c, value);
		assert old == null;
	}

	/**
	 * @return
	 */
	public Iterable<Entry<Channel, String>> sends() {
		return send.entrySet();
	}

	/**
	 * @return
	 */
	public Set<Entry<Pair<String, String>, CFGNode>> successors() {
		return successors.entrySet();
	}
}
