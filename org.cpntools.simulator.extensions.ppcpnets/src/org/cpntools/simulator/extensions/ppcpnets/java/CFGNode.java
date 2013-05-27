package org.cpntools.simulator.extensions.ppcpnets.java;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

import dk.klafbang.tools.Pair;

/**
 * @author michael
 */
public class CFGNode {

	/**
	 * @author michael
	 */
	public static enum Kind {
		/** */
		INVOKE, /** */
		LAUNCH, /** */
		NORMAL;
	}

	private static int counter = 0;

	private Map<String, String> assignmentOrder;
	private final Map<Variable, Assignment> assignments = new HashMap<Variable, Assignment>();
	private final String guard;

	private final int id = CFGNode.counter++;

	private Kind kind = Kind.NORMAL;

	private final SortedSet<Lock> lock = new TreeSet<Lock>();

	private CFGNode method;
	private String methodName;
	private final String name;

	private final Map<Channel, String> receive = new HashMap<Channel, String>();
	private final Map<Channel, String> send = new HashMap<Channel, String>();

	private final Map<Pair<Pair<String, String>, String>, CFGNode> successors = new HashMap<Pair<Pair<String, String>, String>, CFGNode>();

	private final String type;

	private final SortedSet<Lock> unlock = new TreeSet<Lock>(Collections.reverseOrder());

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
	 * @param theLock
	 */
	public void addLock(final Lock theLock) {
		final boolean added = lock.add(theLock);
		assert added;
	}

	/**
	 * @param function
	 * @param successor
	 * @param id
	 */
	public void addSuccessor(final String function, final CFGNode successor, @SuppressWarnings("hiding") final String id) {
		final CFGNode old = successors.put(Pair.createPair(Pair.createPair(function, successor.guard), id), successor);
		assert old == null;
	}

	/**
	 * @param theLock
	 */
	public void addUnlock(final Lock theLock) {
		final boolean added = unlock.add(theLock);
		assert added;
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
	 * @param order
	 * @return
	 */
	public List<Assignment> getAssignments(final List<String> order) {
		final Map<String, Assignment> cache = new HashMap<String, Assignment>();
		for (final Entry<Variable, Assignment> e : assignments.entrySet()) {
			cache.put(e.getKey().getId(), e.getValue());
		}
		final List<Assignment> result = new ArrayList<Assignment>();
		final Map<String, String> reverse = new HashMap<String, String>();
		for (final Entry<String, String> e : assignmentOrder.entrySet()) {
			reverse.put(e.getValue(), e.getKey());
		}
		for (final String o : order) {
			result.add(cache.get(reverse.get(o)));
		}
		return result;
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
	public Kind getKind() {
		return kind;
	}

	/**
	 * @return
	 */
	public CFGNode getMethod() {
		return method;
	}

	/**
	 * @return
	 */
	public String getMethodName() {
		return methodName;
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
	 * @param kind
	 */
	public void setKind(final Kind kind) {
		this.kind = kind;
	}

	/**
	 * @param methodName
	 * @param method
	 */
	public void setMethod(final String methodName, final CFGNode method) {
		this.methodName = methodName;
		this.method = method;
	}

	/**
	 * @return
	 */
	public Set<Entry<Pair<Pair<String, String>, String>, CFGNode>> successors() {
		return successors.entrySet();
	}

	Map<String, String> getAssignmentOrder() {
		return assignmentOrder;
	}

	Iterable<Lock> locks() {
		return lock;
	}

	void setAssignmentOrder(final Map<String, String> map) {
		assignmentOrder = map;
	}

	Iterable<Lock> unlocks() {
		return unlock;
	}
}
