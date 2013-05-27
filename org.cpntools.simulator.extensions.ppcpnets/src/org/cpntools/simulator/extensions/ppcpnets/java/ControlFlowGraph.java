package org.cpntools.simulator.extensions.ppcpnets.java;

import java.util.Collection;
import java.util.List;
import java.util.Map;

import dk.klafbang.tools.Pair;

/**
 * @author michael
 */
public class ControlFlowGraph {

	private final Collection<Channel> channels;
	private final Map<String, Pair<List<String>, CFGNode>> processes;
	private final Collection<Global> shared;

	/**
	 * @param shared
	 * @param channels
	 * @param processes
	 */
	public ControlFlowGraph(final Collection<Global> shared, final Collection<Channel> channels,
	        final Map<String, Pair<List<String>, CFGNode>> processes) {
		this.shared = shared;
		this.channels = channels;
		this.processes = processes;
	}

	/**
	 * @return
	 */
	public Iterable<Channel> channels() {
		return channels;
	}

	/**
	 * @return
	 */
	public Map<String, Pair<List<String>, CFGNode>> getProcesses() {
		return processes;
	}

	/**
	 * @return
	 */
	public Iterable<Global> shared() {
		return shared;
	}

}
