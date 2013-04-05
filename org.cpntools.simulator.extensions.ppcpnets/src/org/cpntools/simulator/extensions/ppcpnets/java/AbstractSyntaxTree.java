package org.cpntools.simulator.extensions.ppcpnets.java;

import java.util.ArrayList;
import java.util.List;

/**
 * @author michael
 */
public class AbstractSyntaxTree {
	private final List<Process> processes = new ArrayList<Process>();

	/**
	 * @param p
	 */
	public void addProcess(final Process p) {
		processes.add(p);
	}

	/**
	 * @return
	 */
	public List<Process> getProcesses() {
		return processes;
	}

}
