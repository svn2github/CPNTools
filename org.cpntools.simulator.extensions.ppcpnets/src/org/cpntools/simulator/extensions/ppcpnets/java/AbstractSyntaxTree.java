package org.cpntools.simulator.extensions.ppcpnets.java;

import java.util.ArrayList;
import java.util.List;

import org.cpntools.simulator.extensions.scraper.types.Other;

/**
 * @author michael
 */
public class AbstractSyntaxTree implements HasJavaName {
	private String name;

	private final List<Process> processes = new ArrayList<Process>();

	/**
	 * @param p
	 */
	public void addProcess(final Process p) {
		processes.add(p);
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.HasJavaName#getJavaName()
	 */
	@Override
	public String getJavaName() {
		return new Other(name).getJavaName();
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
	public List<Process> getProcesses() {
		return processes;
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.HasJavaName#setName(java.lang.String)
	 */
	@Override
	public void setName(final String name) {
		this.name = name;
	}

}
