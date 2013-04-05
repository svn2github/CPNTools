package org.cpntools.simulator.extensions.ppcpnets.java;

import org.cpntools.simulator.extensions.scraper.types.Type;

/**
 * @author michael
 */
public class Global extends Variable {

	private final String init;

	/**
	 * @param id
	 * @param name
	 * @param type
	 * @param originalType
	 * @param init
	 */
	public Global(final String id, final String name, final Type type, final String originalType, final String init) {
		super(id, name, type, originalType);
		this.init = init;
	}

	/**
	 * @return
	 */
	public String getInit() {
		return init;
	}

}
