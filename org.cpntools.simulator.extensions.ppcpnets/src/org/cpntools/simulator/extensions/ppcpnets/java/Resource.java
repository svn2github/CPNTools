package org.cpntools.simulator.extensions.ppcpnets.java;

import org.cpntools.simulator.extensions.scraper.types.Type;

/**
 * @author michael
 */
public class Resource extends Variable {

	private final int capacity;

	/**
	 * @param id
	 * @param name
	 * @param type
	 * @param originalType
	 * @param capacity
	 */
	public Resource(final String id, final String name, final Type type, final String originalType, final int capacity) {
		super(id, name, type, originalType);
		this.capacity = capacity;
	}

	/**
	 * @return
	 */
	public int getCapacity() {
		return capacity;
	}

}
