package org.cpntools.simulator.extensions.graphics;

/**
 * @author michael
 */
public class Group extends Composite {
	private static int nextId = 0;

	public Group() {
		setId("__group" + ++nextId);
	}
}
