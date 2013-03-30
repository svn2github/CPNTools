package org.cpntools.simulator.extensions.tools.labels;

/**
 * @author michael (* Think 7 3 6 4 1 2 8 5 9 )
 */
public enum Position {
	/**
	 * 
	 */
	CENTER(1),
	/**
	 * 
	 */
	NORTH(3),
	/**
	 * 
	 */
	SOUTH(5),
	/**
	 * 
	 */
	EAST(2),
	/**
	 * 
	 */
	WEST(4),
	/**
	 * 
	 */
	NW(7),
	/**
	 * 
	 */
	NE(6),
	/**
	 * 
	 */
	SW(8),
	/**
	 * 
	 */
	SE(9);

	private final int id;

	Position(final int id) {
		this.id = id;
	}

	/**
	 * @return
	 */
	public int getId() {
		return id;
	}
}
