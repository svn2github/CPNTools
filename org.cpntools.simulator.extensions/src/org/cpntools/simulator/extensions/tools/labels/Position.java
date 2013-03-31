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
	EAST(2),
	/**
	 * 
	 */
	NE(6),
	/**
	 * 
	 */
	NORTH(3),
	/**
	 * 
	 */
	NW(7),
	/**
	 * 
	 */
	SE(9),
	/**
	 * 
	 */
	SOUTH(5),
	/**
	 * 
	 */
	SW(8),
	/**
	 * 
	 */
	WEST(4);

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
