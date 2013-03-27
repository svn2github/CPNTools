package org.cpntools.simulator.extensions.graphics;

import java.awt.Dimension;
import java.awt.Point;

/**
 * @author michael
 * @Deprecated - misspelling, use Ellipse instead
 */
public class Ellipsis extends Ellipse {

	/**
	 * @param x
	 * @param y
	 * @param width
	 * @param height
	 * @Deprecated - misspelling, use Ellipse instead
	 */
	public Ellipsis(final int x, final int y, final int width, final int height) {
		super(x, y, width, height);
	}

	/**
	 * @param bounds
	 *            * @Deprecated - misspelling, use Ellipse instead
	 */
	public Ellipsis(final java.awt.Rectangle bounds) {
		super(bounds);
	}

	/**
	 * @param position
	 * @param size
	 * @Deprecated - misspelling, use Ellipse instead
	 */
	public Ellipsis(final Point position, final Dimension size) {
		super(position, size);
	}

}
