package org.cpntools.simulator.extensions.graphics.charts;

import java.awt.Point;

import org.cpntools.simulator.extensions.Channel;
import org.cpntools.simulator.extensions.graphics.Canvas;
import org.cpntools.simulator.extensions.graphics.Text;

/**
 * @author michael
 */
public abstract class Chart {
	private final Text header;
	protected Canvas c;
	protected final int height;
	protected final int width;

	/**
	 * @param c
	 * @param title
	 * @param width
	 * @param height
	 * @throws Exception
	 */
	public Chart(final Channel c, final String title, final int width, final int height) throws Exception {
		this.width = width;
		this.height = height;
		this.c = new Canvas(c, title);
		header = new Text(new Point(50, height - 15), title);
		this.c.add(header);
	}

}
