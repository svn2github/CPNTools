package org.cpntools.simulator.extensions.graphics.charts;

import java.awt.Point;

import org.cpntools.simulator.extensions.Channel;
import org.cpntools.simulator.extensions.graphics.Line;

/**
 * @author michael
 */
public abstract class XChart extends Chart {

	protected static final int END = 15;
	protected final static int TEXT_SPACE = 42;
	protected final Line xaxis;

	/**
	 * @param c
	 * @param title
	 * @param width
	 * @param height
	 * @throws Exception
	 */
	public XChart(final Channel c, final String title, final int width, final int height) throws Exception {
		super(c, title, width, height);
		xaxis = new Line(new Point(0, XChart.TEXT_SPACE), new Point(width, XChart.TEXT_SPACE));
		xaxis.setLineStyle(2);
		this.c.add(xaxis);
	}
}
