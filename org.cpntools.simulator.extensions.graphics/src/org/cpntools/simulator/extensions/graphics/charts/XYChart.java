package org.cpntools.simulator.extensions.graphics.charts;

import java.awt.Point;

import org.cpntools.simulator.extensions.Channel;
import org.cpntools.simulator.extensions.graphics.Line;

/**
 * @author michael
 */
public class XYChart extends XChart {

	protected final Line yaxis;

	/**
	 * @param c
	 * @param title
	 * @param width
	 * @param height
	 * @throws Exception
	 */
	public XYChart(final Channel c, final String title, final int width, final int height) throws Exception {
		super(c, title, width, height);
		yaxis = new Line(new Point(0, XChart.TEXT_SPACE), new Point(0, height));
		yaxis.setLineStyle(2);
		this.c.add(yaxis);
	}

}
