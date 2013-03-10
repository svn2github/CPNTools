package org.cpntools.simulator.extensions.graphics.charts;

import java.awt.Point;

import org.cpntools.simulator.extensions.Channel;
import org.cpntools.simulator.extensions.graphics.Line;

/**
 * @author michael
 */
public class XYChart extends XChart {

	protected final Line yaxis;

	public XYChart(final Channel c, final String title, final int width, final int height) throws Exception {
		super(c, title, width, height);
		yaxis = new Line(new Point(0, TEXT_SPACE), new Point(0, height), new Point(5, height - 10), new Point(-5,
		        height - 10), new Point(0, height));
		this.c.add(yaxis);
	}

}
