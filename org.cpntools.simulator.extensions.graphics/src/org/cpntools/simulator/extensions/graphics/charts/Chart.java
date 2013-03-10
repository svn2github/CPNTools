package org.cpntools.simulator.extensions.graphics.charts;

import java.awt.Point;

import org.cpntools.simulator.extensions.Channel;
import org.cpntools.simulator.extensions.graphics.Canvas;
import org.cpntools.simulator.extensions.graphics.Text;

public abstract class Chart {
	protected Canvas c;
	protected final int width;
	protected final int height;
	private final Text header;

	public Chart(final Channel c, final String title, final int width, final int height) throws Exception {
		this.width = width;
		this.height = height;
		this.c = new Canvas(c, title);
		header = new Text(new Point(50, height - 15), title);
		this.c.add(header);
	}

}
