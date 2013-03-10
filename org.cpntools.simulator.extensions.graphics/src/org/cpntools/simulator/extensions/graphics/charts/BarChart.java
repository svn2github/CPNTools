package org.cpntools.simulator.extensions.graphics.charts;

import java.awt.Color;
import java.awt.Point;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.cpntools.simulator.extensions.Channel;
import org.cpntools.simulator.extensions.graphics.Line;
import org.cpntools.simulator.extensions.graphics.Rectangle;
import org.cpntools.simulator.extensions.graphics.Text;

public class BarChart extends XYChart implements BarChartable {

	private final Color barColor;

	private final List<Bar> bars = new ArrayList<Bar>();
	private final Map<Bar, Rectangle> graphics = new HashMap<Bar, Rectangle>();
	private final Map<Bar, Text> labels = new HashMap<Bar, Text>();
	private final List<Line> tics = new ArrayList<Line>();

	private int max = 1;

	public BarChart(final Channel c, final String title, final int width, final int height, final Color barColor)
	        throws Exception {
		super(c, title, width, height);
		this.barColor = barColor;
	}

	public Bar addBar(final String name, final int value) {
		final Bar b = new Bar(name, value);
		b.setParent(this);
		max = Math.max(max, value);
		bars.add(b);
		repaint();
		return b;
	}

	private final static int END = 15;
	private final static int SPACING = 10;

	private void repaint() {
		try {
			c.suspend(true);
			int i = 0;
			final int w = (width - END) / bars.size();
			for (final Bar b : bars) {
				Rectangle r = graphics.get(b);
				if (r == null) {
					r = new Rectangle(1, 1, 1, 1);
					c.add(r);
					graphics.put(b, r);
					r.setForeground(Color.BLACK);
					r.setBackground(barColor);
				}
				r.setBounds(new java.awt.Rectangle(i + SPACING / 2, TEXT_SPACE, w - SPACING,
				        (height - TEXT_SPACE - TEXT_SPACE) * b.getValue() / max));

				Text t = labels.get(b);
				if (t != null && !b.getName().equals(t.getText())) {
					c.remove(t);
					t = null;
				}
				if (t == null) {
					t = new Text(0, 0, b.getName());
					labels.put(b, t);
					c.add(t);
				}
				t.setPosition(new Point(i + SPACING, TEXT_SPACE - 15));

				i += w;
			}

			while (tics.size() > bars.size()) {
				final Line remove = tics.remove(tics.size() - 1);
				c.remove(remove);
			}
			while (tics.size() < bars.size()) {
				final Line l = new Line(new Point(0, 0), new Point(0, 5), new Point(0, -5));
				c.add(l);
				tics.add(l);
			}

			i = w;
			for (final Line l : tics) {
				l.setPosition(new Point(i, TEXT_SPACE));
				i += w;
			}
			c.suspend(false);
		} catch (final Exception _) {

		}
	}

	@Override
	public void changed(final Bar b) {
		recomputeMax();
		repaint();
	}

	public void delete(final Bar b) {
		try {
			bars.remove(b);
			c.suspend(true);
			final Rectangle g = graphics.remove(b);
			c.remove(g);
			final Text l = labels.remove(b);
			c.remove(l);
			recomputeMax();
			repaint();
			c.suspend(false);
		} catch (final Exception _) {

		}
	}

	private void recomputeMax() {
		max = 0;
		for (final Bar bar : bars) {
			max = Math.max(max, bar.getValue());
		}
	}
}
