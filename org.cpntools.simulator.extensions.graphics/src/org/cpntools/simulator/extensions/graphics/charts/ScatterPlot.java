package org.cpntools.simulator.extensions.graphics.charts;

import java.awt.Point;
import java.awt.geom.Point2D;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import org.cpntools.simulator.extensions.Channel;
import org.cpntools.simulator.extensions.graphics.Ellipsis;
import org.cpntools.simulator.extensions.graphics.Line;
import org.cpntools.simulator.extensions.graphics.Text;

/**
 * @author michael
 */
public class ScatterPlot extends XYChart {
	private static final int POINT_SIZE = 4;
	private Text equation = null;
	private final Map<Point2D, Ellipsis> graphics = new HashMap<Point2D, Ellipsis>();
	private Line line = null;
	private double maxX = 0, maxY = 0;
	private final List<Point2D> points = new ArrayList<Point2D>();
	private boolean showTrend = false, showLine = false, showSortedLine = false;

	private Line sortedLine = null;

	private Line trend = null;

	/**
	 * @param c
	 * @param title
	 * @param width
	 * @param height
	 * @throws Exception
	 */
	public ScatterPlot(final Channel c, final String title, final int width, final int height) throws Exception {
		super(c, title, width, height);
	}

	/**
	 * @param p
	 */
	public void addPoint(final Point2D p) {
		maxX = Math.max(maxX, p.getX());
		maxY = Math.max(maxY, p.getY());
		points.add(p);
		repaint();
	}

	/**
	 * @return
	 */
	public boolean isShowLine() {
		return showLine;
	}

	/**
	 * @return
	 */
	public boolean isShowSortedLine() {
		return showSortedLine;
	}

	/**
	 * @return
	 */
	public boolean isShowTrend() {
		return showTrend;
	}

	/**
	 * @param p
	 */
	public void removePoint(final Point p) {
		if (points.remove(p)) {
			recomputeMax();
			repaint();
		}
	}

	/**
	 * @param showLine
	 */
	public void setShowLine(final boolean showLine) {
		if (this.showLine == showLine) { return; }
		this.showLine = showLine;
		repaint();
	}

	/**
	 * @param showSortedLine
	 */
	public void setShowSortedLine(final boolean showSortedLine) {
		if (this.showSortedLine == showSortedLine) { return; }
		this.showSortedLine = showSortedLine;
		repaint();
	}

	/**
	 * @param showTrend
	 */
	public void setShowTrend(final boolean showTrend) {
		if (this.showTrend == showTrend) { return; }
		this.showTrend = showTrend;
		repaint();
	}

	private void recomputeMax() {
		maxX = maxY = 0;
		for (final Point2D p : points) {
			maxX = Math.max(maxX, p.getX());
			maxY = Math.max(maxY, p.getY());
		}
	}

	private void repaint() {
		try {
			c.suspend(true);
			final List<Point> linepoints = new ArrayList<Point>();
			double sp = 0, sx = 0, sy = 0, ss = 0;
			for (final Point2D p : points) {
				final double xx = p.getX() * (width - END) / maxX;
				final int x = (int) Math.round(xx);
				final double yy = p.getY() * (height - 2 * TEXT_SPACE) / maxY;
				final int y = (int) Math.round(yy);
				sp += xx * yy;
				sx += xx;
				sy += yy;
				ss += xx * xx;
				Ellipsis r = graphics.get(p);
				if (r == null) {
					r = new Ellipsis(0, 0, POINT_SIZE, POINT_SIZE);
					graphics.put(p, r);
					c.add(r);
				}
				final Point position = new Point(x - POINT_SIZE / 2, TEXT_SPACE + y - POINT_SIZE / 2);
				linepoints.add(position);
				r.setPosition(position);
			}
			if (line != null) {
				c.remove(line);
				line = null;
			}
			if (showLine) {
				line = new Line(linepoints);
				line.setLineWidth(2);
				c.add(line);
			}
			if (sortedLine != null) {
				c.remove(sortedLine);
				sortedLine = null;
			}
			if (showSortedLine) {
				final Set<Point> set = new TreeSet<Point>(new Comparator<Point>() {
					@Override
					public int compare(final Point arg0, final Point arg1) {
						if (arg0.getX() == arg1.getX()) { return (int) Math.signum(arg0.getY() - arg1.getY()); }
						return (int) Math.signum(arg0.getX() - arg1.getX());
					}
				});
				set.addAll(linepoints);
				sortedLine = new Line(set);
				sortedLine.setLineWidth(2);
				c.add(sortedLine);
			}
			if (trend != null) {
				c.remove(trend);
				trend = null;
				c.remove(equation);
				equation = null;
			}
			if (showTrend) {
				final double b = (sp - sx * sy / points.size()) / (ss - sx * sx / points.size());
				final double a = (sy - b * sx) / points.size();
				if (b == 0) {
					if (a >= 0 && a < height - TEXT_SPACE) {
						trend = new Line(new Point(0, TEXT_SPACE + (int) a), new Point(width, TEXT_SPACE + (int) a));
					}
				} else if (b > 0) {
					trend = new Line(new Point((int) Math.round(Math.max(0, -a / b)), TEXT_SPACE
					        + (int) Math.round(Math.max(a, 0))), new Point((int) Math.round(Math.min(width, (height
					        - TEXT_SPACE - a)
					        / b)), TEXT_SPACE + (int) Math.round(Math.min(height - TEXT_SPACE, a + b * width))));
				} else {
					trend = new Line(new Point((int) Math.round(Math.max(0, -a / b)), TEXT_SPACE
					        + (int) Math.round(Math.min(a, height - TEXT_SPACE))), new Point((int) Math.round(Math.min(
					        width, -a / b)), TEXT_SPACE + (int) Math.round(Math.max(0, a + b * width))));
				}

				if (trend != null) {
					trend.setLineWidth(2);
					c.add(trend);
				}

				if (b < 0) {
					equation = new Text(width / 2, height - 15, String.format("y = %.3f - %.3f * x", a, -b));
				} else if (b > 0) {
					equation = new Text(width / 2, height - 15, String.format("y = %.3f + %.3f * x", a, b));
				} else {
					equation = new Text(width / 2, height - 15, String.format("y = %.3f", a));
				}
				c.add(equation);
			}
			c.suspend(false);
		} catch (final Exception e) { // Ignore
		}
	}
}
