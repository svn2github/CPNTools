package org.cpntools.simulator.extensions.graphics;

import java.awt.Point;
import java.awt.Rectangle;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.cpntools.accesscpn.engine.protocol.Packet;

/**
 * @author michael
 */
public class Line extends Node<Line> {
	private int lineStyle;
	private final List<Point> points = new ArrayList<Point>();

	/**
	 * @param points
	 */
	public Line(final Iterable<Point> points) {
		super(new Rectangle(points.iterator().next()));
		for (final Point p : points) {
			this.points.add(new Point((int) p.getX() - getX(), (int) p.getY() + getY()));
		}
		filled = false;
		lineStyle = 0;
	}

	/**
	 * @param points
	 */
	public Line(final Point... points) {
		this(Arrays.asList(points));
	}

	/**
	 * @see org.cpntools.simulator.extensions.graphics.Node#getCreatePackage(java.lang.String)
	 */
	@Override
	public Packet getCreatePackage(final String canvasid) {
		final Packet p = super.getCreatePackage(canvasid);
		p.addInteger(4);
		p.addInteger(points.size());
		int dx = 0, dy = 0;
		if (owner != null) {
			dx = getX();
			dy = getY();
		}
		for (final Point point : points) {
			p.addInteger(dx + (int) point.getX());
			p.addInteger(dy + -(int) point.getY());
		}
		p.addInteger(getCurvature());
		p.addInteger(getLineStyle());
		p.addBoolean(isFilled());
		return p;
	}

	/**
	 * @return
	 */
	public int getLineStyle() {
		return lineStyle;
	}

	/**
	 * @param lineStyle
	 * @return
	 * @throws Exception
	 */
	public Line setLineStyle(final int lineStyle) throws Exception {
		if (this.lineStyle == lineStyle) { return this; }
		this.lineStyle = lineStyle;
		if (owner != null) {
			owner.style(this);
		}
		return this;
	}

	@Override
	protected int getX() {
		if (owner == null) {
			return (int) bounds.getX();
		} else {
			return (int) bounds.getX() + owner.getX();
		}
	}

	@Override
	protected int getY() {
		if (owner == null) {
			return -(int) bounds.getY();
		} else {
			return -(int) bounds.getY() + owner.getY();
		}
	}

}
