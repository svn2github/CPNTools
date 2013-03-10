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
	private final List<Point> points = new ArrayList<Point>();

	/**
	 * @param points
	 */
	public Line(final Iterable<Point> points) {
		super(new Rectangle(points.iterator().next()));
		for (final Point p : points) {
			this.points.add(new Point((int) p.getX() - getX(), (int) p.getY() + getY()));
		}
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
		for (final Point point : points) {
			p.addInteger((int) point.getX());
			p.addInteger(-(int) point.getY());
		}
		return p;
	}

	@Override
	protected int getX() {
		return (int) bounds.getX();
	}

	@Override
	protected int getY() {
		return -(int) bounds.getY();
	}

}
