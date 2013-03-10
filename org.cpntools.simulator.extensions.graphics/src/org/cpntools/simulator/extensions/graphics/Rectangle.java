package org.cpntools.simulator.extensions.graphics;

import java.awt.Dimension;
import java.awt.Point;

import org.cpntools.accesscpn.engine.protocol.Packet;

/**
 * @author michael
 */
public class Rectangle extends Node {

	public Rectangle(final java.awt.Rectangle bounds) {
		super(bounds);
	}

	public Rectangle(final Point position, final Dimension size) {
		this(new java.awt.Rectangle(position, size));
	}

	public Rectangle(final int x, final int y, final int width, final int height) {
		this(new java.awt.Rectangle(x, y, width, height));
	}

	@Override
	public Packet getCreatePackage(final String canvasid) {
		final Packet p = super.getCreatePackage(canvasid);
		p.addInteger(1);
		p.addInteger(getX());
		p.addInteger(getY());
		p.addInteger(getWidth());
		p.addInteger(getHeight());
		return p;
	}
}
