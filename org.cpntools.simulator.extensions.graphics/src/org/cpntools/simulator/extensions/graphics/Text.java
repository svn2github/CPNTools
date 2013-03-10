package org.cpntools.simulator.extensions.graphics;

import java.awt.Dimension;
import java.awt.Point;

import org.cpntools.accesscpn.engine.protocol.Packet;

/**
 * @author michael
 */
public class Text extends Node {

	private final String text;

	public Text(final java.awt.Rectangle bounds, final String text) {
		super(bounds);
		this.text = text;
	}

	public Text(final Point position, final String text) {
		this(new java.awt.Rectangle(position, new Dimension()), text);
	}

	public Text(final int x, final int y, final String text) {
		this(new Point(x, y), text);
	}

	@Override
	public Packet getCreatePackage(final String canvasid) {
		final Packet p = super.getCreatePackage(canvasid);
		p.addInteger(3);
		p.addInteger(getX());
		p.addInteger(getY());
		p.addString(getText());
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

	public String getText() {
		return text;
	}

}
