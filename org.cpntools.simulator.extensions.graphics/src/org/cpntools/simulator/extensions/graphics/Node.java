package org.cpntools.simulator.extensions.graphics;

import java.awt.Color;
import java.awt.Rectangle;

import org.cpntools.accesscpn.engine.protocol.Packet;

/**
 * @author michael
 */
public abstract class Node extends Element {
	private Color fg, bg;
	private int width;

	public Node(final Rectangle bounds) {
		super(bounds);
		fg = Color.BLACK;
		bg = Color.WHITE;
		width = 1;
	}

	public Rectangle getBounds() {
		return bounds;
	}

	public int getWidth() {
		return (int) bounds.getWidth();
	}

	/**
	 * @return
	 */
	public int getHeight() {
		return (int) bounds.getHeight();
	}

	/**
	 * @param newBounds
	 * @throws Exception
	 */
	public void setBounds(final Rectangle newBounds) throws Exception {
		if (bounds.equals(newBounds)) { return; }
		bounds.setBounds(newBounds);
		updatePosition();
	}

	public void setSize(final int width, final int height) throws Exception {
		bounds.setSize(width, height);
		updatePosition();
	}

	/**
	 * @param canvasid
	 * @return
	 */
	public Packet getCreatePackage(final String canvasid) {
		final Packet p = new Packet(3, 3);
		p.addString(canvasid);
		return p;
	}

	public Color getForeground() {
		return fg;
	}

	public void setForeground(final Color fg) throws Exception {
		if (this.fg.equals(fg)) { return; }
		this.fg = fg;
		if (owner != null) {
			owner.style(this);
		}
	}

	public Color getBackground() {
		return bg;
	}

	public void setBackground(final Color bg) throws Exception {
		if (this.bg.equals(bg)) { return; }
		this.bg = bg;
		if (owner != null) {
			owner.style(this);
		}
	}

	public int getLineWidth() {
		return width;
	}

	public void setLineWidth(final int width) throws Exception {
		if (this.width == width) { return; }
		this.width = width;
		if (owner != null) {
			owner.style(this);
		}
	}
}
