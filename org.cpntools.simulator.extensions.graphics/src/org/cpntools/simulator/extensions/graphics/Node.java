package org.cpntools.simulator.extensions.graphics;

import java.awt.Color;
import java.awt.Rectangle;

import org.cpntools.accesscpn.engine.protocol.Packet;

/**
 * @author michael
 * @param <T>
 */
public abstract class Node<T extends Node<T>> extends Element<T> {
	private int curvature;
	private Color fg, bg;
	private boolean observed;
	private boolean trace;
	private int width;
	protected boolean filled;

	/**
	 * @param bounds
	 */
	public Node(final Rectangle bounds) {
		super(bounds);
		fg = Color.BLACK;
		bg = Color.WHITE;
		width = 1;
		curvature = 0;
		filled = true;
	}

	/**
	 * @return
	 */
	public Color getBackground() {
		return bg;
	}

	/**
	 * @return
	 */
	public Rectangle getBounds() {
		return bounds;
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

	/**
	 * @return
	 */
	public int getCurvature() {
		return curvature;
	}

	/**
	 * @return
	 */
	public Color getForeground() {
		return fg;
	}

	/**
	 * @return
	 */
	public int getHeight() {
		return (int) bounds.getHeight();
	}

	/**
	 * @return
	 */
	public int getLineWidth() {
		return width;
	}

	/**
	 * @return
	 */
	public int getWidth() {
		return (int) bounds.getWidth();
	}

	/**
	 * @return
	 */
	public boolean isFilled() {
		return filled;
	}

	/**
	 * @return
	 */
	public boolean isSubscribed() {
		return observed;
	}

	/**
	 * @return
	 */
	public boolean isTracing() {
		return trace;
	}

	/**
	 * @param bg
	 * @return
	 * @throws Exception
	 */
	@SuppressWarnings("unchecked")
	public T setBackground(final Color bg) throws Exception {
		if (this.bg.equals(bg)) { return (T) this; }
		this.bg = bg;
		if (owner != null) {
			owner.style(this);
		}
		return (T) this;
	}

	/**
	 * @param newBounds
	 * @return
	 * @throws Exception
	 */
	@SuppressWarnings("unchecked")
	public T setBounds(final Rectangle newBounds) throws Exception {
		if (bounds.equals(newBounds)) { return (T) this; }
		bounds.setBounds(newBounds);
		updatePosition();
		return (T) this;
	}

	/**
	 * @param curvature
	 * @return
	 * @throws Exception
	 */
	@SuppressWarnings("unchecked")
	public T setCurvature(final int curvature) throws Exception {
		if (this.curvature == curvature) { return (T) this; }
		this.curvature = curvature;
		if (owner != null) {
			owner.style(this);
		}
		return (T) this;
	}

	/**
	 * @param filled
	 * @return
	 * @throws Exception
	 */
	@SuppressWarnings("unchecked")
	public T setFilled(final boolean filled) throws Exception {
		if (this.filled == filled) { return (T) this; }
		this.filled = filled;
		if (owner != null) {
			owner.style(this);
		}
		return (T) this;
	}

	/**
	 * @param fg
	 * @return
	 * @throws Exception
	 */
	@SuppressWarnings("unchecked")
	public T setForeground(final Color fg) throws Exception {
		if (this.fg.equals(fg)) { return (T) this; }
		this.fg = fg;
		if (owner != null) {
			owner.style(this);
		}
		return (T) this;
	}

	/**
	 * @param width
	 * @return
	 * @throws Exception
	 */
	@SuppressWarnings("unchecked")
	public T setLineWidth(final int width) throws Exception {
		if (this.width == width) { return (T) this; }
		this.width = width;
		if (owner != null) {
			owner.style(this);
		}
		return (T) this;
	}

	/**
	 * @param width
	 * @param height
	 * @return
	 * @throws Exception
	 */
	@SuppressWarnings("unchecked")
	public T setSize(final int width, final int height) throws Exception {
		bounds.setSize(width, height);
		updatePosition();
		return (T) this;
	}

	/**
	 * @return
	 * @throws Exception
	 */
	@SuppressWarnings("unused")
	private T subscribe() throws Exception {
		return subscribe(false);
	}

	/**
	 * @param trace
	 * @return
	 * @throws Exception
	 */
	@SuppressWarnings("unchecked")
	private T subscribe(@SuppressWarnings("hiding") final boolean trace) throws Exception {
		this.trace = trace;
		observed = true;
		if (owner != null) {
			owner.subscribe(this);
		}
		return (T) this;
	}

	void forceNotify() {
		setChanged();
		notifyObservers();
	}

}
