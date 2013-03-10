package org.cpntools.simulator.extensions.graphics;

import java.awt.Point;
import java.awt.Rectangle;

/**
 * @author michael
 */
public abstract class Element {
	private String id;

	public String getId() {
		return id;
	}

	void setId(final String id) {
		assert this.id == null;
		this.id = id;
	}

	public Element(final Rectangle bounds) {
		this.bounds = new Rectangle(bounds);
	}

	public Point getPosition() {
		return bounds.getLocation();
	}

	protected int getX() {
		return (int) bounds.getCenterX();
	}

	protected int getY() {
		return -(int) bounds.getCenterY();
	}

	protected Composite owner;

	protected final Rectangle bounds;

	protected void updatePosition() throws Exception {
		if (owner != null) {
			owner.moved(this);
		}
	}

	public void setPosition(final Point position) throws Exception {
		bounds.setLocation(position);
		updatePosition();
	}

	public void move(final Point delta) throws Exception {
		setPosition(new Point((int) (bounds.getX() + delta.getX()), (int) (bounds.getY() + delta.getY())));
	}

}
