package org.cpntools.simulator.extensions.graphics;

import java.awt.Point;
import java.awt.Rectangle;
import java.util.Observable;

/**
 * @author michael
 * @param <T>
 */
public abstract class Element<T extends Element<T>> extends Observable {
	private String id;

	protected final Rectangle bounds;

	protected Composite<? extends Composite<?>> owner;

	/**
	 * @param bounds
	 */
	public Element(final Rectangle bounds) {
		this.bounds = new Rectangle(bounds);
	}

	/**
	 * @return
	 */
	public String getId() {
		return id;
	}

	/**
	 * @return
	 */
	public Point getPosition() {
		return bounds.getLocation();
	}

	/**
	 * @param delta
	 * @return
	 * @throws Exception
	 */
	public T move(final Point delta) throws Exception {
		return setPosition(new Point((int) (bounds.getX() + delta.getX()), (int) (bounds.getY() + delta.getY())));
	}

	/**
	 * @param position
	 * @return
	 * @throws Exception
	 */
	@SuppressWarnings("unchecked")
	public T setPosition(final Point position) throws Exception {
		bounds.setLocation(position);
		updatePosition();
		return (T) this;
	}

	protected int getX() {
		return (int) bounds.getCenterX();
	}

	protected int getY() {
		return -(int) bounds.getCenterY();
	}

	protected void updatePosition() throws Exception {
		if (owner != null) {
			owner.moved(this);
		}
	}

	@SuppressWarnings("unchecked")
	T setId(final String id) {
		assert this.id == null;
		this.id = id;
		return (T) this;
	}

}
