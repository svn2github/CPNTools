package org.cpntools.simulator.extensions.graphics;

import java.awt.Point;
import java.awt.Rectangle;
import java.util.HashMap;
import java.util.Map;

/**
 * @author michael
 * @param <T>
 */
public abstract class Composite<T extends Composite<T>> extends Element<T> {
	protected final Map<String, Node<?>> elements = new HashMap<String, Node<?>>();

	/**
	 * 
	 */
	public Composite() {
		super(new Rectangle(0, 0, 0, 0));
	}

	/**
	 * @param node
	 * @return
	 * @throws Exception
	 */
	public <U extends Element<?>> U add(final U node) throws Exception {
		node.owner = this;
		if (node instanceof Composite) {
			for (final Node<?> n : ((Composite<?>) node).elements.values()) {
				add(n);
			}
		} else {
			elements.put(node.getId(), (Node<?>) node);
			if (owner != null) {
				owner.add(node);
			}
		}
		return node;
	}

	/**
	 * @see org.cpntools.simulator.extensions.graphics.Element#move(java.awt.Point)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public T move(final Point delta) throws Exception {
		for (final Element<?> e : elements.values()) {
			e.move(delta);
		}
		return (T) this;
	}

	/**
	 * @param element
	 * @return
	 * @throws Exception
	 */
	public <U extends Element<?>> U remove(final U element) throws Exception {
		if (element instanceof Composite) {
			for (final Node<?> n : ((Composite<?>) element).elements.values()) {
				remove(n);
			}
		} else {
			if (owner != null) {
				owner.remove(element);
			}
			elements.remove(element.getId());
		}
		element.owner = null;
		return element;
	}

	/**
	 * @see org.cpntools.simulator.extensions.graphics.Element#setPosition(java.awt.Point)
	 */
	@Override
	public T setPosition(final Point position) throws Exception {
		return move(new Point((int) (position.getX() - bounds.getX()), (int) (position.getY() - bounds.getY())));
	}

	void moved(final Element<?> element) throws Exception {
		if (owner != null) {
			owner.moved(element);
		}
	}

	void style(final Node<?> node) throws Exception {
		if (owner != null) {
			owner.style(node);
		}
	}

	void subscribe(final Node<?> node) throws Exception {
		if (owner != null) {
			owner.subscribe(node);
		}
	}
}
