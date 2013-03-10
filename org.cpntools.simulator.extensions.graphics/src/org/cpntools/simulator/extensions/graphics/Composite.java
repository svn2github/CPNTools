package org.cpntools.simulator.extensions.graphics;

import java.awt.Point;
import java.awt.Rectangle;
import java.util.HashMap;
import java.util.Map;

/**
 * @author michael
 */
public abstract class Composite extends Element {
	protected final Map<String, Node> elements = new HashMap<String, Node>();

	public Composite() {
		super(new Rectangle(0, 0, 0, 0));
	}

	void moved(final Element element) throws Exception {
		if (owner != null) {
			owner.moved(element);
		}
	}

	@Override
	public void setPosition(final Point position) throws Exception {
		move(new Point((int) (position.getX() - bounds.getX()), (int) (position.getY() - bounds.getY())));
	}

	@Override
	public void move(final Point delta) throws Exception {
		for (final Element e : elements.values()) {
			e.move(delta);
		}
	}

	void style(final Node node) throws Exception {
		if (owner != null) {
			owner.style(node);
		}
	}

	public void add(final Element node) throws Exception {
		node.owner = this;
		if (node instanceof Composite) {
			for (final Node n : ((Composite) node).elements.values()) {
				add(n);
			}
		} else {
			elements.put(node.getId(), (Node) node);
			if (owner != null) {
				owner.add(node);
			}
		}
	}

	public void remove(final Element element) throws Exception {
		if (element instanceof Composite) {
			for (final Node n : ((Composite) element).elements.values()) {
				remove(n);
			}
		} else {
			if (owner != null) {
				owner.remove(element);
			}
			elements.remove(element.getId());
		}
		element.owner = null;
	}
}
