package org.cpntools.simulator.extensions.graphics;

import java.awt.Rectangle;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

/**
 * @author michael
 * @param <T>
 */
public abstract class Composite<T extends Composite<T>> extends Element<T> {
	private final List<Element<?>> added = new ArrayList<Element<?>>();
	protected final Map<String, Element<?>> elements = new HashMap<String, Element<?>>();

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
		addToRoot(node);
		if (node.getId() != null) {
			elements.put(node.getId(), node);
		} else {
			added.add(node);
		}
		return node;
	}

	/**
	 * @param element
	 * @return
	 * @throws Exception
	 */
	public <U extends Element<?>> U remove(final U element) throws Exception {
		if (elements.remove(element.getId()) == null) {
			added.remove(element);
		}
		if (owner != null) {
			owner.remove(element);
		}
		element.owner = null;
		return element;
	}

	<U extends Element<?>> U addToRoot(final U node) throws Exception {
		if (node instanceof Composite) {
			for (final Element<?> element : ((Composite<?>) node).elements.values()) {
				addToRoot(element);
			}
			for (final Element<?> element : ((Composite<?>) node).added) {
				addToRoot(element);
			}
		} else {
			if (owner != null) { return owner.addToRoot(node); }
		}
		return node;
	}

	void hoist() {
		for (final Iterator<Element<?>> i = added.iterator(); i.hasNext();) {
			final Element<?> n = i.next();
			if (n.getId() != null) {
				elements.put(n.getId(), n);
				i.remove();
			}
			if (n instanceof Composite) {
				((Composite<?>) n).hoist();
			}
		}
	}

	void moved(final Element<?> element) throws Exception {
		if (owner != null) {
			owner.moved(element);
		}
	}

	void style(final Element<?> element) throws Exception {
		if (owner != null) {
			owner.style(element);
		}
	}

	void subscribe(final Node<?> node) throws Exception {
		if (owner != null) {
			owner.subscribe(node);
		}
	}
}
