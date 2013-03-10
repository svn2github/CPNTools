package org.cpntools.simulator.extensions.graphics;

import java.awt.Color;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Observable;
import java.util.Observer;
import java.util.Set;

import org.cpntools.accesscpn.engine.protocol.Packet;
import org.cpntools.simulator.extensions.Channel;

/**
 * @author michael
 */
public class Canvas extends Composite<Canvas> implements Observer {
	private final List<Element<?>> added = new ArrayList<Element<?>>();

	private final Channel c;
	private final Set<String> deleted = new HashSet<String>();
	private final Set<String> moved = new HashSet<String>();
	private final Set<String> styled = new HashSet<String>();
	private boolean suspended = false;

	/**
	 * @param c
	 * @param name
	 * @throws Exception
	 */
	public Canvas(final Channel c, final String name) throws Exception {
		this(c, name, true);
	}

	/**
	 * @param c
	 * @param name
	 * @param raise
	 * @throws Exception
	 */
	public Canvas(final Channel c, final String name, final boolean raise) throws Exception {
		this(c, name, raise, true);
	}

	/**
	 * @param c
	 * @param name
	 * @param raise
	 * @param canvas
	 * @throws Exception
	 */
	public Canvas(final Channel c, final String name, final boolean raise, final boolean canvas) throws Exception {
		this.c = c;

		Packet p = new Packet(3, 2);
		p.addString(name);
		p.addBoolean(!canvas);
		p.addBoolean(raise);
		p = c.send(p);
		p.reset();
		if (p.getInteger() != 1) { throw new Exception("Could not create canvas"); }
		setId(p.getString());
	}

	/**
	 * @see org.cpntools.simulator.extensions.graphics.Composite#add(org.cpntools.simulator.extensions.graphics.Element)
	 */
	@Override
	public <U extends Element<?>> U add(final U element) throws Exception {
		if (suspended) {
			added.add(element);
		} else {
			if (element instanceof Node) {
				final Node<?> n = (Node<?>) element;
				element.setId(add((Node<?>) element, true));
				if (n.getForeground() != Color.WHITE || n.getBackground() != Color.BLACK || n.getWidth() != 1) {
					style(n);
				}
				if (n.isSubscribed()) {
					subscribe(n);
				}
			}
			super.add(element);
			if (element instanceof Line) {
				moved(element);
			}
		}
		return element;
	}

	/**
	 * @throws Exception
	 */
	public void center() throws Exception {
		center(true);
	}

	/**
	 * @param raise
	 * @throws Exception
	 */
	public void center(final boolean raise) throws Exception {
		center(true, raise);
	}

	/**
	 * @param zoom
	 * @param raise
	 * @throws Exception
	 */
	public void center(final boolean zoom, final boolean raise) throws Exception {
		Packet p = new Packet(3, 9);
		p.addString(getId());
		p.addBoolean(zoom);
		p.addBoolean(raise);
		p = c.send(p);
		p.reset();
		if (p.getInteger() != 1) { throw new Exception("Could not center"); }
	}

	/**
	 * @see org.cpntools.simulator.extensions.graphics.Composite#remove(org.cpntools.simulator.extensions.graphics.Element)
	 */
	@Override
	public <U extends Element<?>> U remove(final U element) throws Exception {
		if (elements.containsKey(element.getId()) || added.contains(element)) {
			if (suspended) {
				deleted.add(element.getId());
			} else {
				super.remove(element);
				if (element instanceof Node) {
					remove(element.getId());
				}
			}
		}
		return element;
	}

	/**
	 * @param suspended
	 * @throws Exception
	 */
	public void suspend(@SuppressWarnings("hiding") final boolean suspended) throws Exception {
		if (suspended == this.suspended) { return; }
		this.suspended = suspended;
		if (!suspended) {
			for (final Element<?> elm : added) {
				add(elm);
			}
			added.clear();
			for (final String id : moved) {
				moved(elements.get(id));
			}
			moved.clear();
			for (final String id : styled) {
				style(elements.get(id));
			}
			styled.clear();
			for (final String id : deleted) {
				remove(elements.get(id));
			}
			deleted.clear();
		}
	}

	/**
	 * @see java.util.Observer#update(java.util.Observable, java.lang.Object)
	 */
	@Override
	public void update(final Observable arg0, final Object arg1) {
		// TODO Auto-generated method stub
	}

	private String add(final Node<?> element, final boolean b) throws Exception {
		final Packet p = c.send(element.getCreatePackage(getId()));
		p.reset();
		if (p.getInteger() != 1) { throw new Exception("Error adding element"); }
		return p.getString();
	}

	private void moved(final String id, final Node<?> node) throws Exception {
		Packet p = new Packet(3, 5);
		p.addString(id);
		p.addInteger(node.getX());
		p.addInteger(node.getY());
		p.addInteger(node.getWidth());
		p.addInteger(node.getHeight());
		p = c.send(p);
		p.reset();
		if (p.getInteger() != 1) { throw new Exception("Could not move element " + id); }
	}

	private void remove(final String id) throws Exception {
		Packet p = new Packet(3, 6);
		p.addBoolean(true);
		p.addString(id);
		p = c.send(p);
		p.reset();
		if (p.getInteger() != 1) { throw new Exception("Could not remove element " + id); }
	}

	private void style(final String id, final Color foreground, final Color background, final int width)
	        throws Exception {
		Packet p = new Packet(3, 4);
		p.addString(id);
		p.addInteger(foreground.getRed());
		p.addInteger(foreground.getGreen());
		p.addInteger(foreground.getBlue());
		p.addInteger(background.getRed());
		p.addInteger(background.getGreen());
		p.addInteger(background.getBlue());
		p.addInteger(width);
		p = c.send(p);
		p.reset();
		if (p.getInteger() != 1) { throw new Exception("Could not style element " + id); }
	}

	@Override
	void moved(final Element<?> element) throws Exception {
		if (element instanceof Node && elements.containsKey(element.getId())) {
			if (suspended) {
				moved.add(element.getId());
			} else {
				moved(element.getId(), (Node<?>) element);
			}
		}
	}

	@Override
	void style(final Node<?> element) throws Exception {
		if (elements.containsKey(element.getId()) || added.contains(element)) {
			if (suspended) {
				styled.add(element.getId());
			} else {
				style(element.getId(), element.getForeground(), element.getBackground(), element.getLineWidth());
			}
		}

	}

	@Override
	void subscribe(final Node<?> node) throws Exception {
		Packet p = new Packet(3, 7);
		p.addString(node.getId());
		p.addBoolean(node.isTracing());
		p = c.send(p);
		p.reset();
		if (p.getInteger() != 1) { throw new Exception("Could not subscribe to updates from element " + node.getId()); }
	}
}
