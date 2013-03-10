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
public class Canvas extends Composite implements Observer {
	private final Channel c;

	private boolean suspended = false;
	private final List<Element> added = new ArrayList<Element>();
	private final Set<String> moved = new HashSet<String>();
	private final Set<String> styled = new HashSet<String>();
	private final Set<String> deleted = new HashSet<String>();
	private final Set<String> subscribed = new HashSet<String>();

	public void suspend(final boolean suspended) throws Exception {
		if (suspended == this.suspended) { return; }
		this.suspended = suspended;
		if (!suspended) {
			for (final Element elm : added) {
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

	public Canvas(final Channel c, final String name) throws Exception {
		this(c, name, true);
	}

	public Canvas(final Channel c, final String name, final boolean raise) throws Exception {
		this(c, name, raise, true);
	}

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

	@Override
	public void update(final Observable arg0, final Object arg1) {
		// TODO Auto-generated method stub
	}

	@Override
	public void add(final Element element) throws Exception {
		if (suspended) {
			added.add(element);
		} else {
			if (element instanceof Node) {
				final Node n = (Node) element;
				element.setId(add((Node) element, true));
				if (n.getForeground() != Color.WHITE || n.getBackground() != Color.BLACK || n.getWidth() != 1) {
					style(n);
				}
			}
			super.add(element);
			if (element instanceof Line) {
				moved(element);
			}
		}
	}

	@Override
	public void remove(final Element element) throws Exception {
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
	}

	private void remove(final String id) throws Exception {
		Packet p = new Packet(3, 6);
		p.addBoolean(true);
		p.addString(id);
		p = c.send(p);
		p.reset();
		if (p.getInteger() != 1) { throw new Exception("Could not remove element " + id); }
	}

	public void center(final boolean raise) throws Exception {
		center(true, raise);
	}

	public void center() throws Exception {
		center(true);
	}

	public void center(final boolean zoom, final boolean raise) throws Exception {
		Packet p = new Packet(3, 9);
		p.addString(getId());
		p.addBoolean(zoom);
		p.addBoolean(raise);
		p = c.send(p);
		p.reset();
		if (p.getInteger() != 1) { throw new Exception("Could not center"); }
	}

	private String add(final Node element, final boolean b) throws Exception {
		final Packet p = c.send(element.getCreatePackage(getId()));
		p.reset();
		if (p.getInteger() != 1) { throw new Exception("Error adding element"); }
		return p.getString();
	}

	@Override
	void moved(final Element element) throws Exception {
		if (element instanceof Node && elements.containsKey(element.getId())) {
			if (suspended) {
				moved.add(element.getId());
			} else {
				moved(element.getId(), (Node) element);
			}
		}
	}

	private void moved(final String id, final Node node) throws Exception {
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

	@Override
	void style(final Node element) throws Exception {
		if (elements.containsKey(element.getId()) || added.contains(element)) {
			if (suspended) {
				styled.add(element.getId());
			} else {
				style(element.getId(), element.getForeground(), element.getBackground(), element.getLineWidth());
			}
		}

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

}