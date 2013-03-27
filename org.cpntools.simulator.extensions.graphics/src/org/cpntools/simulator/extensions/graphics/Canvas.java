package org.cpntools.simulator.extensions.graphics;

import java.awt.Color;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Observable;
import java.util.Observer;

import org.cpntools.accesscpn.engine.protocol.Packet;
import org.cpntools.simulator.extensions.Channel;
import org.cpntools.simulator.extensions.Command;
import org.cpntools.simulator.extensions.SubscriptionHandler;

/**
 * @author michael
 */
public class Canvas extends Composite<Canvas> implements Observer, SubscriptionHandler {
	private final List<Element<?>> added = new ArrayList<Element<?>>();

	private final Channel c;
	private final List<Element<?>> deleted = new ArrayList<Element<?>>();
	private final List<Element<?>> moved = new ArrayList<Element<?>>();
	private final List<Element<?>> styled = new ArrayList<Element<?>>();
	private boolean subscribed;

	private final Map<String, Node<?>> subscriptions = new HashMap<String, Node<?>>();

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
	 * @see org.cpntools.simulator.extensions.SubscriptionHandler#getIdentifier()
	 */
	@Override
	public int getIdentifier() {
		return 10006;
	}

	/**
	 * @see org.cpntools.simulator.extensions.SubscriptionHandler#handle(org.cpntools.accesscpn.engine.protocol.Packet,
	 *      org.cpntools.accesscpn.engine.protocol.Packet)
	 */
	@Override
	public Packet handle(final Packet p, final Packet response) {
		p.reset();
		if (p.getInteger() == 10000 && p.getInteger() == 401) { // Command = 10000, subcmd = 401
			final String id = p.getString();
			final Node<?> n = subscriptions.get(id);
			if (n != null) {
				final int x = p.getInteger();
				final int y = p.getInteger();
				if (n instanceof Rectangle || n instanceof Ellipse) {
					final int w = p.getInteger();
					final int h = p.getInteger();
					n.bounds.setBounds(x - w / 2, -(y + h / 2), w, h);
				} else {
					n.bounds.setLocation(x, -y);
				}
				if (n instanceof Text) {
					final String text = p.getString();
					final Text t = (Text) n;
					t.setText(text);
				}
				n.forceNotify();
			}
		}
		System.out.println("Done");
		return null;
	}

	/**
	 * @see org.cpntools.simulator.extensions.SubscriptionHandler#prefilter(org.cpntools.accesscpn.engine.protocol.Packet)
	 */
	@Override
	public Packet prefilter(final Packet request) {
		return null;
	}

	/**
	 * @see org.cpntools.simulator.extensions.graphics.Composite#remove(org.cpntools.simulator.extensions.graphics.Element)
	 */
	@Override
	public <U extends Element<?>> U remove(final U element) throws Exception {
		subscriptions.remove(element.getId());
		if (suspended) {
			deleted.add(element);
		} else {
			if (element instanceof Node) {
				remove(element.getId());
			} else if (element instanceof Composite) {
				@SuppressWarnings("hiding")
				final Composite<?> c = (Composite<?>) element;
				for (final Element<?> e : c.elements.values()) {
					remove(e);
				}
			}
			super.remove(element);
		}
		return element;
	}

	/**
	 * @param suspended
	 * @return
	 * @throws Exception
	 */
	public boolean suspend(@SuppressWarnings("hiding") final boolean suspended) throws Exception {
		if (suspended == this.suspended) { return suspended; }
		this.suspended = suspended;
		if (!suspended) {
			for (final Element<?> elm : added) {
				addToRoot(elm);
			}
			for (final Element<?> elm : added) {
				if (elm instanceof Composite) {
					((Composite<?>) elm).hoist();
				}
			}
			added.clear();
			for (final Element<?> id : moved) {
				moved(id);
			}
			moved.clear();
			for (final Element<?> id : styled) {
				style(id);
			}
			styled.clear();
			for (final Element<?> id : deleted) {
				remove(id);
			}
			deleted.clear();
		}
		return !suspended; // We return the parameter to make this thread-safe
	}

	/**
	 * @see java.util.Observer#update(java.util.Observable, java.lang.Object)
	 */
	@Override
	public void update(final Observable arg0, final Object arg1) {
		// TODO Auto-generated method stub
	}

	private String addToRoot(final Node<?> element, final boolean b) throws Exception {
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

	/**
	 * @see org.cpntools.simulator.extensions.graphics.Composite#add(org.cpntools.simulator.extensions.graphics.Element)
	 */
	@Override
	<U extends Element<?>> U addToRoot(final U element) throws Exception {
		if (suspended) {
			added.add(element);
		} else {
			if (element instanceof Node) {
				final Node<?> n = (Node<?>) element;
				element.setId(addToRoot((Node<?>) element, true));
				if (n.getForeground() != Color.WHITE || n.getBackground() != Color.BLACK || n.getWidth() != 1) {
					style(n);
				}
				if (n.isSubscribed()) {
					subscribe(n);
				}
			} else {
				super.addToRoot(element);
			}
		}
		return element;
	}

	@Override
	void moved(final Element<?> element) throws Exception {
		if (element instanceof Node) {
			if (suspended) {
				moved.add(element);
			} else {
				moved(element.getId(), (Node<?>) element);
			}
		}
	}

	@Override
	void style(final Element<?> element) throws Exception {
		if (suspended) {
			styled.add(element);
		} else {
			if (element instanceof Node) {
				final Node<?> n = (Node<?>) element;
				style(n.getId(), n.getForeground(), n.getBackground(), n.getLineWidth());
			} else if (element instanceof Composite) {
				@SuppressWarnings("hiding")
				final Composite<?> c = (Composite<?>) element;
				for (final Element<?> e : c.elements.values()) {
					style(e);
				}
			}
		}
	}

	@Override
	void subscribe(final Node<?> node) throws Exception {
		if (!subscribed) {
			subscribed = true;
			c.subscribe(new Command(10000, 401), this);
		}
		Packet p = new Packet(3, 7);
		p.addString(node.getId());
		p.addBoolean(node.isTracing());
		p = c.send(p);
		p.reset();
		if (p.getInteger() != 1) { throw new Exception("Could not subscribe to updates from element " + node.getId()); }
		subscriptions.put(node.getId(), node);
	}
}
