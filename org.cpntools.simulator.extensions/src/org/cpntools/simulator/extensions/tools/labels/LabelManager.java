package org.cpntools.simulator.extensions.tools.labels;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.Observable;
import java.util.Observer;

import org.cpntools.accesscpn.engine.protocol.Packet;
import org.cpntools.simulator.extensions.Channel;
import org.cpntools.simulator.extensions.scraper.Element;
import org.cpntools.simulator.extensions.scraper.Scraper;

/**
 * @author michael
 */
public class LabelManager implements Observer {
	private final Map<String, Map<Object, Label>> labels = new HashMap<String, Map<Object, Label>>();

	private final Channel c;

	/**
	 * @param c
	 */
	public LabelManager(final Channel c) {
		this.c = c;
	}

	private boolean subscribed = false;

	private void subscribe() {
		if (subscribed) { return; }
		subscribed = true;
		final Scraper scraper = c.getExtension(Scraper.class);
		if (scraper != null) {
			scraper.addObserver(this);
		}
	}

	/**
	 * @param label
	 * @throws Exception
	 */
	public void delete(final Label label) throws Exception {
		delete(label.getOwnerId(), label.getNamespace());
	}

	/**
	 * @param element
	 * @param namespace
	 * @throws Exception
	 */
	public void delete(final Element element, final Object namespace) throws Exception {
		delete(element.getId(), namespace);
	}

	/**
	 * @param ownerId
	 * @param namespace
	 * @throws Exception
	 */
	public synchronized void delete(final String ownerId, final Object namespace) throws Exception {
		final Map<Object, Label> forElement = labels.get(ownerId);
		if (forElement == null) { return; }
		final Label oldLabel = forElement.remove(namespace);
		if (oldLabel == null) { return; }
		delete(oldLabel.getId());
	}

	private void delete(final String id) throws Exception {
		Packet p = new Packet(3, 11);
		p.addInteger(1);
		p.addString(id);
		p = c.send(p);
		p.reset();
		if (p.getInteger() != 1) { throw new Exception("Simulator returned unexpected value:" + p); }
	}

	/**
	 * @param element
	 * @param text
	 * @return
	 * @throws Exception
	 */
	public Label add(final Element element, final String text) throws Exception {
		return add(element, text, Position.NW);
	}

	/**
	 * @param element
	 * @param text
	 * @param position
	 * @return
	 * @throws Exception
	 */
	public Label add(final Element element, final String text, final Position position) throws Exception {
		return add(element, text, text, position);
	}

	/**
	 * @param element
	 * @param namespace
	 * @param text
	 * @return
	 * @throws Exception
	 */
	public Label add(final Element element, final Object namespace, final String text) throws Exception {
		return add(element, namespace, text, Position.NW);
	}

	/**
	 * @param element
	 * @param namespace
	 * @param text
	 * @param position
	 * @return
	 * @throws Exception
	 */
	public Label add(final Element element, final Object namespace, final String text, final Position position)
	        throws Exception {
		subscribe();
		synchronized (this) {
			Map<Object, Label> forElement = labels.get(element.getId());
			if (forElement == null) {
				forElement = new HashMap<Object, Label>();
				labels.put(element.getId(), forElement);
			}
			final Label oldLabel = forElement.remove(namespace);
			if (oldLabel != null) {
				if (oldLabel.getText().equals(text) && oldLabel.getPosition() == position) {
					forElement.put(namespace, oldLabel);
					return oldLabel;
				}
				delete(oldLabel.getId());
			}

			final String id = createLabel(element.getId(), text, position);
			final Label l = new Label(this, element.getId(), namespace, id, text, position);
			forElement.put(namespace, l);
			return l;
		}
	}

	private String createLabel(final String ownerId, final String text, final Position position) throws IOException,
	        Exception {
		Packet p = new Packet(3, 10);
		p.addInteger(1);
		p.addInteger(position.getId());
		p.addString(ownerId);
		p.addString(text);
		p = c.send(p);
		p.reset();
		if (p.getInteger() != 1) { throw new Exception("Simulator returned unexpected value:" + p); }
		if (!p.getBoolean()) { throw new Exception(p.getString()); }
		final String id = p.getString();
		return id;
	}

	synchronized void replace(final Label label) throws Exception {
		delete(label.getId());
		label.setId(createLabel(label.getOwnerId(), label.getText(), label.getPosition()));
	}

	/**
	 * @see java.util.Observer#update(java.util.Observable, java.lang.Object)
	 */
	@Override
	public void update(final Observable arg0, final Object arg1) {
		if (arg0 instanceof Scraper && arg1 instanceof Scraper.Removed) {
			synchronized (this) {
// labels.remove(r.getElm().getId());
			}
		}

	}
}
