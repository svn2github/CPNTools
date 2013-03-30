package org.cpntools.simulator.extensions.tools.labels;

/**
 * @author michael
 */
public class Label {
	private final LabelManager manager;
	private String id;
	private String text;
	private final String ownerId;
	private final Object namespace;
	private Position position;

	/**
	 * @param manager
	 * @param ownerId
	 * @param namespace
	 * @param id
	 * @param text
	 * @param position
	 */
	public Label(final LabelManager manager, final String ownerId, final Object namespace, final String id,
	        final String text, final Position position) {
		this.manager = manager;
		this.ownerId = ownerId;
		this.namespace = namespace;
		this.position = position;
		this.id = id;
		this.text = text;
	}

	/**
	 * 
	 */
	public void delete() {
		try {
			manager.delete(this);
		} catch (final Exception e) { // Ignore
		}
	}

	/**
	 * @return
	 */
	public String getText() {
		return text;
	}

	/**
	 * @param text
	 */
	public void setText(final String text) {
		this.text = text;
		try {
			manager.replace(this);
		} catch (final Exception _) { // Ignore
		}
	}

	/**
	 * @return
	 */
	public String getOwnerId() {
		return ownerId;
	}

	/**
	 * @return
	 */
	public Object getNamespace() {
		return namespace;
	}

	/**
	 * @return
	 */
	public String getId() {
		return id;
	}

	void setId(final String id) {
		this.id = id;
	}

	/**
	 * @return
	 */
	public Position getPosition() {
		return position;
	}

	/**
	 * @param position
	 */
	public void setPosition(final Position position) {
		this.position = position;
		try {
			manager.replace(this);
		} catch (final Exception _) { // Ignore
		}
	}
}
