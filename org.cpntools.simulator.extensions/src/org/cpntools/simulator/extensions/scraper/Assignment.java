package org.cpntools.simulator.extensions.scraper;

/**
 * @author michael
 */
public class Assignment {
	private final String port;
	private final Place socket;
	private final Subpage parent;

	/**
	 * @param parent
	 * @param port
	 * @param socket
	 */
	public Assignment(final Subpage parent, final String port, final Place socket) {
		this.parent = parent;
		this.port = port;
		this.socket = socket;
	}

	/**
	 * @return
	 */
	public String getPort() {
		return port;
	}

	/**
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + (port == null ? 0 : port.hashCode());
		result = prime * result + (socket == null ? 0 : socket.hashCode());
		return result;
	}

	/**
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(final Object obj) {
		if (this == obj) { return true; }
		if (obj == null) { return false; }
		if (!(obj instanceof Assignment)) { return false; }
		final Assignment other = (Assignment) obj;
		if (port == null) {
			if (other.port != null) { return false; }
		} else if (!port.equals(other.port)) { return false; }
		if (socket == null) {
			if (other.socket != null) { return false; }
		} else if (!socket.equals(other.socket)) { return false; }
		return true;
	}

	/**
	 * @return
	 */
	public Place getSocket() {
		return socket;
	}

	/**
	 * @return
	 */
	public Subpage getParent() {
		return parent;
	}

}
