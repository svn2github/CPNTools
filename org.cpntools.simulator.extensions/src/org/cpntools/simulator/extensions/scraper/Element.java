package org.cpntools.simulator.extensions.scraper;

/**
 * @author michael
 */
public abstract class Element {
	private String id;

	/**
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + (getId() == null ? 0 : getId().hashCode());
		return result;
	}

	/**
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(final Object obj) {
		if (this == obj) { return true; }
		if (obj == null) { return false; }
		if (!(obj instanceof Element)) { return false; }
		final Element other = (Element) obj;
		if (getId() == null) {
			if (other.getId() != null) { return false; }
		} else if (!getId().equals(other.getId())) { return false; }
		return true;
	}

	public Element(final String id) {
		setId(id);
	}

	public String getId() {
		return id;
	}

	/**
	 * @param id
	 */
	public void setId(final String id) {
		this.id = id;
	}
}
