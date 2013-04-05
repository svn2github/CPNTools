package org.cpntools.simulator.extensions.scraper;

/**
 * @author michael
 */
public class HasName extends Element {

	private String name;

	/**
	 * @param dictionary
	 * @param id
	 * @param name
	 */
	public HasName(final ElementDictionary dictionary, final String id, final String name) {
		super(dictionary, id);
		setName(name);
	}

	/**
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(final Object obj) {
		if (this == obj) { return true; }
		if (!super.equals(obj)) { return false; }
		if (!(obj instanceof HasName)) { return false; }
		final HasName other = (HasName) obj;
		if (name == null) {
			if (other.name != null) { return false; }
		} else if (!name.equals(other.name)) { return false; }
		return true;
	}

	/**
	 * @return
	 */
	public String getName() {
		return name;
	}

	/**
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + (name == null ? 0 : name.hashCode());
		return result;
	}

	/**
	 * @param name
	 * @return
	 */
	public boolean setName(final String name) {
		if (name == null) {
			if (this.name == null) { return false; }
			this.name = name;
			return true;
		}
		if (name.equals(this.name)) { return false; }
		this.name = name;
		return true;
	}

	/**
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return getClass().getSimpleName() + ": " + getName() + " [" + getId() + "]";
	}

}
