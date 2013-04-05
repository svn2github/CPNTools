package org.cpntools.simulator.extensions.scraper;

/**
 * @author michael
 */
public class Place extends Node {

	private String initMark;

	private String type;

	/**
	 * @param dictionary
	 * @param id
	 * @param name
	 * @param page
	 * @param type
	 * @param initMark
	 */
	public Place(final ElementDictionary dictionary, final String id, final String name, final Page page,
	        final String type, final String initMark) {
		super(dictionary, id, name, page);
		this.type = type;
		this.initMark = initMark;
	}

	/**
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(final Object obj) {
		if (this == obj) { return true; }
		if (!super.equals(obj)) { return false; }
		if (!(obj instanceof Place)) { return false; }
		final Place other = (Place) obj;
		if (initMark == null) {
			if (other.initMark != null) { return false; }
		} else if (!initMark.equals(other.initMark)) { return false; }
		if (type == null) {
			if (other.type != null) { return false; }
		} else if (!type.equals(other.type)) { return false; }
		return true;
	}

	/**
	 * @return
	 */
	public String getInitMark() {
		return initMark;
	}

	/**
	 * @return
	 */
	public String getType() {
		return type;
	}

	/**
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + (initMark == null ? 0 : initMark.hashCode());
		result = prime * result + (type == null ? 0 : type.hashCode());
		return result;
	}

	/**
	 * @param initMark
	 * @return
	 */
	public boolean setInitMark(final String initMark) {
		if (initMark.equals(this.initMark)) { return false; }
		this.initMark = initMark;
		return true;
	}

	/**
	 * @param type
	 * @return
	 */
	public boolean setType(final String type) {
		if (type.equals(this.type)) { return false; }
		this.type = type;
		return true;
	}
}