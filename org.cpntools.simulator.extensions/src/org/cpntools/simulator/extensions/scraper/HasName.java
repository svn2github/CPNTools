package org.cpntools.simulator.extensions.scraper;

/**
 * @author michael
 */
public class HasName extends Element {

	private String name;

	/**
	 * @param id
	 * @param name
	 */
	public HasName(final String id, final String name) {
		super(id);
		setName(name);
	}

	/**
	 * @return
	 */
	public String getName() {
		return name;
	}

	public boolean setName(final String name) {
		if (name.equals(this.name)) { return false; }
		this.name = name;
		return true;
	}

}
