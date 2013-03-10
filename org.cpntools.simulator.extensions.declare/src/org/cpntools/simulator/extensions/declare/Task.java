package org.cpntools.simulator.extensions.declare;

/**
 * @author michael
 */
public class Task {
	private String name;

	/**
	 * 
	 */
	public Task() {
	}

	/**
	 * @return
	 */
	public String getName() {
		return name;
	}

	/**
	 * @param name
	 */
	public void setName(final String name) {
		this.name = name;
	}

	/**
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		if (name != null) { return name; }
		return super.toString();
	}

}
