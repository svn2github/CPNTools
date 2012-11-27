package org.cpntools.simulator.extensions.declare;

/**
 * @author michael
 */
public class Task {
	private String name;

	public Task() {
	}

	public String getName() {
		return name;
	}

	public void setName(final String name) {
		this.name = name;
	}

	@Override
	public String toString() {
		if (name != null) { return name; }
		return super.toString();
	}

}
