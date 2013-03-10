package org.cpntools.simulator.extensions.graphics.charts;

/**
 * @author michael
 */
public class Bar {
	private String name;

	private int value;

	BarChartable parent;

	/**
	 * @param name
	 * @param value
	 */
	public Bar(final String name, final int value) {
		super();
		setName(name);
		setValue(value);
	}

	/**
	 * 
	 */
	public void delete() {
		if (parent != null) {
			parent.delete(this);
		}
	}

	/**
	 * @return
	 */
	public String getName() {
		return name;
	}

	/**
	 * @return
	 */
	public int getValue() {
		return value;
	}

	/**
	 * @param name
	 */
	public void setName(final String name) {
		this.name = name;
		if (parent != null) {
			parent.changed(this);
		}
	}

	/**
	 * @param value
	 */
	public void setValue(final int value) {
		this.value = value;
		if (parent != null) {
			parent.changed(this);
		}
	}

	void setParent(final BarChartable parent) {
		this.parent = parent;
	}
}
