package org.cpntools.simulator.extensions.graphics.charts;

/**
 * @author michael
 */
public class Bar {
	BarChartable parent;

	public Bar(final String name, final int value) {
		super();
		setName(name);
		setValue(value);
	}

	void setParent(final BarChartable parent) {
		this.parent = parent;
	}

	private String name;
	private int value;

	public String getName() {
		return name;
	}

	public int getValue() {
		return value;
	}

	public void setName(final String name) {
		this.name = name;
		if (parent != null) {
			parent.changed(this);
		}
	}

	public void setValue(final int value) {
		this.value = value;
		if (parent != null) {
			parent.changed(this);
		}
	}

	public void delete() {
		if (parent != null) {
			parent.delete(this);
		}
	}
}
