package org.cpntools.simulator.extensions.ppcpnets.java;

/**
 * @author michael
 */
public class Lock implements HasJavaName, Comparable<Lock> {
	private final int count;

	private String name;

	/**
	 * @param name
	 * @param count
	 */
	public Lock(final String name, final int count) {
		this.name = name;
		this.count = count;

	}

	/**
	 * @see java.lang.Comparable#compareTo(java.lang.Object)
	 */
	@Override
	public int compareTo(final Lock o) {
		return name.compareTo(o.name);
	}

	/**
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(final Object obj) {
		if (this == obj) { return true; }
		if (obj == null) { return false; }
		if (!(obj instanceof Lock)) { return false; }
		final Lock other = (Lock) obj;
		if (name == null) {
			if (other.name != null) { return false; }
		} else if (!name.equals(other.name)) { return false; }
		return true;
	}

	/**
	 * @return
	 */
	public int getCount() {
		return count;
	}

	/**
	 * @return
	 */
	@Override
	public String getJavaName() {
		return Variable.makeJavaName(name);
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
		int result = 1;
		result = prime * result + (name == null ? 0 : name.hashCode());
		return result;
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.HasJavaName#setName(java.lang.String)
	 */
	@Override
	public void setName(final String name) {
		this.name = name;
	}
}
