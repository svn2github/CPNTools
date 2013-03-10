package org.cpntools.simulator.extensions;

/**
 * @author michael
 * @param <T>
 */
public class Option<T> {
	/**
	 * @param name
	 * @param key
	 * @param type
	 * @return
	 */
	public static final <T> Option<T> create(final String name, final String key, final Class<T> type) {
		return new Option<T>(name, key, type);
	}

	private final String key;

	private final String name;

	private final Class<T> type;

	/**
	 * @param name
	 * @param key
	 * @param type
	 */
	public Option(final String name, final String key, final Class<T> type) {
		this.name = name;
		this.key = key;
		checkType(type);
		this.type = type;
	}

	/**
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(final Object obj) {
		if (this == obj) { return true; }
		if (obj == null) { return false; }
		if (!(obj instanceof Option)) { return false; }
		final Option<?> other = (Option<?>) obj;
		if (key == null) {
			if (other.key != null) { return false; }
		} else if (!key.equals(other.key)) { return false; }
		if (type == null) {
			if (other.type != null) { return false; }
		} else if (!type.equals(other.type)) { return false; }
		return true;
	}

	/**
	 * @return
	 */
	public String getKey() {
		return key;
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
	public Class<T> getType() {
		return type;
	}

	/**
	 * @return
	 */
	public int getTypeId() {
		if (type == Boolean.class) { return 0; }
		if (type == Integer.class) { return 1; }
		if (type == String.class) { return 2; }
		return -1;
	}

	/**
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + (key == null ? 0 : key.hashCode());
		result = prime * result + (type == null ? 0 : type.hashCode());
		return result;
	}

	/**
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return name + " (" + type.getSimpleName() + ')';
	}

	@SuppressWarnings("hiding")
	protected void checkType(final Class<?> type) {
		if (type == Integer.class) { return; }
		if (type == Boolean.class) { return; }
		if (type == String.class) { return; }
		throw new IllegalArgumentException(type + " is not of type integer, boolean or string");
	}
}
