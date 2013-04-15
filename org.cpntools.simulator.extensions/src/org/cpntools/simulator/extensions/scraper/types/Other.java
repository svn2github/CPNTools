package org.cpntools.simulator.extensions.scraper.types;

/**
 * @author michael
 */
public class Other extends Type {

	private final java.lang.String name;

	/**
	 * @param name
	 */
	public Other(final java.lang.String name) {
		this.name = name;
	}

	/**
	 * @return
	 */
	@Override
	public java.lang.String getJavaName() {
		final StringBuilder result = new StringBuilder();
		for (final java.lang.String t : name.split("_")) {
			if (!"".equals(t.trim())) {
				if (t.equals(t.toUpperCase())) {
					result.append(t.substring(0, 1).toUpperCase());
					result.append(t.substring(1).toLowerCase());
				} else {
					result.append(t);
				}
			}
		}
		return result.toString();
	}

	/**
	 * @return
	 */
	public java.lang.String getName() {
		return name;
	}

}
