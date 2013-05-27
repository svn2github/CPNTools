package org.cpntools.simulator.extensions.ppcpnets.java;

import java.util.Map;

import org.cpntools.simulator.extensions.scraper.types.Type;

/**
 * @author michael
 */
public class Local extends Variable {
	private final Map<String, String> initialValues;

	/**
	 * @param id
	 * @param name
	 * @param type
	 * @param originalType
	 * @param initialValues
	 */
	public Local(final String id, final String name, final Type type, final String originalType,
	        final Map<String, String> initialValues) {
		super(id, name, type, originalType);
		this.initialValues = initialValues;
	}

	/**
	 * @param key
	 * @return
	 */
	public String getInitialValue(final String key) {
		return initialValues.get(key);
	}

	/**
	 * @return
	 */
	public Map<String, String> getInitialValues() {
		return initialValues;
	}

}
