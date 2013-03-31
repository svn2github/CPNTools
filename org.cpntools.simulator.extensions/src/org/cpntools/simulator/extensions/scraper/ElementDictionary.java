package org.cpntools.simulator.extensions.scraper;

/**
 * @author michael
 */
public interface ElementDictionary {
	/**
	 * @param id
	 * @return
	 */
	Element get(String id);

	/**
	 * @param id
	 * @param element
	 */
	void put(String id, Element element);
}
