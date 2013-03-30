package org.cpntools.simulator.extensions.scraper;

/**
 * @author michael
 */
public class DummyElement extends Element {

	/**
	 * @param id
	 */
	public DummyElement(final String id) {
		super(new ElementDictionary() {
			@Override
			public void put(@SuppressWarnings("hiding") final String id, final Element element) {
				// Dummy implementation
			}

			@Override
			public Element get(@SuppressWarnings("hiding") final String id) {
				return null;
			}
		}, id);
	}
}
