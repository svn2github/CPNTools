package org.cpntools.simulator.extensions;

import org.cpntools.simulator.extensions.scraper.Element;

/**
 * @author michael
 */
public class Invocation {

	private final Instrument instrument;
	private final Element element;

	/**
	 * @param i
	 * @param e
	 */
	public Invocation(final Instrument i, final Element e) {
		instrument = i;
		element = e;
	}

	/**
	 * @return
	 */
	public Element getElement() {
		return element;
	}

	/**
	 * @return
	 */
	public Instrument getInstrument() {
		return instrument;
	}

}
