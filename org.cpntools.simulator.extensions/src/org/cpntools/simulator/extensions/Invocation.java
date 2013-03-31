package org.cpntools.simulator.extensions;

import org.cpntools.simulator.extensions.scraper.Element;

/**
 * @author michael
 */
public class Invocation {

	private final Element element;
	private final Instrument instrument;

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
