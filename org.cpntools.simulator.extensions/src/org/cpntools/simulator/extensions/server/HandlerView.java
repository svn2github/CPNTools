package org.cpntools.simulator.extensions.server;

/**
 * @author michael
 */
public interface HandlerView {

	/**
	 * @param handler
	 * @param string
	 */
	void add(Handler handler, String string);

	/**
	 * @param handler
	 */
	void remove(Handler handler);

}
