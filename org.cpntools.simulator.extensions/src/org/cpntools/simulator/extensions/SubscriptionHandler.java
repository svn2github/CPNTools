package org.cpntools.simulator.extensions;

import org.cpntools.accesscpn.engine.protocol.Packet;

/**
 * @author michael
 */
public interface SubscriptionHandler {
	/**
	 * REturn the identifier for the extension. Must be globally unique and should hence be requested from Michael
	 * Westergaard. For testing purposes TESTING (static field) can be used, but this will conflict and should be
	 * avoided for anything but short-term usage.
	 * 
	 * @return
	 */
	int getIdentifier();

	/**
	 * Handle a subscription. The response given by the CPN simulator is given as well.
	 * 
	 * @param p
	 * @param response
	 * @return
	 */
	Packet handle(Packet p, Packet response);

	/**
	 * @param request
	 * @return
	 */
	Packet prefilter(Packet request);

}
