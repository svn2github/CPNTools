package org.cpntools.simulator.extensions;

import java.util.List;

import org.cpntools.accesscpn.engine.protocol.Packet;

/**
 * @author michael
 */
public interface Extension {

	/**
	 * Identifier for use for testing or early stages of development of extensions.
	 */
	public static final int TESTING = 9999;

	/**
	 * REturn the identifier for the extension. Must be globally unique and should hence be requested from Michael
	 * Westergaard. For testing purposes TESTING (static field) can be used, but this will conflict and should be
	 * avoided for anything but short-term usage.
	 * 
	 * @return
	 */
	int getIdentifier();

	/**
	 * NAme of this extension. Should not be more than 50 characters.
	 * 
	 * @return
	 */
	String getName();

	/**
	 * Return a list describing all registered options.
	 * 
	 * @return
	 */
	List<Option<?>> getOptions();

	/**
	 * Return an object that can be used to call using JavaExecute RPC
	 * 
	 * @return
	 */
	Object getRPCHandler();

	/**
	 * Allow this handler to subscribe to other kinds of messages, e.g., for conputing enabling or syntax check.
	 * 
	 * @return
	 */
	List<Command> getSubscriptions();

	/**
	 * Handle a packet. Only packets matching the identifier will ever be passed. handle is only called on objects
	 * created by start and hence had a Handler.
	 * 
	 * @param p
	 * @return
	 */
	Packet handle(Packet p);

	/**
	 * Handle a subscription. The response given by the CPN simulator is given as well.
	 * 
	 * @param p
	 * @param response
	 * @return
	 */
	Packet handle(Packet p, Packet response);

	/**
	 * If result is non-null, this code will be injected into the simulator; this can be useful for generating sub code
	 * for calling the extension.
	 * 
	 * @return
	 */
	String inject();

	/**
	 * Set the value of the option.
	 * 
	 * @param option
	 * @param value
	 */
	<T> void setOption(Option<T> option, T value);

	/**
	 * Instantiate extension (non-static factory).
	 * 
	 * @param c
	 * @return
	 */
	Extension start(Channel c);

	Packet prefilter(Packet request);

}
