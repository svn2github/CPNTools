package org.cpntools.simulator.extensions;

import java.io.IOException;

import org.cpntools.accesscpn.engine.protocol.Packet;

/**
 * @author michael
 */
public interface Channel {
	/**
	 * @param expression
	 * @return
	 * @throws Exception
	 */
	String evaluate(String expression) throws Exception;

	/**
	 * @param clazz
	 * @return
	 */
	<T extends Extension> T getExtension(Class<T> clazz);

	/**
	 * @param p
	 * @return
	 * @throws IOException
	 */
	Packet send(Packet p) throws IOException;

	/**
	 * @param command
	 * @param h
	 * @throws IOException
	 */
	void subscribe(Command command, SubscriptionHandler h) throws IOException;
}
