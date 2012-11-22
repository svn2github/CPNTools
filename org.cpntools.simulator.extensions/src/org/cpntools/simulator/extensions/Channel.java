package org.cpntools.simulator.extensions;

import java.io.IOException;

import org.cpntools.accesscpn.engine.protocol.Packet;

/**
 * @author michael
 */
public interface Channel {
	/**
	 * @param p
	 * @return
	 * @throws IOException
	 */
	Packet send(Packet p) throws IOException;

	<T extends Extension> T getExtension(Class<T> clazz);
}
