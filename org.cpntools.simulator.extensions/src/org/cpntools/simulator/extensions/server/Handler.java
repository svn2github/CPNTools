package org.cpntools.simulator.extensions.server;

import java.io.DataInputStream;
import java.io.DataOutputStream;

import org.cpntools.accesscpn.engine.protocol.Packet;

/**
 * @author michael
 */
public class Handler implements Runnable {

	private final DataInputStream in;
	private final DataOutputStream out;

	/**
	 * @param in
	 * @param out
	 */
	public Handler(final DataInputStream in, final DataOutputStream out) {
		this.in = in;
		this.out = out;
	}

	/**
	 * @see java.lang.Runnable#run()
	 */
	@Override
	public void run() {
		try {
			while (true) {
				final Packet p = new Packet();
				p.receive(in);
			}
		} catch (final Exception _) {

		}
	}
}
