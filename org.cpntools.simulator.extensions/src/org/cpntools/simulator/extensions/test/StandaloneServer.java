package org.cpntools.simulator.extensions.test;

import java.io.IOException;

import org.cpntools.simulator.extensions.server.Server;

public class StandaloneServer {

	/**
	 * @param args
	 * @throws InterruptedException
	 * @throws IOException
	 */
	public static void main(final String[] args) throws InterruptedException, IOException {
		final Thread t = new Thread(new Server(Server.DEFAULT_PORT), "Main server on port " + Server.DEFAULT_PORT);
		t.start();
		t.join();
	}
}
