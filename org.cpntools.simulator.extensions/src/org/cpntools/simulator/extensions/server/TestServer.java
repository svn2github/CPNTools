package org.cpntools.simulator.extensions.server;

import java.io.IOException;

import org.cpntools.simulator.extensions.test.HelloWorld;

public class TestServer {

	/**
	 * @param args
	 * @throws InterruptedException
	 * @throws IOException
	 */
	public static void main(final String[] args) throws InterruptedException, IOException {
		final Thread t = new Thread(new Server(Server.DEFAULT_PORT, new HelloWorld()), "Main server in port "
		        + Server.DEFAULT_PORT);
		t.start();
		t.join();
	}
}
