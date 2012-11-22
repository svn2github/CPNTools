package org.cpntools.simulator.extensions.declare;

import java.io.IOException;

import org.cpntools.simulator.extensions.scraper.Scraper;
import org.cpntools.simulator.extensions.server.Server;
import org.cpntools.simulator.extensions.test.HelloWorld;

public class DeclareTestServer {

	/**
	 * @param args
	 * @throws InterruptedException
	 * @throws IOException
	 */
	public static void main(final String[] args) throws InterruptedException, IOException {
		final Thread t = new Thread(new Server(Server.DEFAULT_PORT, Scraper.INSTANCE, new HelloWorld(),
		        new DeclareExtension()), "Main server on port " + Server.DEFAULT_PORT);
		t.start();
		t.join();
	}
}
