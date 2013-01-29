package org.cpntools.simulator.extensions.ranges;

import java.io.IOException;

import org.cpntools.simulator.extensions.Extension;
import org.cpntools.simulator.extensions.declare.DeclareExtension;
import org.cpntools.simulator.extensions.scraper.Scraper;
import org.cpntools.simulator.extensions.server.Server;
import org.cpntools.simulator.extensions.server.ui.MainFrame;

/**
 * @author michael
 */
public class RangeTestServer {

	/**
	 * @param args
	 * @throws InterruptedException
	 * @throws IOException
	 */
	public static void main(final String[] args) throws InterruptedException, IOException {
		final MainFrame main = new MainFrame();
		final Server server = new Server(Server.DEFAULT_PORT, Scraper.INSTANCE, new RangeExtension(),
		        new DeclareExtension());
		final Thread t = new Thread(server, "Main server on port " + Server.DEFAULT_PORT);
		for (final Extension e : server) {
			main.addExtension(e);
		}
		main.setVisible(true);
		t.start();
		t.join();
	}
}
