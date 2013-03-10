package org.cpntools.simulator.extensions.test;

import java.io.IOException;

import javax.swing.UnsupportedLookAndFeelException;

import org.cpntools.simulator.extensions.Extension;
import org.cpntools.simulator.extensions.scraper.Scraper;
import org.cpntools.simulator.extensions.server.Server;
import org.cpntools.simulator.extensions.server.ui.MainFrame;

/**
 * @author michael
 */
public class TestServer {

	/**
	 * @param args
	 * @throws InterruptedException
	 * @throws IOException
	 * @throws UnsupportedLookAndFeelException
	 * @throws IllegalAccessException
	 * @throws InstantiationException
	 * @throws ClassNotFoundException
	 */
	public static void main(final String[] args) throws InterruptedException, IOException, ClassNotFoundException,
	        InstantiationException, IllegalAccessException, UnsupportedLookAndFeelException {
		final MainFrame main = new MainFrame();
		final Server server = new Server(Server.DEFAULT_PORT, Scraper.INSTANCE);
		final Thread t = new Thread(server, "Main server on port " + Server.DEFAULT_PORT);
		for (final Extension e : server) {
			main.addExtension(e);
		}
		main.setVisible(true);
		t.start();
		t.join();
	}
}
