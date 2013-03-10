package org.cpntools.simulator.extensions.server;

import java.awt.HeadlessException;
import java.io.IOException;
import java.util.Collection;

import javax.swing.ProgressMonitor;
import javax.swing.UnsupportedLookAndFeelException;

import org.clapper.util.classutil.ClassInfo;
import org.cpntools.simulator.extensions.Extension;
import org.cpntools.simulator.extensions.scraper.Scraper;
import org.cpntools.simulator.extensions.server.ui.MainFrame;
import org.cpntools.simulator.extensions.utils.Discovery;

import dk.klafbang.tools.Pair;

/**
 * @author michael
 */
public class DiscoveryServer {

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
		new DiscoveryServer();
	}

	/**
	 * @throws HeadlessException
	 * @throws IOException
	 * @throws InterruptedException
	 */
	public DiscoveryServer() throws HeadlessException, IOException, InterruptedException {
		final ProgressMonitor progressMonitor = new ProgressMonitor(null, "Launching...", "Discovering extensions...",
		        0, 4);
		int progress = 0;

		final Pair<Collection<ClassInfo>, Class<Extension>> extensionsClasses = Discovery
		        .findExtensions(Extension.class);

		progressMonitor.setNote("Instantiating extensions...");
		progressMonitor.setProgress(++progress);
		final Collection<Extension> extensions = Discovery.instantiate(extensionsClasses);
		extensions.add(Scraper.INSTANCE);

		progressMonitor.setNote("Starting server thread...");
		progressMonitor.setProgress(++progress);
		final Server server = new Server(Server.DEFAULT_PORT, extensions);
		final Thread t = new Thread(server, "Main server on port " + Server.DEFAULT_PORT);

		progressMonitor.setNote("Preparing UI...");
		progressMonitor.setProgress(++progress);
		final MainFrame main = new MainFrame();
		for (final Extension e : server) {
			main.addExtension(e);
		}

		progressMonitor.setNote("Done!");
		progressMonitor.setProgress(++progress);
		progressMonitor.close();
		main.setVisible(true);
		t.start();
		t.join();
	}
}
