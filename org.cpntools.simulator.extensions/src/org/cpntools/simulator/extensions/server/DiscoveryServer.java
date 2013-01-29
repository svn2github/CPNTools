package org.cpntools.simulator.extensions.server;

import java.awt.HeadlessException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.swing.ProgressMonitor;
import javax.swing.UnsupportedLookAndFeelException;

import org.clapper.util.classutil.AbstractClassFilter;
import org.clapper.util.classutil.AndClassFilter;
import org.clapper.util.classutil.ClassFilter;
import org.clapper.util.classutil.ClassFinder;
import org.clapper.util.classutil.ClassInfo;
import org.clapper.util.classutil.InterfaceOnlyClassFilter;
import org.clapper.util.classutil.NotClassFilter;
import org.clapper.util.classutil.SubclassClassFilter;
import org.cpntools.simulator.extensions.Extension;
import org.cpntools.simulator.extensions.scraper.Scraper;
import org.cpntools.simulator.extensions.server.ui.MainFrame;

/**
 * @author michael
 */
public class DiscoveryServer {

	/**
	 * @throws HeadlessException
	 * @throws IOException
	 * @throws InterruptedException
	 */
	public DiscoveryServer() throws HeadlessException, IOException, InterruptedException {
		final ProgressMonitor progressMonitor = new ProgressMonitor(null, "Launching...", "Discovering extensions...",
		        0, 4);
		int progress = 0;

		final Collection<ClassInfo> extensionsClasses = findExtensions();

		progressMonitor.setNote("Instantiating extensions...");
		progressMonitor.setProgress(++progress);
		final Collection<Extension> extensions = instantiate(extensionsClasses);

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

	private Collection<Extension> instantiate(final Collection<ClassInfo> extensionsClasses) {
		final List<Extension> result = new ArrayList<Extension>();
		result.add(Scraper.INSTANCE);
		for (final ClassInfo ci : extensionsClasses) {
			try {
				final Class<Extension> clazz = (Class<Extension>) Class.forName(ci.getClassName());
				if (!clazz.equals(Scraper.class)) {
					final Extension instance = clazz.newInstance();
					result.add(instance);
				}
			} catch (final Exception _) {
				_.printStackTrace();
			}
		}
		return result;
	}

	private Collection<ClassInfo> findExtensions() {
		final ClassFinder finder = new ClassFinder();
		finder.addClassPath();

		final ClassFilter filter = new AndClassFilter(new NotClassFilter(new InterfaceOnlyClassFilter()),
		        new SubclassClassFilter(Extension.class), new NotClassFilter(new AbstractClassFilter()));

		final Collection<ClassInfo> foundClasses = new ArrayList<ClassInfo>();
		finder.findClasses(foundClasses, filter);
		return foundClasses;
	}

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
}
