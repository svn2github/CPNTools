package org.cpntools.simulator.extensions.server;

import java.awt.AWTException;
import java.awt.CheckboxMenuItem;
import java.awt.HeadlessException;
import java.awt.Image;
import java.awt.MenuItem;
import java.awt.MenuShortcut;
import java.awt.PopupMenu;
import java.awt.SystemTray;
import java.awt.Toolkit;
import java.awt.TrayIcon;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.IOException;
import java.net.BindException;
import java.util.Collection;
import java.util.prefs.Preferences;

import javax.imageio.ImageIO;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.KeyStroke;
import javax.swing.ProgressMonitor;
import javax.swing.WindowConstants;

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

	private static final String MINIMIZED = "minimized";

	/**
	 * @param args
	 * @throws Exception
	 */
	public static void main(final String[] args) throws Exception {
		new DiscoveryServer(false);
	}

	PopupMenu extensionsItem = new PopupMenu("Extensions");

	/**
	 * @param silent
	 * @throws HeadlessException
	 * @throws IOException
	 * @throws InterruptedException
	 */
	public DiscoveryServer(final boolean silent) throws HeadlessException, IOException, InterruptedException {
		final ProgressMonitor progressMonitor = new ProgressMonitor(null, "Launching...", "Discovering extensions...",
		        0, 4);
		int progress = 0;

		final Pair<Collection<ClassInfo>, Class<Extension>> extensionsClasses = Discovery.findExtensions(
		        Extension.class, false);

		progressMonitor.setNote("Instantiating extensions...");
		progressMonitor.setProgress(++progress);
		final Collection<Extension> extensions = Discovery.instantiate(extensionsClasses);
		extensions.add(Scraper.INSTANCE);

		progressMonitor.setNote("Starting server thread...");
		progressMonitor.setProgress(++progress);
		try {
			final Server server = new Server(Server.DEFAULT_PORT, extensions);
			final Thread t = new Thread(server, "Main server on port " + Server.DEFAULT_PORT);

			progressMonitor.setNote("Preparing UI...");
			progressMonitor.setProgress(++progress);
			final MainFrame main = new MainFrame() {
				/**
				 * 
				 */
				private static final long serialVersionUID = 1L;

				@Override
				public void addExtension(final Extension e) {
					super.addExtension(e);
					final CheckboxMenuItem item = new CheckboxMenuItem(e.getName(), true);
					item.setEnabled(false);
					extensionsItem.add(item);
				}
			};
			for (final Extension e : server) {
				main.addExtension(e);
			}

			progressMonitor.setNote("Done!");
			progressMonitor.setProgress(++progress);
			progressMonitor.close();
			try {
				final SystemTray tray = SystemTray.getSystemTray();
				final Image image = ImageIO.read(DiscoveryServer.class.getResource("trayicon128.png"));
				final PopupMenu menu = new PopupMenu();
				final MenuItem showItem = new MenuItem("Show");
				menu.add(showItem);
				menu.add(extensionsItem);
// menu.addSeparator();
// final MenuItem stopItem = new MenuItem("Stop");
// stopItem.setEnabled(false);
// menu.add(stopItem);
// final MenuItem startItem = new MenuItem("Start");
// startItem.setEnabled(false);
// menu.add(startItem);
				menu.addSeparator();
				final MenuItem quitItem = new MenuItem("Quit", new MenuShortcut(KeyStroke.getKeyStroke(KeyEvent.VK_Q,
				        Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()).getKeyCode()));
				menu.add(quitItem);
				final TrayIcon trayIcon = new TrayIcon(image, "CPN Tools Extension Server", menu);
				trayIcon.setImageAutoSize(true);
				final Preferences preferences = Preferences.userNodeForPackage(DiscoveryServer.class);
				showItem.addActionListener(new ActionListener() {
					@Override
					public void actionPerformed(final ActionEvent e) {
						main.setVisible(true);
						preferences.putBoolean(DiscoveryServer.MINIMIZED, false);
						tray.remove(trayIcon);
					}
				});
				trayIcon.addMouseListener(new MouseAdapter() {
					@Override
					public void mouseClicked(final MouseEvent e) {
						if (e.getClickCount() >= 2) {
							main.setVisible(true);
							preferences.putBoolean(DiscoveryServer.MINIMIZED, false);
							tray.remove(trayIcon);
						}
					}
				});
				quitItem.addActionListener(new ActionListener() {
					@Override
					public void actionPerformed(final ActionEvent arg0) {
						System.exit(0);
					}
				});
				main.addComponentListener(new ComponentAdapter() {
					@Override
					public void componentHidden(final ComponentEvent e) {
						try {
							tray.add(trayIcon);
							preferences.putBoolean(DiscoveryServer.MINIMIZED, true);
						} catch (final AWTException _) {
							System.exit(0);
						}
					}
				});
				main.setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);
				if (preferences.getBoolean(DiscoveryServer.MINIMIZED, true)) {
					tray.add(trayIcon);
				} else {
					main.setVisible(true);
				}
			} catch (final Exception _) {
				main.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
				main.setVisible(true);
			}
			t.start();
			t.join();
		} catch (final BindException e) {
			progressMonitor.close();
			if (!silent) {
				JOptionPane.showMessageDialog(null, "Port " + Server.DEFAULT_PORT
				        + " is already in use. Maybe the extension server is already running?",
				        "Error starting extension server", JOptionPane.ERROR_MESSAGE);
			}
			System.exit(1);
		}
	}
}
