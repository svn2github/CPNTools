package org.cpntools.simulator.extensions.server.ui;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Graphics;
import java.awt.HeadlessException;
import java.awt.Image;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.net.InetAddress;
import java.net.UnknownHostException;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.DefaultListModel;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JList;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.KeyStroke;
import javax.swing.ScrollPaneConstants;
import javax.swing.UIManager;
import javax.swing.table.DefaultTableModel;

import org.cpntools.simulator.extensions.Extension;
import org.cpntools.simulator.extensions.server.Handler;
import org.cpntools.simulator.extensions.server.HandlerView;
import org.cpntools.simulator.extensions.server.Server;

/**
 * @author michael
 */
public class MainFrame extends JFrame implements HandlerView {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	static {
		System.setProperty("apple.laf.useScreenMenuBar", "true");
		System.setProperty("com.apple.mrj.application.apple.menu.about.name", "CPN Tools Extensions");
		try {
			UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
		} catch (final Exception e) {
			// Ignore
		}
	}

	private final DefaultTableModel model;
	private final DefaultListModel sessions;

	/**
	 * @throws HeadlessException
	 * @throws UnknownHostException
	 */
	public MainFrame() throws HeadlessException, UnknownHostException {
		super("CPN Tools Simulator Extension Server");
		setLayout(new BorderLayout());

		final JPanel frame = new JPanel(new BorderLayout()) {
			/**
			 * 
			 */
			private static final long serialVersionUID = 1L;
			Image bg = new ImageIcon(MainFrame.class.getResource("background.png")).getImage();

			@Override
			public void paintComponent(final Graphics g) {
				final int w = bg.getWidth(null);
				final int h = bg.getHeight(null);
				for (int x = 0; x < getWidth(); x += w) {
					for (int y = 0; y < getHeight(); y += h) {
						g.drawImage(bg, x, y, w, h, this);
					}
				}
			}
		};
		add(frame);

		final JMenuBar menu = new JMenuBar();
		final JMenu file = new JMenu("File");
		file.setMnemonic(KeyEvent.VK_F);
		menu.add(file);
		final JMenuItem quit = new JMenuItem("Quit", KeyEvent.VK_Q);
		quit.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_Q, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));

		file.add(quit);
		quit.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(final ActionEvent arg0) {
				System.exit(0);
			}
		});
		setJMenuBar(menu);

		final JPanel buttons = new JPanel();
		buttons.setLayout(new FlowLayout(FlowLayout.CENTER));
// final JButton startButton = new JButton("Start");
// startButton.setMnemonic(KeyEvent.VK_S);
// startButton.setEnabled(false);
// buttons.add(startButton);
// final JButton stopButton = new JButton("Stop");
// stopButton.setMnemonic(KeyEvent.VK_O);
// stopButton.setEnabled(false);
// buttons.add(stopButton);
		final JButton hideButton = new JButton("Hide");
		hideButton.setMnemonic(KeyEvent.VK_H);
		hideButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(final ActionEvent e) {
				MainFrame.this.setVisible(false);
			}
		});
		buttons.add(hideButton);
		final JButton quitButton = new JButton("Quit");
		quitButton.setMnemonic(KeyEvent.VK_Q);
		quitButton.setDefaultCapable(true);
		quitButton.addActionListener(new ActionListener() {

			@Override
			public void actionPerformed(final ActionEvent arg0) {
				System.exit(0);
			}
		});
		buttons.add(quitButton);
		buttons.setOpaque(false);
		frame.add(buttons, BorderLayout.SOUTH);

		final JPanel main = new JPanel();
		main.setOpaque(false);
		main.setLayout(new BoxLayout(main, BoxLayout.Y_AXIS));
		final JTextField host = new JTextField();
		try {
			host.setText(InetAddress.getLocalHost().getHostName());
		} catch (final Exception e) {
			try {
				host.setText(InetAddress.getLocalHost().getHostAddress());
			} catch (final Exception e2) {
				host.setText("localhost");
			}
		}
		host.setBackground(new Color(255, 255, 255, 192));
		host.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
		final JPanel hostPanel = new JPanel(new BorderLayout());
		hostPanel.add(host);
		hostPanel.setOpaque(false);
		hostPanel.setBorder(BorderFactory.createTitledBorder("Host"));
		host.setMinimumSize(new Dimension(200, 40));
		host.setPreferredSize(new Dimension(300, 40));
		host.setMaximumSize(new Dimension(2000, 40));
		host.setEditable(false);
		main.add(hostPanel);
		final JTextField port = new JTextField("" + Server.DEFAULT_PORT);
		port.setBackground(new Color(255, 255, 255, 192));
		port.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
		final JPanel portPanel = new JPanel(new BorderLayout());
		portPanel.setOpaque(false);
		portPanel.add(port);
		portPanel.setBorder(BorderFactory.createTitledBorder("Port"));
		port.setMinimumSize(new Dimension(200, 40));
		port.setPreferredSize(new Dimension(300, 40));
		port.setMaximumSize(new Dimension(2000, 40));
		port.setEditable(false);
		main.add(portPanel);

		model = new DefaultTableModel() {
			/**
			 * 
			 */
			private static final long serialVersionUID = 1L;

			@Override
			public Class<?> getColumnClass(final int column) {
				if (column == 0) return Boolean.class;
				return super.getColumnClass(column);
			}

			@Override
			public boolean isCellEditable(final int row, final int column) {
				return false;
			}
		};
		model.setColumnIdentifiers(new Object[] { "Enabled", "Name" });
		final JTable extensionList = new JTable(model);
		extensionList.setBackground(new Color(255, 255, 255, 192));
		extensionList.setOpaque(false);
		extensionList.getColumnModel().getColumn(0).setMaxWidth(50);
		final JScrollPane scroller = new JScrollPane(extensionList);
		scroller.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
		scroller.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
		scroller.setBorder(BorderFactory.createTitledBorder("Installed Extensions"));
		scroller.setMinimumSize(new Dimension(200, 100));
		scroller.setPreferredSize(new Dimension(300, 150));
		scroller.setMaximumSize(new Dimension(2000, 1000));
		scroller.setOpaque(false);
		scroller.getViewport().setOpaque(false);
		main.add(scroller);

		sessions = new DefaultListModel();
		final JList sessionList = new JList(sessions);
		sessionList.setOpaque(false);
		sessionList.setBackground(new Color(255, 255, 255, 192));
		final JScrollPane sessionScroller = new JScrollPane(sessionList);
		sessionScroller.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
		sessionScroller.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
		sessionScroller.setBorder(BorderFactory.createTitledBorder("Active Sessions"));
		sessionScroller.setBackground(null);
		sessionScroller.setMinimumSize(new Dimension(200, 100));
		sessionScroller.setPreferredSize(new Dimension(300, 150));
		sessionScroller.setMaximumSize(new Dimension(2000, 1000));
		sessionScroller.setOpaque(false);
		sessionScroller.getViewport().setOpaque(false);
		main.add(sessionScroller);

		frame.add(main);

		pack();
	}

	/**
	 * @param e
	 */
	public void addExtension(final Extension e) {
		model.addRow(new Object[] { true, e });
	}

	private static class View {
		private final Handler handler;
		private final String text;

		public View(final Handler handler, final String text) {
			this.handler = handler;
			this.text = text;
		}

		@Override
		public String toString() {
			return text;
		}

		public Handler getHandler() {
			return handler;
		}
	}

	/**
	 * @see org.cpntools.simulator.extensions.server.HandlerView#add(org.cpntools.simulator.extensions.server.Handler,
	 *      java.lang.String)
	 */
	@Override
	public synchronized void add(final Handler handler, final String string) {
		sessions.addElement(new View(handler, string));
	}

	/**
	 * @see org.cpntools.simulator.extensions.server.HandlerView#remove(org.cpntools.simulator.extensions.server.Handler)
	 */
	@Override
	public synchronized void remove(final Handler handler) {
		for (int i = 0; i < sessions.getSize(); i++) {
			final Object o = sessions.getElementAt(i);
			if (o instanceof View) {
				final View v = (View) o;
				if (v.getHandler() == handler) {
					sessions.remove(i);
					return;
				}
			}
		}
	}
}
