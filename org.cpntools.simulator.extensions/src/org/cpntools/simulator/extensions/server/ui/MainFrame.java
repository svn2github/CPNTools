package org.cpntools.simulator.extensions.server.ui;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.HeadlessException;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.net.InetAddress;
import java.net.UnknownHostException;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JFrame;
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
import org.cpntools.simulator.extensions.server.Server;

/**
 * @author michael
 */
public class MainFrame extends JFrame {
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

	/**
	 * @throws HeadlessException
	 * @throws UnknownHostException
	 */
	public MainFrame() throws HeadlessException, UnknownHostException {
		super("CPN Tools Simulator Extension Server");
		setLayout(new BorderLayout());

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
		add(buttons, BorderLayout.SOUTH);

		final JPanel main = new JPanel();
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
		final JPanel hostPanel = new JPanel(new BorderLayout());
		hostPanel.add(host);
		hostPanel.setBorder(BorderFactory.createTitledBorder("Host"));
		host.setMinimumSize(new Dimension(200, (int) host.getSize().getHeight()));
		host.setEditable(false);
		main.add(hostPanel);
		final JTextField port = new JTextField("" + Server.DEFAULT_PORT);
		final JPanel portPanel = new JPanel(new BorderLayout());
		portPanel.add(port);
		portPanel.setBorder(BorderFactory.createTitledBorder("Port"));
		port.setMinimumSize(new Dimension(200, (int) port.getSize().getHeight()));
		port.setEditable(false);
		main.add(portPanel);

		model = new DefaultTableModel() {
			/**
			 * 
			 */
			private static final long serialVersionUID = 1L;

			@Override
			public Class<?> getColumnClass(final int column) {
				if (column == 0) { return Boolean.class; }
				return super.getColumnClass(column);
			}

			@Override
			public boolean isCellEditable(final int row, final int column) {
				return false;
			}
		};
		model.setColumnIdentifiers(new Object[] { "Enabled", "Name" });
		final JTable extensionList = new JTable(model);
		extensionList.getColumnModel().getColumn(0).setMaxWidth(50);
		final JScrollPane scroller = new JScrollPane(extensionList);
		scroller.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
		scroller.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
		scroller.setBorder(BorderFactory.createTitledBorder("Installed Extensions"));
		scroller.setBackground(null);
		extensionList.setBackground(null);
		main.add(scroller);

		add(main);

		pack();
	}

	/**
	 * @param e
	 */
	public void addExtension(final Extension e) {
		model.addRow(new Object[] { true, e });
	}
}
