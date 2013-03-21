package org.cpntools.simulator.extensions.test;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Frame;
import java.util.HashMap;
import java.util.Map;
import java.util.Observable;
import java.util.Observer;
import java.util.SortedSet;
import java.util.TreeSet;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.DefaultListModel;
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;
import javax.swing.SwingConstants;

import org.cpntools.accesscpn.engine.protocol.Packet;
import org.cpntools.simulator.extensions.AbstractExtension;
import org.cpntools.simulator.extensions.Channel;
import org.cpntools.simulator.extensions.Command;
import org.cpntools.simulator.extensions.NamedRPCHandler;
import org.cpntools.simulator.extensions.Option;
import org.cpntools.simulator.extensions.scraper.Page;
import org.cpntools.simulator.extensions.scraper.Scraper;
import org.cpntools.simulator.extensions.scraper.Transition;

/**
 * @author michael
 */
public abstract class HelloWorld extends AbstractExtension implements Observer {
	/**
	 * @author michael
	 */
	public final class HelloWorldRPCHandler implements NamedRPCHandler {
		/**
		 * @param a
		 * @param b
		 * @return
		 */
		public Integer add(final Integer a, final Integer b) {
			return a + b;
		}

		/**
		 * @param name
		 * @return
		 */
		public String hello(final String name) {
			return "Hello, " + name;
		}

		/**
		 * @see org.cpntools.simulator.extensions.NamedRPCHandler#structureName()
		 */
		@Override
		public String structureName() {
			return "Hello";
		}
	}

	/**
	 * 
	 */
	public static final int ID = 10000;
	private JCheckBox booleanOption;
	private JDialog dialog;
	private final Map<Page, Integer> indexes = new HashMap<Page, Integer>();
	private JTextField integerOption;
	private final Map<Page, DefaultListModel> lists = new HashMap<Page, DefaultListModel>();

	private JTextField stringOption;

	private JTabbedPane tabs;

	/**
	 * @see org.cpntools.simulator.extensions.AbstractExtension#getRPCHandler()
	 */
	@Override
	public Object getRPCHandler() {
		return new HelloWorldRPCHandler();
	}

	/**
	 * 
	 */
	public HelloWorld() {
		// addOption(Option.create("String", "string", String.class), Option.create("Integer", "integer",
// Integer.class),
		// Option.create("Boolean", "boolean", Boolean.class));
		addOption(Option.create("Smart simulation", "safe", Boolean.class));
		addSubscription(new Command(200, 9));
		addObserver(this);
	}

	/**
	 * @see org.cpntools.simulator.extensions.Extension#getIdentifier()
	 */
	@Override
	public int getIdentifier() {
		return HelloWorld.ID;
	}

	/**
	 * @see org.cpntools.simulator.extensions.Extension#getName()
	 */
	@Override
	public String getName() {
		return "DCR Graph";
	}

	/**
	 * @see org.cpntools.simulator.extensions.Extension#handle(org.cpntools.accesscpn.engine.protocol.Packet)
	 */
	@Override
	public Packet handle(final Packet p) {
		return null;
	}

	/**
	 * @see org.cpntools.simulator.extensions.AbstractExtension#handle(org.cpntools.accesscpn.engine.protocol.Packet,
	 *      org.cpntools.accesscpn.engine.protocol.Packet)
	 */
	@Override
	public Packet handle(final Packet p, final Packet r) {
		p.reset();
		if (p.getInteger() == 200 && p.getInteger() == 9) {
			dialog.setTitle("Hello World [" + p.getString() + "]");
		}
		return null;
	}

	/**
	 * @see org.cpntools.simulator.extensions.AbstractExtension#setChannel(org.cpntools.simulator.extensions.Channel)
	 */
	@Override
	public void setChannel(final Channel c) {
		super.setChannel(c);

		try {
			dialog = new JDialog((Frame) null, "Hello World", false);
			dialog.setSize(new Dimension(600, 400));
			dialog.setLayout(new BorderLayout());
			tabs = new JTabbedPane(SwingConstants.LEFT, JTabbedPane.SCROLL_TAB_LAYOUT);
			final JPanel rightPanel = new JPanel();
			rightPanel.setLayout(new BoxLayout(rightPanel, BoxLayout.Y_AXIS));
			final JPanel stringPanel = new JPanel(new BorderLayout());
			stringPanel.setBorder(BorderFactory.createTitledBorder("String"));
			stringOption = new JTextField();
			stringOption.setEditable(false);
			stringPanel.add(stringOption);
			rightPanel.add(stringPanel);
			final JPanel integerPanel = new JPanel(new BorderLayout());
			integerPanel.setBorder(BorderFactory.createTitledBorder("Integer"));
			integerOption = new JTextField();
			integerOption.setEditable(false);
			integerPanel.add(integerOption);
			rightPanel.add(integerPanel);
			final JPanel booleanPanel = new JPanel(new BorderLayout());
			booleanPanel.setBorder(BorderFactory.createTitledBorder("Boolean"));
			booleanOption = new JCheckBox();
			booleanPanel.add(booleanOption);
			rightPanel.add(booleanPanel);
			final JSplitPane pane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, tabs, rightPanel);
			dialog.add(pane);
			dialog.setVisible(true);
			pane.setDividerLocation(0.66);

			final Scraper scraper = c.getExtension(Scraper.class);
			if (scraper != null) {
				scraper.addObserver(this);
			}
		} catch (final Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * @see java.util.Observer#update(java.util.Observable, java.lang.Object)
	 */
	@Override
	public void update(final Observable arg0, final Object arg1) {
		if (arg0 == this) {
			if (arg1 instanceof Option) {
				final Option<?> option = (Option<?>) arg1;
				if ("string".equals(option.getKey())) {
					stringOption.setText((String) getOption(option));
				} else if ("integer".equals(option.getKey())) {
					integerOption.setText("" + getOption(option));
				} else if ("boolean".equals(option.getKey())) {
					booleanOption.setSelected((Boolean) getOption(option));
				}
			}
		}
		if (arg0 instanceof Scraper && arg1 instanceof Scraper.Event) {
			final Scraper.Event event = (Scraper.Event) arg1;
			if (event.getElm() instanceof Page) {
				final Page p = (Page) event.getElm();
				switch (event.getType()) {
				case ADDED: {
					final DefaultListModel model = new DefaultListModel();
					final JList list = new JList(model);
					indexes.put(p, tabs.getTabCount());
					tabs.addTab(p.getName(), new JScrollPane(list));
					lists.put(p, model);
				}
				//$FALL-THROUGH$
				case CHANGED: {
					try {
						final DefaultListModel model = lists.get(p);
						tabs.setTitleAt(indexes.get(p), p.getName());
						final SortedSet<String> transitionNames = new TreeSet<String>();
						for (final Transition t : p.transitions()) {
							transitionNames.add(t.getName());
						}
						model.removeAllElements();
						for (final String name : transitionNames) {
							model.addElement(name);
						}
					} catch (final Exception e) {
						e.printStackTrace();
					}
				}
					break;
				case REMOVED:
					break;
				}
			}
		}
	}

}
