package org.cpntools.simulator.extensions.debugging;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Frame;
import java.util.Collection;
import java.util.Observable;
import java.util.Observer;

import javax.swing.JDialog;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextArea;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingConstants;

import org.clapper.util.classutil.ClassInfo;
import org.cpntools.accesscpn.engine.protocol.Packet;
import org.cpntools.simulator.extensions.AbstractExtension;
import org.cpntools.simulator.extensions.Channel;
import org.cpntools.simulator.extensions.Option;
import org.cpntools.simulator.extensions.utils.Discovery;

import dk.klafbang.tools.Pair;

/**
 * @author michael
 */
public class DebuggingExtension extends AbstractExtension implements Observer {
	/**
	 * 
	 */
	public static final int ID = 10004;
	private JDialog dialog;
	private JTabbedPane tabs;
	private JTextArea packet;
	private Collection<DebuggingPanel> extensions;

	/**
	 * 
	 */
	public DebuggingExtension() {
		addOption(Option.create("Visible", "visible", Boolean.class));
		try {
			dialog = new JDialog((Frame) null, "Debug/CPN", false);
			dialog.setSize(new Dimension(600, 700));
			dialog.setLayout(new BorderLayout());
			tabs = new JTabbedPane(SwingConstants.TOP, JTabbedPane.WRAP_TAB_LAYOUT);
			packet = new JTextArea();
			packet.setEditable(false);
			final JScrollPane scroller = new JScrollPane(packet, ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS,
			        ScrollPaneConstants.HORIZONTAL_SCROLLBAR_ALWAYS);
			scroller.setMinimumSize(new Dimension(300, 150));
			final JSplitPane jSplitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, tabs, scroller);
			jSplitPane.setContinuousLayout(true);
			jSplitPane.setResizeWeight(1.0);
			dialog.add(jSplitPane);

			final Pair<Collection<ClassInfo>, Class<DebuggingPanel>> extensionDescriptors = Discovery
			        .findExtensions(DebuggingPanel.class);
			extensions = Discovery.instantiate(extensionDescriptors);
			for (final DebuggingPanel panel : extensions) {
				panel.setChannel(this, channel);
				tabs.addTab(panel.getName(), panel);
			}
			dialog.pack();
		} catch (final Exception e) {
			e.printStackTrace();
		}
		addObserver(this);
	}

	@Override
	public void setChannel(final Channel channel) {
		super.setChannel(channel);
		for (final DebuggingPanel panel : extensions) {
			panel.setChannel(this, channel);
		}
	}

	public void update(final Observable arg0, final Object arg1) {
		if (arg0 == this) {
			if (arg1 instanceof Option) {
				final Option<?> option = (Option<?>) arg1;
				if ("visible".equals(option.getKey())) {
					dialog.setVisible((Boolean) getOption(option));
				}
			}
		}
	}

	@Override
	public int getIdentifier() {
		return ID;
	}

	@Override
	public String getName() {
		return "Debugger";
	}

	@Override
	public Packet handle(final Packet p) {
		packet.setText(p.toString());
		return null;
	}

	@Override
	public Packet handle(final Packet p, final Packet r) {
		return null;
	}

}
