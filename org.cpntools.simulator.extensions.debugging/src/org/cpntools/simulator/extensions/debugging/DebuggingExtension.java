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
import org.cpntools.simulator.extensions.Instrument;
import org.cpntools.simulator.extensions.Invocation;
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
	private Collection<DebuggingPanel> extensions;
	private JTextArea packet;
	private JTabbedPane tabs;

	/**
	 * 
	 */
	public DebuggingExtension() {
		addInstrument(new Instrument("Development", "debug", "De-\nbug", "Start protocol debugger"));
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

	/**
	 * @see org.cpntools.simulator.extensions.Extension#getIdentifier()
	 */
	@Override
	public int getIdentifier() {
		return DebuggingExtension.ID;
	}

	/**
	 * @see org.cpntools.simulator.extensions.Extension#getName()
	 */
	@Override
	public String getName() {
		return "Debugger";
	}

	/**
	 * @see org.cpntools.simulator.extensions.Extension#handle(org.cpntools.accesscpn.engine.protocol.Packet)
	 */
	@Override
	public Packet handle(final Packet p) {
		packet.setText(p.toString());
		return null;
	}

	/**
	 * @see org.cpntools.simulator.extensions.AbstractExtension#handle(org.cpntools.accesscpn.engine.protocol.Packet,
	 *      org.cpntools.accesscpn.engine.protocol.Packet)
	 */
	@Override
	public Packet handle(final Packet p, final Packet r) {
		return null;
	}

	/**
	 * @see org.cpntools.simulator.extensions.AbstractExtension#setChannel(org.cpntools.simulator.extensions.Channel)
	 */
	@Override
	public void setChannel(final Channel channel) {
		super.setChannel(channel);
		for (final DebuggingPanel panel : extensions) {
			panel.setChannel(this, channel);
		}
	}

	/**
	 * @see java.util.Observer#update(java.util.Observable, java.lang.Object)
	 */
	@Override
	public void update(final Observable arg0, final Object arg1) {
		if (arg0 == this) {
			if (arg1 instanceof Invocation) {
				final Invocation i = (Invocation) arg1;
				if ("debug".equals(i.getInstrument().getKey())) {
					dialog.setVisible(true);
				}
			}
		}
	}

}
