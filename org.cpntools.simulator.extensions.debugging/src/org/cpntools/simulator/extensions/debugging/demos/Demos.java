package org.cpntools.simulator.extensions.debugging.demos;

import java.util.Collection;

import javax.swing.JTabbedPane;
import javax.swing.SwingConstants;

import org.clapper.util.classutil.ClassInfo;
import org.cpntools.simulator.extensions.Channel;
import org.cpntools.simulator.extensions.debugging.DebuggingExtension;
import org.cpntools.simulator.extensions.debugging.DebuggingPanel;
import org.cpntools.simulator.extensions.utils.Discovery;

import dk.klafbang.tools.Pair;

/**
 * @author michael
 */
public class Demos extends DebuggingPanel {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private Collection<DemoPanel> extensions;

	/**
	 * 
	 */
	public Demos() {
		final JTabbedPane tabs = new JTabbedPane(SwingConstants.TOP, JTabbedPane.WRAP_TAB_LAYOUT);
		final Pair<Collection<ClassInfo>, Class<DemoPanel>> extensionDescriptors = Discovery
		        .findExtensions(DemoPanel.class);
		extensions = Discovery.instantiate(extensionDescriptors);
		for (final DemoPanel panel : extensions) {
			tabs.addTab(panel.getName(), panel);
		}
		add(tabs);
	}

	/**
	 * @see org.cpntools.simulator.extensions.debugging.DebuggingPanel#getName()
	 */
	@Override
	public String getName() {
		return "Demos";
	}

	/**
	 * @see org.cpntools.simulator.extensions.debugging.DebuggingPanel#setChannel(org.cpntools.simulator.extensions.debugging.DebuggingExtension,
	 *      org.cpntools.simulator.extensions.Channel)
	 */
	@Override
	public void setChannel(final DebuggingExtension orphanage, final Channel c) {
		super.setChannel(orphanage, c);
		if (extensions == null) { return; }
		for (final DemoPanel extension : extensions) {
			extension.setChannel(c);
		}
		if (c == null) { return; }
		extensions.clear();
		extensions = null;
	}
}
