package org.cpntools.simulator.extensions.debugging.demos;

import java.awt.BorderLayout;

import javax.swing.JPanel;

import org.cpntools.simulator.extensions.Channel;

/**
 * @author michael
 */
public abstract class DemoPanel extends JPanel {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	protected Channel channel;

	public DemoPanel() {
		super(new BorderLayout());
	}

	/**
	 * @param orphanage
	 * @param c
	 */
	public void setChannel(final Channel c) {
		channel = c;
	}

	@Override
	public abstract String getName();
}
