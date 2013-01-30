package org.cpntools.simulator.extensions.debugging;

import java.awt.BorderLayout;

import javax.swing.JPanel;

import org.cpntools.simulator.extensions.Channel;

/**
 * @author michael
 */
public abstract class DebuggingPanel extends JPanel {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	protected Channel channel;
	protected DebuggingExtension orphanage;

	public DebuggingPanel() {
		super(new BorderLayout());
	}

	/**
	 * @param orphanage
	 * @param c
	 */
	public void setChannel(final DebuggingExtension orphanage, final Channel c) {
		this.orphanage = orphanage;
		channel = c;
	}

	@Override
	public abstract String getName();

}
