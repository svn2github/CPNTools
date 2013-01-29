package org.cpntools.simulator.extensions.launcher;

import java.awt.HeadlessException;
import java.io.IOException;

import org.cpntools.simulator.extensions.server.DiscoveryServer;

/**
 * @author michael
 */
public class Launcher extends DiscoveryServer {
	/**
	 * @throws HeadlessException
	 * @throws IOException
	 * @throws InterruptedException
	 */
	public Launcher() throws HeadlessException, IOException, InterruptedException {
		super();
	}

	/**
	 * @param args
	 * @throws InterruptedException
	 * @throws IOException
	 * @throws HeadlessException
	 */
	public static void main(final String[] args) throws HeadlessException, IOException, InterruptedException {
		new Launcher();
	}

}
