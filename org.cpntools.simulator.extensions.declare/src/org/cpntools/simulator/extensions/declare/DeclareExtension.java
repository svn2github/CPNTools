package org.cpntools.simulator.extensions.declare;

import org.cpntools.accesscpn.engine.protocol.Packet;
import org.cpntools.simulator.extensions.AbstractExtension;
import org.cpntools.simulator.extensions.Command;
import org.cpntools.simulator.extensions.Option;

/**
 * @author michael
 * @param <T>
 */
public class DeclareExtension extends AbstractExtension {
	public static final int ID = 10001;
	private final Option<Boolean> ON_THE_FLY = Option.create("On-the-fly", "on_the_fly", Boolean.class);

	public DeclareExtension() {
		addOption(ON_THE_FLY);
		addSubscription(new Command(400, 2), // Syntax check page
		        new Command(500, 3), // Generate instances
		        new Command(500, 4), // Update instances
		        new Command(500, 11), // Start run
		        new Command(500, 13), // Check transition for enabledness
		        new Command(500, 14), // Checked enabledness without scheduler
		        new Command(500, 15), // Manual binding
		        new Command(500, 20), // Init state
		        new Command(500, 21), // Create + reset scheduler
		        new Command(500, 35), // Check enabling of list of transitions
		        new Command(500, 36), // Check enabling of transitions without scheduler
		        new Command(800, 1) // Set state space options
		);
	}

	/**
	 * @see org.cpntools.simulator.extensions.Extension#handle(org.cpntools.accesscpn.engine.protocol.Packet)
	 */
	@Override
	public Packet handle(final Packet p) {
// System.out.println("Handling " + p);
		return null;
	}

	@Override
	public Packet handle(final Packet p, final Packet response) {
// System.out.println("Scraping " + p);
		return null;
	}

	/**
	 * @see org.cpntools.simulator.extensions.Extension#getIdentifier()
	 */
	@Override
	public int getIdentifier() {
		return ID;
	}

	/**
	 * @see org.cpntools.simulator.extensions.Extension#getName()
	 */
	@Override
	public String getName() {
		return "Declare";
	}

}
