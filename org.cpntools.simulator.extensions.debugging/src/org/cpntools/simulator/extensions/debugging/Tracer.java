package org.cpntools.simulator.extensions.debugging;

/**
 * @author michael
 */
public abstract class Tracer extends DebuggingPanel {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * 
	 */
	public Tracer() {
	}

	/**
	 * @see org.cpntools.simulator.extensions.debugging.DebuggingPanel#getName()
	 */
	@Override
	public String getName() {
		return "Tracer";
	}

}
