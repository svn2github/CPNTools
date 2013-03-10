package org.cpntools.simulator.extensions;

/**
 * @author michael
 */
public class Command {
	/**
	 * 
	 */
	public static final int ANY = -1;
	private final int command, subcommand;
	private final boolean prefilter;

	/**
	 * @param command
	 */
	public Command(final int command) {
		this(command, Command.ANY);
	}

	/**
	 * @param command
	 * @param subcommand
	 */
	public Command(final int command, final int subcommand) {
		this(command, subcommand, false);
	}

	/**
	 * @param command
	 * @param subcommand
	 * @param prefilter
	 */
	public Command(final int command, final int subcommand, final boolean prefilter) {
		super();
		this.command = command;
		this.subcommand = subcommand;
		this.prefilter = prefilter;
	}

	/**
	 * @return
	 */
	public int getCommand() {
		return command;
	}

	/**
	 * @return
	 */
	public int getSubcommand() {
		return subcommand;
	}

	/**
	 * @return
	 */
	public boolean isPrefilter() {
		return prefilter;
	}
}
