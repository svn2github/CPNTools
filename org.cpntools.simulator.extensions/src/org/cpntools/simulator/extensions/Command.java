package org.cpntools.simulator.extensions;

/**
 * @author michael
 */
public class Command {
	public static final int ANY = -1;
	private final int command, subcommand;
	private final boolean prefilter;

	public Command(final int command) {
		this(command, Command.ANY);
	}

	public Command(final int command, final int subcommand) {
		this(command, subcommand, false);
	}

	public Command(final int command, final int subcommand, final boolean prefilter) {
		super();
		this.command = command;
		this.subcommand = subcommand;
		this.prefilter = prefilter;
	}

	public int getCommand() {
		return command;
	}

	public int getSubcommand() {
		return subcommand;
	}

	public boolean isPrefilter() {
		return prefilter;
	}
}
