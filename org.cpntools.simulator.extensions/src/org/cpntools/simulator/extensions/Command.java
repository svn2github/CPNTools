package org.cpntools.simulator.extensions;

public class Command {
	public static final int ANY = -1;
	private final int command, subcommand;

	public Command(final int command) {
		this(command, Command.ANY);
	}

	public Command(final int command, final int subcommand) {
		super();
		this.command = command;
		this.subcommand = subcommand;
	}

	public int getCommand() {
		return command;
	}

	public int getSubcommand() {
		return subcommand;
	}
}
