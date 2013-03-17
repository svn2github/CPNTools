package org.cpntools.simulator.extensions.launcher;

import java.awt.HeadlessException;
import java.io.File;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.GnuParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.apache.commons.cli.UnrecognizedOptionException;
import org.cpntools.simulator.extensions.server.DiscoveryServer;
import org.cpntools.simulator.extensions.utils.Discovery;

/**
 * @author michael
 */
public class Launcher extends DiscoveryServer {
	/**
	 * @param args
	 * @throws InterruptedException
	 * @throws IOException
	 * @throws HeadlessException
	 * @throws ParseException
	 * @throws ClassNotFoundException
	 * @throws NoSuchMethodException
	 * @throws InvocationTargetException
	 * @throws IllegalAccessException
	 * @throws InstantiationException
	 * @throws SecurityException
	 * @throws IllegalArgumentException
	 */
	public static void main(final String[] args) throws HeadlessException, IOException, InterruptedException,
	        ParseException, ClassNotFoundException, IllegalArgumentException, SecurityException,
	        InstantiationException, IllegalAccessException, InvocationTargetException, NoSuchMethodException {
		final File plugins = new File("plugins");
		if (plugins.isDirectory()) {
			for (final File f : plugins.listFiles()) {
				Discovery.addFile(f);
			}
		}

		final Options options = new Options();
		options.addOption("s", "silent", false, "silently quit on error");
		options.addOption("h", "help", false, "show this usage information");
		try {
			final CommandLine line = new GnuParser().parse(options, args);
			if (line.hasOption('h')) { throw new UnrecognizedOptionException("Show help"); }
			new Launcher(line.hasOption('s'));
		} catch (final UnrecognizedOptionException _) {
			new HelpFormatter().printHelp("java -jar SimulatorExtensions.jar", options, true);
		}
	}

	/**
	 * @param silent
	 * @throws HeadlessException
	 * @throws IOException
	 * @throws InterruptedException
	 */
	public Launcher(final boolean silent) throws HeadlessException, IOException, InterruptedException {
		super(silent);
	}

}
