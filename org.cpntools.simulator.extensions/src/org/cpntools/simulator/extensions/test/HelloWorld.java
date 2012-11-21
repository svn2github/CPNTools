package org.cpntools.simulator.extensions.test;

import java.util.Observable;
import java.util.Observer;

import org.cpntools.accesscpn.engine.protocol.Packet;
import org.cpntools.simulator.extensions.AbstractExtension;
import org.cpntools.simulator.extensions.Option;

/**
 * @author michael
 */
public class HelloWorld extends AbstractExtension implements Observer {
	/**
	 * 
	 */
	public static final int ID = 10000;

	/**
	 * 
	 */
	public HelloWorld() {
		addOption(Option.create("String", "string", String.class), Option.create("Integer", "integer", Integer.class),
		        Option.create("Boolean", "boolean", Boolean.class));

	}

	@Override
	public int getIdentifier() {
		return ID;
	}

	@Override
	public String getName() {
		return "Hello World";
	}

	@Override
	public Packet handle(final Packet p) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void update(final Observable arg0, final Object arg1) {
		if (arg0 == this) {
			if (arg1 instanceof Option) {
				System.out.println("Option changed - " + arg1 + ": " + getOption((Option<?>) arg1));
			}
		}

	}

}
