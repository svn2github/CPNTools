package org.cpntools.simulator.extensions.server;

import org.cpntools.simulator.extensions.Extension;

/**
 * @author michael
 */
public class ConflictingExtensionsException extends ExtensionException {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private final Extension conflict;

	/**
	 * @param e
	 * @param extension
	 */
	public ConflictingExtensionsException(final Extension e, final Extension extension) {
		super("Conflicting with extension `" + extension.getName() + "'", e);
		conflict = extension;
	}

	public Extension getConflict() {
		return conflict;
	}

}
