package org.cpntools.simulator.extensions.server;

import java.io.IOException;

import org.cpntools.simulator.extensions.Extension;

/**
 * @author michael
 */
public class ErrorInjectingException extends ExtensionException {

	private final String code;
	private final String error;

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * @param extension
	 * @param code
	 * @param exception
	 */
	public ErrorInjectingException(final Extension extension, final String code, final IOException exception) {
		super("Error injecting code: `" + code + "'", extension, exception);
		this.code = code;
		error = null;
	}

	/**
	 * @param extension
	 * @param code
	 * @param error
	 */
	public ErrorInjectingException(final Extension extension, final String code, final String error) {
		super("Error injecting code: `" + code + "'", extension);
		this.code = code;
		this.error = error;
	}

	public String getCode() {
		return code;
	}

	public String getError() {
		return error;
	}

}
