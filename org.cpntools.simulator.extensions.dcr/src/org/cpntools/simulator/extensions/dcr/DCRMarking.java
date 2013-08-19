package org.cpntools.simulator.extensions.dcr;

import java.util.HashSet;

public class DCRMarking {
	public HashSet<String> executed = new HashSet<String>();
	public HashSet<String> included = new HashSet<String>();
	public HashSet<String> pending = new HashSet<String>();

	@Override
	public String toString() {
		final StringBuilder result = new StringBuilder();
		final String NEW_LINE = System.getProperty("line.separator");

		result.append(this.getClass().getName() + " DCR marking {" + NEW_LINE);
		result.append(" Executed: ");
		for (final String e : executed) {
			result.append(e + "; ");
		}
		result.append(NEW_LINE);

		result.append(" Pending: ");
		for (final String e : pending) {
			result.append(e + "; ");
		}
		result.append(NEW_LINE);

		result.append(" Included: ");
		for (final String e : included) {
			result.append(e + "; ");
		}
		result.append(NEW_LINE);

		return result.toString();
	}
}
