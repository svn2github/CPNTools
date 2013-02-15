package org.cpntools.simulator.extensions.dcr;

import java.util.*;

public class DCRMarking {
	public HashSet<String> executed = new HashSet<String>();
	public HashSet<String> pending = new HashSet<String>();
	public HashSet<String> included = new HashSet<String>();
}
