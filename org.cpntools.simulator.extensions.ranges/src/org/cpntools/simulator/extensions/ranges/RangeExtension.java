package org.cpntools.simulator.extensions.ranges;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.cpntools.accesscpn.engine.protocol.Packet;
import org.cpntools.simulator.extensions.AbstractExtension;
import org.cpntools.simulator.extensions.Command;

/**
 * @author michael
 */
public class RangeExtension extends AbstractExtension {
	/**
	 * 
	 */
	public static final int ID = 10005;

	private static final Pattern constantRange = Pattern.compile("@ *\\[ *([0-9]+)* *, *([0-9]+) *\\]");

	private static final Pattern range = Pattern.compile("@ *(\\[[^\\],]*,[^\\]]*\\])");

	private int counter = 0;
	private final Map<String, String> types = new HashMap<String, String>();
	private final Map<String, String> variables = new HashMap<String, String>();
	boolean inited = false;

	/**
	 * 
	 */
	public RangeExtension() {
		addSubscription(new Command(400, 2, true) // Syntax check page
		);
	}

	/**
	 * @see org.cpntools.simulator.extensions.Extension#getIdentifier()
	 */
	@Override
	public int getIdentifier() {
		return RangeExtension.ID;
	}

	/**
	 * @see org.cpntools.simulator.extensions.Extension#getName()
	 */
	@Override
	public String getName() {
		return "Time Ranges";
	}

	/**
	 * @see org.cpntools.simulator.extensions.Extension#handle(org.cpntools.accesscpn.engine.protocol.Packet)
	 */
	@Override
	public Packet handle(final Packet p) {
		return null;
	}

	/**
	 * @see org.cpntools.simulator.extensions.AbstractExtension#prefilter(org.cpntools.accesscpn.engine.protocol.Packet)
	 */
	@Override
	public Packet prefilter(final Packet p) {
		p.reset();
		final int command = p.getInteger();
		if (command == 400) {
			final int subcommand = p.getInteger();
			switch (subcommand) {
			case 2:
				return filter(p);
			}
		}
		return null;
	}

	private String addToGuard(final String guard, final String string) {
		final String filtered = guard.trim();
		if (filtered.isEmpty()) { return "[" + string + "]"; }
		if (guard.charAt(0) == '[') { return "[" + string + ", " + guard.substring(1); }
		return "(" + string + ")::(" + guard + ")";
	}

	private String createConstRange(final String id, final int a, final int b) throws Exception {
		return createRange(id, "[" + a + ", " + b + "]");
	}

	private String createRange(final String id, @SuppressWarnings("hiding") final String range) throws Exception {
		delete(id);
		final int serial = counter++;
		Packet p = new Packet(300);
		p.addInteger(1); // check decl
		p.addBoolean(false); // timed
		p.addInteger(27); // intrange cs
		p.addInteger(1); // # vars
		p.addInteger(0); // # ms vars
		p.addInteger(0); // # aliases
		p.addInteger(0); // # declares
		p.addString(id + "_cs"); // id
		p.addString(range);
		final String type = "CPN'TIMERANGE'" + serial;
		p.addString(type); // name
		final String variable = "timerange" + serial;
		p.addString(variable); // var1
		p = channel.send(p);
		System.out.println(p);
		types.put(id, type);
		variables.put(id, variable);
		return variable;
	}

	private void delete(final String id) {
		types.remove(id);
		variables.remove(id);
	}

	private Packet filter(final Packet packet) {
		final Packet filtered = new Packet(packet.getOpcode(), 400);
		packet.reset();
		packet.getInteger(); // cmd
		filtered.addInteger(packet.getInteger()); // subcmd
		final String id = packet.getString();
		filtered.addString(id);
		final String name = packet.getString();
		filtered.addString(name);
		filtered.addInteger(packet.getInteger()); // Prime multiplicity
		filtered.addBoolean(packet.getBoolean()); // thisPage.isIncludedInSim
		int placeno = packet.getInteger();
		filtered.addInteger(placeno);
		for (int i = placeno; i > 0; i--) {
			filtered.addString(packet.getString());
		}

		int transno = packet.getInteger();
		final List<String> keepers = new ArrayList<String>();
		for (int i = transno; i > 0; i--) {
			keepers.add(packet.getString());
		}
		keep(keepers);
		filtered.addInteger(keepers.size());
		for (final String tid : keepers) {
			filtered.addString(tid);
		}

		placeno = packet.getInteger();
		filtered.addInteger(placeno);
		for (int i = placeno; i > 0; i--) {
			filtered.addString(packet.getString()); // id
			filtered.addString(packet.getString()); // name
			filtered.addString(packet.getString()); // colset
			filtered.addString(packet.getString()); // initmark
		}
		// Ignore places

		placeno = packet.getInteger();
		filtered.addInteger(placeno);
		for (int i = placeno; i > 0; i--) {
			filtered.addString(packet.getString()); // id
			filtered.addString(packet.getString()); // fusid
			filtered.addString(packet.getString()); // name
			filtered.addString(packet.getString()); // colset
			filtered.addString(packet.getString()); // initmark
		}
		// Ignore fusion places

		transno = packet.getInteger();
		filtered.addInteger(transno);
		for (int i = transno; i > 0; i--) {
			filtered.addString(packet.getString()); // id
			filtered.addString(packet.getString()); // name
			filtered.addString(packet.getString()); // subpageid
			final int portno = packet.getInteger();
			filtered.addInteger(portno);
			for (int j = portno; j > 0; j--) {
				filtered.addString(packet.getString()); // portid
				filtered.addString(packet.getString()); // socketid
			}
		}
		// Ignore subst transitions

		transno = packet.getInteger();
		filtered.addInteger(transno);
		for (int i = transno; i > 0; i--) {
			final String tid = packet.getString();
			filtered.addString(tid);
			filtered.addString(packet.getString()); // name
			final String guard = packet.getString();
			final String time = packet.getString();
			final String regionVariable = timeRegion(tid, time);
			if (regionVariable != null) {
				filtered.addString(addToGuard(
				        addToGuard(guard, "List.length (" + time.replaceFirst("@", "") + ") >= 0"), regionVariable
				                + " >= 0"));
				filtered.addString("@+" + regionVariable);
			} else {
				filtered.addString(guard);
				filtered.addString(time);
			}
			filtered.addString(packet.getString()); // code
			filtered.addString(packet.getString()); // channel
			filtered.addString(packet.getString()); // priority
			filtered.addBoolean(packet.getBoolean()); // controllable

			int arcno = packet.getInteger();
			filtered.addInteger(arcno);
			for (int j = arcno; j > 0; j--) { // input
				filtered.addString(packet.getString()); // arcid
				filtered.addString(packet.getString()); // placeid
				filtered.addString(packet.getString()); // expression
			}
			arcno = packet.getInteger();
			filtered.addInteger(arcno);
			for (int j = arcno; j > 0; j--) { // output
				final String aid = packet.getString();
				filtered.addString(aid); // arcid
				filtered.addString(packet.getString()); // placeid
				filtered.addString(timeArc(tid, aid, packet.getString())); // expression
			}
			arcno = packet.getInteger();
			filtered.addInteger(arcno);
			for (int j = arcno; j > 0; j--) { // inoutput
				filtered.addString(packet.getString()); // arcid
				filtered.addString(packet.getString()); // placeid
				filtered.addString(packet.getString()); // expression
			}
			arcno = packet.getInteger();
			filtered.addInteger(arcno);
			for (int j = arcno; j > 0; j--) { // inhibitor
				filtered.addString(packet.getString()); // arcid
				filtered.addString(packet.getString()); // placeid
				filtered.addString(packet.getString()); // expression
			}
			arcno = packet.getInteger();
			filtered.addInteger(arcno);
			for (int j = arcno; j > 0; j--) { // reset
				filtered.addString(packet.getString()); // arcid
				filtered.addString(packet.getString()); // placeid
				filtered.addString(packet.getString()); // expression
			}
		}

		return filtered;
	}

	private void keep(final List<String> keepers) {
		final Set<String> remove = new HashSet<String>(types.keySet());
		remove.removeAll(keepers);
		for (final String r : remove) {
			delete(r);
		}
	}

	private String timeArc(final String tid, final String aid, final String string) {
		String result = string;
		while (true) {
			final Matcher m = RangeExtension.range.matcher(result);
			if (m.find()) {
				try {
					final String variable = createRange(aid, m.group(1));
					if (variable != null) {
						result = m.replaceFirst("@+" + variable);
					} else {
						return result;
					}
				} catch (final Exception e) {
					return result;
				}
			} else {
				return result;
			}
		}
	}

	private String timeRegion(final String id, final String string) {
		final String time = string.trim();
		final Matcher n = RangeExtension.constantRange.matcher(time);
		if (n.matches()) {
			final int a = Integer.parseInt(n.group(1));
			final int b = Integer.parseInt(n.group(2));
			try {
				return createConstRange(id, a, b);
			} catch (final Exception e) { // Ignore
			}
		}
		final Matcher m = RangeExtension.range.matcher(time);
		if (m.matches()) {
			try {
				return createRange(id, m.group(1));
			} catch (final Exception e) { // Ignore
			}
		}
		return null;
	}

}
