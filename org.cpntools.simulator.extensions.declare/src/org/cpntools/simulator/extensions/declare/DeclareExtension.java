package org.cpntools.simulator.extensions.declare;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import ltl2aut.automaton.AcceptabilityFlavor;
import ltl2aut.automaton.Automaton;
import ltl2aut.regexp.RegExp;

import org.cpntools.accesscpn.engine.protocol.Packet;
import org.cpntools.simulator.extensions.AbstractExtension;
import org.cpntools.simulator.extensions.Command;
import org.cpntools.simulator.extensions.Option;
import org.cpntools.simulator.extensions.server.Handler;

/**
 * @author michael
 */
public class DeclareExtension extends AbstractExtension {
	/**
	 * 
	 */
	public static final int ID = 10001;
	private final Map<String, Automaton> automata = new HashMap<String, Automaton>();
	@SuppressWarnings("unused")
	private final Option<Boolean> DATA_AWARE = Option.create("Data-aware simulation", "data_aware", Boolean.class);

	private final Map<String, Module> modules = new HashMap<String, Module>();

	private final Option<Boolean> SMART = Option.create("Smart simulation", "smart", Boolean.class);

	private final Map<String, Integer> states = new HashMap<String, Integer>();
	private final Map<String, Task> tasks = new HashMap<String, Task>();
	private final List<String> trace = new ArrayList<String>();

	/**
	 * 
	 */
	public DeclareExtension() {
		addOption(SMART);
// addOption(DATA_AWARE, SMART);
		addLazySubscription(// new Command(400, 2), // Syntax check page
// new Command(500, 3), // Generate instances
// new Command(500, 4), // Update instances
// new Command(500, 11), // Start run
		        new Command(500, 12), // Execute transition
		        new Command(500, 13), // Check transition for enabledness
		        new Command(500, 14), // Checked enabledness without scheduler
		        new Command(500, 15), // Manual binding
		        new Command(500, 20), // Init state
		        new Command(500, 21), // Create + reset scheduler
		        new Command(500, 35), // Check enabling of list of transitions
		        new Command(500, 36), // Check enabling of transitions without scheduler
		        new Command(800, 1) // Set state space options
		);
	}

	/**
	 * @see org.cpntools.simulator.extensions.Extension#getIdentifier()
	 */
	@Override
	public int getIdentifier() {
		return DeclareExtension.ID;
	}

	/**
	 * @see org.cpntools.simulator.extensions.Extension#getName()
	 */
	@Override
	public String getName() {
		return "Declare";
	}

	/**
	 * @see org.cpntools.simulator.extensions.Extension#handle(org.cpntools.accesscpn.engine.protocol.Packet)
	 */
	@Override
	public Packet handle(final Packet p) {
		makeLazySubscriptions();
		p.reset();
		final int command = p.getInteger();
		final int extension = p.getInteger();
		final int subcommand = p.getInteger();
		assert command == Handler.EXTERNAL_COMMAND;
		assert extension == DeclareExtension.ID;
		Packet result;
		switch (subcommand) {
		case 1:
			result = handleCheckPage(p);
			break;
		default:
			result = new Packet(7, -1);
			result.addString("Unknown Declare command");
			break;
		}
		return result;
	}

	/**
	 * @see org.cpntools.simulator.extensions.AbstractExtension#handle(org.cpntools.accesscpn.engine.protocol.Packet,
	 *      org.cpntools.accesscpn.engine.protocol.Packet)
	 */
	@Override
	public Packet handle(final Packet p, final Packet response) {
		p.reset();
		final int command = p.getInteger();
		if (command == 500) {
			final int subcommand = p.getInteger();
			switch (subcommand) {
			case 12:
				execute(p);
				return response;
			case 13:
			case 14:
				return enabled(p, response);
			case 20:
			case 21:
				reset();
				return response;
			case 35:
			case 36:
				return multipleEnabled(p, response);
			}
		}
		return null;
	}

	private boolean acceptable(final Automaton a, final int state, final Object transition) {
		final int next = execute(transition, a, state);
		if (next < 0) { return false; }
		return !AcceptabilityFlavor.isImpossible(a, next);
	}

	private Packet enabled(final Packet p, final Packet response) {
		response.reset();
		p.reset();
// System.out.println("-----------------------------------");
		if (response.getBoolean()) {
			final Packet result = new Packet(7, 1);
			result.addBoolean(enabled(p.getString(), p.getInteger()));
// System.out.println("-----------------------------------");
			return result;
		}
// System.out.println("Rejecting " + p.getString());
// System.out.println("-----------------------------------");
		return response;
	}

	private boolean enabled(final String string, final int integer) {
// System.out.print("Enabled? " + string + " - ");
		final Object task = getTask(string);
		for (final String pageId : new ArrayList<String>(automata.keySet())) {
			final Automaton a = automata.get(pageId);
			final int state = states.get(pageId);
// System.out.print(state + " ");
			if (!acceptable(a, state, task)) {
// System.out.println("= false");
				return false;
			}
		}
// System.out.println("= true");
		return true;
	}

	private void execute(final Packet p) {
		p.reset();
		final String taskId = p.getString();
// System.out.print(taskId + ": (");
		trace.add(taskId);
		final Object task = getTask(taskId);
// boolean first = true;
		for (final String pageId : new ArrayList<String>(automata.keySet())) {
			final Automaton a = automata.get(pageId);
			final int state = states.get(pageId);
			final int next = execute(task, a, state);
// System.out.print(state + " -> " + next);
// if (!first) {
// System.out.print(", ");
// }
// first = false;
			states.put(pageId, next);
		}
// System.out.println(")");
	}

	Object getTask(final String taskId) {
		Object task = tasks.get(taskId);
		if (task == null) {
			task = Automaton.OTHERWISE;
		}
		return task;
	}

	int execute(final Object task, final Automaton a, final int state) {
		int next = a.next(state, task);
		if (next < 0) {
			next = a.next(state, Automaton.OTHERWISE);
		}
		return next;
	}

	private Packet handleCheckPage(final Packet p) {
		final Packet result = new Packet(7, 1);
		try {
			p.reset();
			p.getInteger(); // command
			p.getInteger(); // extension
			p.getInteger(); // subcmd
			final int count = p.getInteger();
			final String pageId = p.getString();
			Module m = modules.get(pageId);
			if (m == null) {
				m = new Module();
				modules.put(pageId, m);
			}
			for (int i = 0; i < count; i++) {
				final int parameters = p.getInteger();
				final String id = p.getString();
				final String name = p.getString();
				final String formula = p.getString();
				p.getString(); // inscription
				final Constraint c = new Constraint(name, formula, parameters);
				for (int j = 0; j < parameters; j++) {
					final String tid = p.getString();
					Task t = tasks.get(tid);
					if (t == null) {
						t = new Task();
						t.setName(tid);
						tasks.put(tid, t);
					}
					c.setParameters(j, t);
				}
				m.addConstraint(id, c);
			}
			final int removeCount = p.getInteger();
			for (int i = 0; i < removeCount; i++) {
				m.removeConstraint(p.getString());
			}
			if (m.count() == 0) {
				modules.remove(pageId);
				states.remove(pageId);
				automata.remove(pageId);
				result.addBoolean(true);
				result.addInteger(0);
				return result;
			}
			final Map<RegExp<Task>, Constraint> formulae = Translator.INSTANCE.parse(m);
			final Automaton automaton = Translator.INSTANCE.translateRaw(formulae);
			Translator.INSTANCE.colorAutomaton(automaton);
			int state = automaton.getInit();
			for (final String taskId : trace) {
				final Object task = getTask(taskId);
				state = execute(task, automaton, state);
			}
			states.put(pageId, state);
			automata.put(pageId, automaton);
			result.addBoolean(true);
			result.addInteger(0);
// System.out.println(automaton);
		} catch (final Exception e) {
			e.printStackTrace();
			result.addBoolean(false);
			result.addString(e.toString());
		}
		return result;
	}

	private Packet multipleEnabled(final Packet p, final Packet response) {
		p.reset();
		response.reset();
		if (response.getInteger() != 1) { return response; }
		final Packet result = new Packet(7, 1);
		p.getInteger();
		p.getInteger(); // Skip command and subcmd
		final int count = p.getInteger();
		result.addInteger(count);
// System.out.println("=================================== " + count);
		for (int i = 0; i < count; i++) {
			if (response.getBoolean()) {
				result.addBoolean(enabled(p.getString(), p.getInteger()));
			} else {
				p.getString();
				p.getInteger();
				result.addBoolean(false);
// System.out.println("Rejecting " + taskId);
			}
			result.addString(response.getString());
		}
// System.out.println("=================================== " + count);
		return result;
	}

	private void reset() {
// System.out.println("Reset");
		trace.clear();
		for (final String page : new ArrayList<String>(modules.keySet())) {
			states.put(page, automata.get(page).getInit());
		}
	}

}
