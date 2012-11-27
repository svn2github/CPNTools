package org.cpntools.simulator.extensions.declare;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import ltl2aut.automaton.AcceptabilityFlavor;
import ltl2aut.automaton.Automaton;
import ltl2aut.formula.Formula;

import org.cpntools.accesscpn.engine.protocol.Packet;
import org.cpntools.simulator.extensions.AbstractExtension;
import org.cpntools.simulator.extensions.Command;
import org.cpntools.simulator.extensions.Option;
import org.cpntools.simulator.extensions.server.Handler;

/**
 * @author michael
 * @param <T>
 */
public class DeclareExtension extends AbstractExtension {
	public static final int ID = 10001;
	private final Option<Boolean> ON_THE_FLY = Option.create("On-the-fly", "on_the_fly", Boolean.class);

	public DeclareExtension() {
		addOption(ON_THE_FLY);
		addSubscription(new Command(400, 2), // Syntax check page
		        new Command(500, 3), // Generate instances
		        new Command(500, 4), // Update instances
		        new Command(500, 11), // Start run
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
	 * @see org.cpntools.simulator.extensions.Extension#handle(org.cpntools.accesscpn.engine.protocol.Packet)
	 */
	@Override
	public Packet handle(final Packet p) {
		p.reset();
		final int command = p.getInteger();
		final int extension = p.getInteger();
		final int subcommand = p.getInteger();
		assert command == Handler.EXTERNAL_COMMAND;
		assert extension == ID;
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

	private final Map<String, Task> tasks = new HashMap<String, Task>();
	private final Map<String, Module> modules = new HashMap<String, Module>();
	private final Map<String, Integer> states = new HashMap<String, Integer>();
	private final Map<String, Automaton> automata = new HashMap<String, Automaton>();

	private Packet handleCheckPage(final Packet p) {
		final Packet result = new Packet(7, 1);
		try {
			p.reset();
			p.getInteger(); // command
			p.getInteger(); // extension
			p.getInteger(); // subcmd
			final int count = p.getInteger();
			final String pageId = p.getString();
			if (count == 0) {
				modules.remove(pageId);
				states.remove(pageId);
				automata.remove(pageId);
				return result;
			}
			Module m = modules.get(pageId);
			if (m == null) {
				m = new Module();
				modules.put(pageId, m);
			}
			m.removeAllConstraints();
			for (int i = 0; i < count; i++) {
				final int parameters = p.getInteger();
				p.getString();
				final String name = p.getString();
				final String formula = p.getString();
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
				m.addConstraint(c);
			}
			final Map<Formula<Task>, Constraint> formulae = Translator.INSTANCE.parse(m);
			final Automaton automaton = Translator.INSTANCE.translateRaw(formulae);
			Translator.INSTANCE.colorAutomaton(automaton);
			states.put(pageId, automaton.getInit());
			automata.put(pageId, automaton);
			System.out.println(automaton);
		} catch (final Exception e) {
			result.addBoolean(false);
			result.addString(e.toString());
		}
		return result;
	}

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

	private void execute(final Packet p) {
		p.reset();
		Object task = tasks.get(p.getString());
		if (task == null) {
			task = Automaton.OTHERWISE;
		}
		for (final String pageId : new ArrayList<String>(automata.keySet())) {
			final Automaton a = automata.get(pageId);
			final int state = states.get(pageId);
			int next = a.next(state, task);
			if (next < 0) {
				next = a.next(state, Automaton.OTHERWISE);
			}
			states.put(pageId, next);
		}
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
		for (int i = 0; i < count; i++) {
			if (response.getBoolean()) {
				result.addBoolean(enabled(p.getString(), p.getInteger()));
			} else {
				result.addBoolean(false);
			}
			result.addString(response.getString());
		}
		return result;
	}

	private boolean acceptable(final Automaton a, final int state, final Object transition) {
		int next = a.next(state, transition);
		if (next < 0) {
			next = a.next(state, Automaton.OTHERWISE);
		}
		if (next < 0) { return false; }
		return !AcceptabilityFlavor.isImpossible(a, next);
	}

	private Packet enabled(final Packet p, final Packet response) {
		p.reset();
		if (p.getBoolean()) {
			final Packet result = new Packet(7, 1);
			result.addBoolean(enabled(p.getString(), p.getInteger()));
			return result;
		}
		return response;
	}

	private boolean enabled(final String string, final int integer) {
		Object task = tasks.get(string);
		if (task == null) {
			task = Automaton.OTHERWISE;
		}
		for (final String pageId : new ArrayList<String>(automata.keySet())) {
			final Automaton a = automata.get(pageId);
			final int state = states.get(pageId);
			if (!acceptable(a, state, task)) { return false; }
		}
		return true;
	}

	private void reset() {
		for (final String page : new ArrayList<String>(modules.keySet())) {
			states.put(page, automata.get(page).getInit());
		}
	}

	/**
	 * @see org.cpntools.simulator.extensions.Extension#getIdentifier()
	 */
	@Override
	public int getIdentifier() {
		return ID;
	}

	/**
	 * @see org.cpntools.simulator.extensions.Extension#getName()
	 */
	@Override
	public String getName() {
		return "Declare";
	}

}
