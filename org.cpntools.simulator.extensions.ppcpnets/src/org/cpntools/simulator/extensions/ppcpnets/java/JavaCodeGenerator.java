package org.cpntools.simulator.extensions.ppcpnets.java;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Observable;
import java.util.Observer;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.cpntools.simulator.extensions.AbstractExtension;
import org.cpntools.simulator.extensions.Instrument;
import org.cpntools.simulator.extensions.Invocation;
import org.cpntools.simulator.extensions.NamedRPCHandler;
import org.cpntools.simulator.extensions.ppcpnets.PPCPNetChecker;
import org.cpntools.simulator.extensions.ppcpnets.java.CFGNode.Kind;
import org.cpntools.simulator.extensions.scraper.Arc;
import org.cpntools.simulator.extensions.scraper.Page;
import org.cpntools.simulator.extensions.scraper.Place;
import org.cpntools.simulator.extensions.scraper.Transition;
import org.cpntools.simulator.extensions.scraper.types.Other;
import org.cpntools.simulator.extensions.scraper.types.Type;

import dk.klafbang.tools.Pair;

/**
 * @author michael
 */
public class JavaCodeGenerator extends AbstractExtension implements Observer {
	/**
	 * @author michael
	 */
	public static class Handler implements NamedRPCHandler {
		String text;

		/**
		 * @param text
		 */
		public void register(@SuppressWarnings("hiding") final String text) {
			this.text = text;
		}

		/**
		 * @see org.cpntools.simulator.extensions.NamedRPCHandler#structureName()
		 */
		@Override
		public String structureName() {
			return "CPN'PPCPNetHandler";
		}
	}

	private static final int ID = 10008;
	private static final Pattern UNIT_PATTERN = Pattern
	        .compile("\\p{Space}*+(1\\p{Space}*`\\p{Space}*)?+\\(?+\\p{Space}*+(.*)\\p{Space}*+\\)?+\\p{Space}*");

	private final Instrument exportInstrument;

	private final Handler handler = new Handler();

	Pattern initUnit = Pattern.compile("([0-9]+)`\\(\\)");

	/**
	 * 
	 */
	public JavaCodeGenerator() {
		exportInstrument = new Instrument(Instrument.ToolBoxes.NET, "export_java", "Java", "Export model as Java code");
		addInstrument(exportInstrument);
		addObserver(this);
	}

	/**
	 * @see org.cpntools.simulator.extensions.SubscriptionHandler#getIdentifier()
	 */
	@Override
	public int getIdentifier() {
		return JavaCodeGenerator.ID;
	}

	/**
	 * @see org.cpntools.simulator.extensions.Extension#getName()
	 */
	@Override
	public String getName() {
		return "PP-CPN (Java)";
	}

	/**
	 * @see org.cpntools.simulator.extensions.AbstractExtension#getRPCHandler()
	 */
	@Override
	public Object getRPCHandler() {
		return handler;
	}

	/**
	 * @see java.util.Observer#update(java.util.Observable, java.lang.Object)
	 */
	@Override
	public void update(final Observable arg0, final Object arg1) {
		if (arg0 == this) {
			if (arg1 instanceof Invocation) {
				final Invocation i = (Invocation) arg1;
				if (i.getInstrument() == exportInstrument) {
					final PPCPNetChecker checker = channel.getExtension(PPCPNetChecker.class);
					if (checker != null) {
						final boolean wasEnabled = checker.enable();

// System.out.println(checker.partition());
// System.out.println(checker.sharedPlaces());
// System.out.println(checker.channelPlaces());
						final ControlFlowGraph cfg = buildCFG(checker.partition(), checker.sharedPlaces(),
						        checker.channelPlaces(), checker.resourcePlaces(), checker);
						checkVariables(cfg, checker);
						final AbstractSyntaxTree ast = buildAST(cfg, checker.getTypes(), checker);
						emit(ast);
						System.out
						        .println("-----------------------------------------------------------------------------");
						simplyExpressions(ast);
						simplyJumps(ast);
						simplifyVariables(ast);
						removeUnusedTemps(ast);
						addControl(ast);
						emit(ast);
						if (!wasEnabled) {
							checker.disable();
						}
					}
				}
			}
		}
	}

	private void addControl(final AbstractSyntaxTree ast) {
		new ControlFreak().visit(ast);
	}

	private AbstractSyntaxTree buildAST(final ControlFlowGraph cfg, final Map<String, Type> types,
	        final PPCPNetChecker checker) {
		final AbstractSyntaxTree ast = new AbstractSyntaxTree();
		ast.setName("GeneratedCode");
		final List<Pair<Launch, Pair<CFGNode, Map<String, Variable>>>> launches = new ArrayList<Pair<Launch, Pair<CFGNode, Map<String, Variable>>>>();
		final Map<String, Process> processes = new HashMap<String, Process>();
		for (final Entry<String, Pair<List<String>, CFGNode>> process : cfg.getProcesses().entrySet()) {
			final Map<CFGNode, Label> translations = new HashMap<CFGNode, Label>();
			final Map<String, Variable> temporaries = new TreeMap<String, Variable>();
			final Map<String, Variable> locals = new TreeMap<String, Variable>();
			final Map<String, Variable> parameters = new TreeMap<String, Variable>();
			final Set<Lock> locks = new TreeSet<Lock>();
			ASTNode entry = translate(process.getValue().getSecond(), translations, new HashMap<CFGNode, Expression>(),
			        types, checker, temporaries, locals, parameters, locks, process.getValue().getFirst(), launches);
			for (final Entry<String, Variable> e : temporaries.entrySet()) {
				entry = new Declaration(entry, e.getValue());
			}
			final Process p = new Process(process.getKey(), entry, new ArrayList<Variable>(locals.values()),
			        new ArrayList<Variable>(parameters.values()), new ArrayList<Lock>(locks), process.getValue()
			                .getFirst());
			processes.put(process.getKey(), p);
			ast.addProcess(p);
		}
		for (final Pair<Launch, Pair<CFGNode, Map<String, Variable>>> pair : launches) {
			final Process process = processes.get(pair.getFirst().getProcedure());
// System.out.println(pair.getFirst().getProcedure());
			final List<String> ids = new ArrayList<String>();
			ASTNode next = pair.getFirst().getNext();
			final List<Variable> variables = new ArrayList<Variable>();
			for (final Variable v : process.getParameters()) {
				ids.add(v.getId());
				variables.add(v);
			}
			for (final Variable v : process.getLocals()) {
				ids.add(v.getId());
				variables.add(v);
			}
			final Iterator<Variable> vi = variables.iterator();
			for (final Assignment a : pair.getSecond().getFirst().getAssignments(ids)) {
				if (a != null) {
					pair.getFirst().getParameters().add(new VariableExpression(a.getVariable()));
					if (a.getVariable() instanceof Local) {
						next = new AssignmentExp(next, a.getVariable(), new VariableExpression(new ProcessVariable(
						        pair.getFirst(), vi.next())));
					} else {
						vi.next();
					}
				} else {
					pair.getFirst().getParameters().add(new Whatever("/* TODO */"));
					vi.next();
				}
			}
			pair.getFirst().setNext(next);
			assert !vi.hasNext();
// System.out.println(ids);
// System.out.println(pair.getSecond().getFirst().getAssignmentOrder());
// System.out.println(pair.getSecond().getSecond());
// System.out.println(pair.getSecond().getFirst().getAssignments(ids));
		}
		return ast;
	}

	private ControlFlowGraph buildCFG(final Iterable<Set<Place>> partition, final Iterable<Place> sharedPlaces,
	        final Iterable<Place> channelPlaces, final Iterable<Place> resourcePlaces, final PPCPNetChecker checker) {
		final Map<String, Global> shared = ensureUniqueNames(buildShared(sharedPlaces, checker.getTypes()));
		final Map<String, Channel> channels = ensureUniqueNames(buildChannels(channelPlaces, checker.getTypes()));
		final Map<String, Lock> resources = ensureUniqueNames(buildResources(resourcePlaces));
		final Map<String, Pair<List<String>, CFGNode>> processes = new HashMap<String, Pair<List<String>, CFGNode>>();
		final Map<String, Pair<String, CFGNode>> procedures = new HashMap<String, Pair<String, CFGNode>>();
		final List<Pair<CFGNode, Pair<String, Map<String, String>>>> substitutions = new ArrayList<Pair<CFGNode, Pair<String, Map<String, String>>>>();
		for (final Set<Place> p : partition) {
			assert !p.isEmpty();
			final String type = p.iterator().next().getType();
			final Page page = p.iterator().next().getPage();
			String name;
			if (page.isPrime()) {
				name = new Other(type).getJavaName();
			} else {
				name = new Other(page.getName()).getJavaName();
			}

			CFGNode init = null;
			while (processes.containsKey(name)) {
				name = Variable.nextName(name);
			}
			final HashMap<String, CFGNode> nodes = new HashMap<String, CFGNode>();
			final Iterable<Transition> t = checker.transitionsOf(p);
			final List<String> ids = new ArrayList<String>();
			for (final Transition transition : t) {
				final Pair<String, Place> predecessor = checker.prePlace(transition);
				assert checker.isVariable(predecessor.getSecond().getType(), predecessor.getFirst());
				final CFGNode node = new CFGNode(transition.getName(), transition.getGuard(), predecessor.getFirst(),
				        predecessor.getSecond().getType());
				if (!predecessor.getSecond().isSocket()
				        && (!"".equals(predecessor.getSecond().getInitMark()) || !predecessor.getSecond().in()
				                .iterator().hasNext())) {
					ids.addAll(splitInitial(predecessor.getSecond().getType(), predecessor.getSecond().getInitMark()));
					assert init == null;
					init = node;
				}
				nodes.put(transition.getId(), node);
			}
			assert init != null;
			final Map<String, Local> locals = ensureUniqueNames(buildLocals(type, checker.localPlacesOf(t),
			        checker.getTypes(), checker, ids));
			processes.put(name, Pair.createPair(ids, init));
			procedures.put(page.getId(), Pair.createPair(name, init));
			for (final Transition transition : t) {
				checker.prePlace(transition);
				final CFGNode node = nodes.get(transition.getId());
				if (transition.isSubstitution()) {
					node.setKind(Kind.INVOKE);
					substitutions.add(Pair.createPair(node,
					        Pair.createPair(transition.getSubpage(), transition.getPortSocket())));
				}
				final Map<String, String> in = new HashMap<String, String>();
				final Map<String, Pair<Place, String>> out = new HashMap<String, Pair<Place, String>>();
				for (final Arc a : transition.in()) {
					final String old = in.put(a.getPlace().getId(), a.getInscription());
					assert old == null;
				}
				for (final Arc a : transition.out()) {
					final Pair<Place, String> old = out.put(a.getPlace().getId(),
					        Pair.createPair(a.getPlace(), a.getInscription()));
					assert old == null;
				}
				for (final String id : new ArrayList<String>(in.keySet())) {
					if (locals.containsKey(id)) {
						final Pair<Place, String> outExp = out.remove(id);
						final String inExp = in.remove(id);
						assert outExp != null;
						node.addAssignment(inExp, outExp.getSecond(), locals.get(id));
					} else if (shared.containsKey(id)) {
						final Pair<Place, String> outExp = out.remove(id);
						final String inExp = in.remove(id);
						assert outExp != null;
						node.addAssignment(inExp, outExp.getSecond(), shared.get(id));
					} else if (channels.containsKey(id)) {
						assert !out.containsKey(id);
						node.receive(channels.get(id), in.remove(id));
					} else if (resources.containsKey(id)) {
						node.addLock(resources.get(id));
						final String expr = in.remove(id);
						assert extractCount(expr) == 1;
					} else {
						assert checker.isProcessPlace(id);
					}
				}
				assert in.size() == p.size();
				for (final String id : new ArrayList<String>(out.keySet())) {
					if (channels.containsKey(id)) {
						assert !in.containsKey(id);
						node.send(channels.get(id), in.remove(id));
					} else if (resources.containsKey(id)) {
						node.addUnlock(resources.get(id));
					} else {
						final Pair<Place, String> pair = out.remove(id);
						for (final Arc a : pair.getFirst().out()) {
							final CFGNode successor = nodes.get(a.getTransition().getId());
							if (successor != null) {
								node.addSuccessor(pair.getSecond(), successor, a.getTransition().getId());
							}
						}
					}
				}
			}
		}
		for (final Pair<CFGNode, Pair<String, Map<String, String>>> pair : substitutions) {
			pair.getFirst().setMethod(procedures.get(pair.getSecond().getFirst()).getFirst(),
			        procedures.get(pair.getSecond().getFirst()).getSecond());
			new ArrayList<String>();
			pair.getFirst().setAssignmentOrder(pair.getSecond().getSecond());
		}
		return new ControlFlowGraph(shared.values(), channels.values(), processes);
	}

	private Map<String, Channel> buildChannels(final Iterable<Place> places, final Map<String, Type> types) {
		final Map<String, Channel> result = new HashMap<String, Channel>();
		for (final Place p : places) {
			result.put(p.getId(), new Channel(p.getId(), p.getName(), types.get(p.getType()), p.getType()));
			assert "".equals(p.getInitMark());
		}
		return result;
	}

	private Expression buildGuard(final String variable, final String guard, final String type,
	        final PPCPNetChecker checker) {
		if (guard.matches("\\p{Space}*+(1\\p{Space}*`\\p{Space}*)?+\\(?+\\p{Space}*+" + variable
		        + "\\p{Space}*+\\)?+\\p{Space}*")) { return new True(); }
		if ("UNIT".equals(type)
		        && ("()".equals(guard) || "1`()".equals(guard) || checker.isVariable("UNIT", guard.trim()))) { return new True(); }

		Matcher m = Pattern.compile(
		        "\\p{Space}*if\\p{Space}+(.*)\\p{Space}*then\\p{Space}+1\\p{Space}*`\\p{Space}*\\(?(\\(\\)|" + variable
		                + ")\\)?\\p{Space}+else\\p{Space}+empty\\p{Space}*").matcher(guard);
		if (m.matches()) { return new Whatever(m.group(1)); }
		m = Pattern
		        .compile(
		                "\\p{Space}*if\\p{Space}+(.*)\\p{Space}*then\\p{Space}+empty\\p{Space}+else\\p{Space}+1\\p{Space}*`\\p{Space}*\\(?(\\(\\)|"
		                        + variable + ")\\)?\\p{Space}*").matcher(guard);
		if (m.matches()) { return new Not(new Whatever(m.group(1))); }
		return new Equal(new Whatever(variable), new Whatever(guard));
	}

	@SuppressWarnings("null")
	private Map<String, Local> buildLocals(final String processType, final Iterable<Place> places,
	        final Map<String, Type> types, final PPCPNetChecker checker, final List<String> ids) {
		final Map<String, Local> result = new HashMap<String, Local>();
		for (final Place p : places) {
			final List<String> product = checker.getProduct(p.getType());
			assert product != null && product.size() == 2;
			final List<String> first = splitInitial(product.get(0), "List.map (fn (a, b) => a) (" + p.getInitMark()
			        + ")");
			final List<String> second = splitInitial(product.get(1), "List.map (fn (a, b) => b) (" + p.getInitMark()
			        + ")");
			assert first.size() == second.size() && first.size() == ids.size();
			List<String> keys = null, values = null;
			if (processType.equals(product.get(0)) && first.containsAll(ids)) {
				keys = first;
				values = second;
			}
			if (processType.equals(product.get(1)) && second.containsAll(ids)) {
				keys = second;
				values = first;
			}
			assert keys != null && values != null;
			final Map<String, String> map = new HashMap<String, String>();
			final Iterator<String> vi = values.iterator();
			for (final String key : keys) {
				map.put(key, vi.next());
			}
			assert !vi.hasNext();
			result.put(p.getId(), new Local(p.getId(), p.getName(), types.get(p.getType()), p.getType(), map));
		}
		return result;
	}

	private Map<String, Lock> buildResources(final Iterable<Place> places) {
		final Map<String, Lock> result = new HashMap<String, Lock>();
		for (final Place p : places) {
			result.put(p.getId(), new Lock(p.getName(), extractCount(p.getInitMark())));
		}
		return result;
	}

	private Map<String, Global> buildShared(final Iterable<Place> places, final Map<String, Type> types) {
		final Map<String, Global> result = new HashMap<String, Global>();
		for (final Place p : places) {
			result.put(p.getId(),
			        new Global(p.getId(), p.getName(), types.get(p.getType()), p.getType(), p.getInitMark()));
		}
		return result;
	}

	private void checkVariables(final ControlFlowGraph cfg, final PPCPNetChecker checker) {
		// TODO
	}

	private Variable createLocal(final String pvariable, final Variable variable, final PPCPNetChecker checker,
	        final Map<String, String> values) {
		final String typeName = getTypeName(pvariable, variable, checker);
		final Type type = checker.getTypes().get(typeName);
		assert type != null && typeName != null;
		return new Local(variable.getId(), variable.getName(), type, typeName, values);
	}

	private Channel createReadChannel(final String pvariable, final Channel variable, final PPCPNetChecker checker) {
		final String typeName = getTypeName(pvariable, variable, checker);
		final Type type = checker.getTypes().get(typeName);
		assert type != null && typeName != null;
		return new ReceiveChannel(variable.getId(), variable.getName(), type, typeName);
	}

	private Variable createSendChannel(final Channel key) {
		return new SendChannel(key.getId(), key.getName(), key.getType(), key.getOriginalType());
	}

	private Temporary createTemporary(final String pvariable, final Variable variable, final PPCPNetChecker checker,
	        final String name) {
		final String typeName = getTypeName(pvariable, variable, checker);
		final Type type = checker.getTypes().get(typeName);
		assert type != null && typeName != null;
		return new Temporary(name, name, type, typeName);
	}

	private void emit(final AbstractSyntaxTree ast) {
		new JavaVisitor(System.out).visit(ast);
	}

	private <T extends HasJavaName> Map<String, T> ensureUniqueNames(final Map<String, T> variables) {
		final Map<String, T> names = new HashMap<String, T>();
		for (final T global : variables.values()) {
			names.put(global.getJavaName(), global);
		}
		if (names.size() == variables.size()) { return variables; }
		for (final T g : variables.values()) {
			String name = g.getJavaName();
			if (names.get(name) != g) {
				while (names.containsKey(name)) {
					name = Variable.nextName(name);
				}
				g.setName(name);
				names.put(name, g);
			}
		}
		assert names.size() == variables.size();
		return variables;
	}

	private int extractCount(final String initMark) {
		assert initMark != null;
		final String i = initMark.trim();
		if ("()".equals(i)) { return 1; }
		final Matcher m = initUnit.matcher(i);
		if (!m.matches()) { return 1; }
		return Integer.parseInt(m.group(1));
	}

	private String getTypeName(final String pvariable, final Variable variable, final PPCPNetChecker checker) {
		final List<String> product = checker.getProduct(variable.getOriginalType());
		if (product == null) { return variable.getOriginalType(); }
		assert product.size() == 2;
		if (checker.isVariable(product.get(0), pvariable)) { return product.get(1); }
		if (checker.isVariable(product.get(1), pvariable)) { return product.get(0); }
		if (checker.isProcessType(product.get(0)) && !checker.isProcessType(product.get(1))) { return product.get(1); }
		if (checker.isProcessType(product.get(1)) && !checker.isProcessType(product.get(0))) { return product.get(0); }
		if (checker.isProcessType(product.get(0))) { return product.get(1); }
		if (checker.isProcessType(product.get(1))) { return product.get(0); }
		assert false;
		return null;
	}

	private String getVariable(final String process, final String expr, final String processType,
	        final String placeType, final PPCPNetChecker checker, final boolean isLocal, final boolean isVariable) {
		final String variable = getVariable2(process, expr, processType, placeType, checker, isLocal, isVariable);
// if (variable == null) {
// System.out.println("Variable: " + process + " - " + expr + " - " + processType + " - " + placeType + " - "
// + isLocal + " - " + isVariable + " - " + variable);
// }
		return variable;
	}

	private String getVariable2(final String process, final String expr, final String processType,
	        final String placeType, final PPCPNetChecker checker, final boolean isLocal, final boolean isVariable) {
		String variable = null;
		variable = matchesProduct(process, expr, processType, placeType, checker, isVariable);
		if (variable != null) { return variable; }
		variable = matchesProduct("\\(\\)", expr, processType, placeType, checker, isVariable);
		if (variable != null) { return variable; }
		variable = matchesProduct("1\\p{Space}*`\\p{Space}*\\(\\)", expr, processType, placeType, checker, isVariable);
		if (variable != null) { return variable; }
		if (!isLocal || "UNIT".equals(processType)) {
			final Matcher m = JavaCodeGenerator.UNIT_PATTERN.matcher(expr);
			if (m.matches()) {
				variable = m.group(2);
				if (!isVariable || checker.isVariable(placeType, variable)) { return variable; }
			}
		}
		return null;
	}

	private String matchesProduct(final String process, final String expr, final String processType,
	        final String placeType, final PPCPNetChecker checker, final boolean isVariable) {
		String variable = null;
		final Matcher m1 = Pattern.compile(
		        "\\p{Space}*\\(\\p{Space}*" + process
		                + "\\p{Space}*,\\p{Space}*([^\\p{Space}]*)\\p{Space}*\\)\\p{Space}*").matcher(expr);
		if (m1.matches()) {
			variable = m1.group(1);
		}
		final Matcher m2 = Pattern.compile(
		        "\\p{Space}*\\(\\p{Space}*([^\\p{Space}]*)\\p{Space}*,\\p{Space}*" + process
		                + "\\p{Space}*\\)\\p{Space}*").matcher(expr);
		if (m2.matches()) {
			variable = m2.group(2);
		}
		if (variable != null) {
			variable = variable.trim();
			final List<String> product = checker.getProduct(placeType);
			if (product != null && product.size() == 2) {
				if (product.get(0).equals(processType) && (!isVariable || checker.isVariable(product.get(1), variable))) { return variable; }
				if (product.get(1).equals(processType) && (!isVariable || checker.isVariable(product.get(0), variable))) { return variable; }
			}
		}
		return null;
	}

	private void removeUnusedTemps(final AbstractSyntaxTree ast) {
		new RemoveUnusedTemporaries().visit(ast);
	}

	private void simplifyVariables(final AbstractSyntaxTree ast) {
		new SimplifyVariables().visit(ast);
	}

	private void simplyExpressions(final AbstractSyntaxTree ast) {
		new SimplifyExpressions().visit(ast);
	}

	private void simplyJumps(final AbstractSyntaxTree ast) {
		new SimplifyJumps().visit(ast);
	}

	private List<String> splitInitial(final String type, final String value) {
		if ("".equals(value)) { return Collections.emptyList(); }
		handler.text = null;
		try {
			channel.evaluate("val _ = CPN'PPCPNetHandler.register(ListFormat.fmt { init = \"\", sep = \"\\n\", final = \"\", fmt = "
			        + type + ".mkstr } (" + value + "))");
		} catch (final Exception e) {
			try {
				channel.evaluate("val _ = CPN'PPCPNetHandler.register(" + type + ".mkstr (" + value + "))");
			} catch (final Exception e1) {
				return Collections.emptyList();
			}
		}
		if (handler.text == null) { return Collections.emptyList(); }
		final List<String> result = new ArrayList<String>();
		final StringTokenizer strtok = new StringTokenizer(handler.text, "\n");
		while (strtok.hasMoreElements()) {
			result.add(strtok.nextToken().trim());
		}
		return result;
	}

	private Label translate(final CFGNode value, final Map<CFGNode, Label> translations,
	        final Map<CFGNode, Expression> guards, final Map<String, Type> types, final PPCPNetChecker checker,
	        final Map<String, Variable> temporaries, final Map<String, Variable> locals,
	        final Map<String, Variable> parameters, final Set<Lock> locks, final List<String> processIDs,
	        final List<Pair<Launch, Pair<CFGNode, Map<String, Variable>>>> launches) {
		if (translations.containsKey(value)) { return translations.get(value); }

		if (value.getKind() == Kind.NORMAL) {
			ASTNode code = new Skip(null);
			ASTNode first = code;
			guards.put(value, new Whatever(value.getGuard()));

			for (final Lock l : value.unlocks()) {
				locks.add(l);
				code = new ReleaseLock(code, l);
			}

			for (final Entry<Channel, String> e : value.sends()) {
				parameters.put("channel." + e.getKey().getJavaName(), createSendChannel(e.getKey()));
				code = new Send(code, e.getKey(), new Whatever(e.getValue()));
			}

			for (final Assignment a : value.assignments()) {
				if (a.getVariable() instanceof Local) {
					locals.put(
					        a.getVariable().getJavaName(),
					        createLocal(value.getVariable(), a.getVariable(), checker,
					                ((Local) a.getVariable()).getInitialValues()));
				} else {
					parameters.put("globals." + a.getVariable().getJavaName(), a.getVariable());
				}
				final String expr = getVariable(value.getVariable(), a.getWrite(), value.getType(), a.getVariable()
				        .getOriginalType(), checker, a.getVariable() instanceof Local, false);
				code = new AssignmentExp(code, a.getVariable(), new Whatever(expr));
			}

			final Map<String, Variable> collissions = new HashMap<String, Variable>();
			for (final Assignment a : value.assignments()) {
				String variable = getVariable(value.getVariable(), a.getRead(), value.getType(), a.getVariable()
				        .getOriginalType(), checker, a.getVariable() instanceof Local, true);
				final Variable old;
				if (variable == null) {
					assert a.getVariable() instanceof Local;
					String name = "tmp";
					while (collissions.containsKey(name)) {
						name = Variable.nextName(name);
					}
					variable = name;
					final String expr = getVariable(value.getVariable(), a.getRead(), value.getType(), a.getVariable()
					        .getOriginalType(), checker, a.getVariable() instanceof Local, false);
					guards.put(
					        value,
					        new And(guards.get(value), new Equal(new Whatever(expr), new VariableExpression(a
					                .getVariable()))));
					old = collissions.put(name, a.getVariable());
					assert old == null;
				} else if (!(a.getVariable() instanceof Global)) {
					old = collissions.put(
					        variable,
					        createLocal(value.getVariable(), a.getVariable(), checker,
					                Collections.<String, String> emptyMap()));
				} else {
					old = collissions.put(variable, a.getVariable());
				}
				if (old != null) {
					guards.put(
					        value,
					        new And(guards.get(value), new Equal(new VariableExpression(old), new VariableExpression(
					                createLocal(value.getVariable(), a.getVariable(), checker,
					                        Collections.<String, String> emptyMap())))));
				} else {
					final Variable temporary;
					if (a.getVariable() instanceof Global) {
						temporary = new Temporary(variable, variable, a.getVariable().getType(), a.getVariable()
						        .getOriginalType());
					} else {
						temporary = createTemporary(value.getVariable(), a.getVariable(), checker, variable);
					}
					temporaries.put(variable, temporary);
					code = new AssignmentExp(code, temporary, new VariableExpression(a.getVariable()));
				}
			}

			for (final Entry<Channel, String> e : value.receives()) {
				final Channel readChannel = createReadChannel(value.getVariable(), e.getKey(), checker);
				parameters.put("channel." + e.getKey().getJavaName(), readChannel);
				final String variable = getVariable(value.getVariable(), e.getValue(), value.getType(), e.getKey()
				        .getOriginalType(), checker, true, true);
				final Temporary temporary = createTemporary(value.getVariable(), e.getKey(), checker, variable);
				temporaries.put(variable, temporary);
				code = new AssignmentExp(code, temporary, new Receive(readChannel));
			}

			for (final Lock l : value.locks()) {
				locks.add(l);
				code = new AcquireLock(code, l);
			}

			if ("UNIT".equals(value.getType()) && code instanceof Skip && code == first) {
				first = code = new Comment(value.getName());
			}
			code = new Label(code, value.getName());
			translations.put(value, (Label) code);

			ASTNode conditionals = null;
			boolean firstCond = true;
			for (final Entry<Pair<Pair<String, String>, String>, CFGNode> e : value.successors()) {
				final Label translated = translate(e.getValue(), translations, guards, types, checker, temporaries,
				        locals, parameters, locks, processIDs, launches);
				if (firstCond) {
					conditionals = new Jump(conditionals, translated);
				} else {
					final Expression variableGuard = buildGuard(value.getVariable(), e.getKey().getFirst().getFirst(),
					        value.getType(), checker);
					final Expression guardGuard = guards.get(e.getValue());
					Expression guard;
					if ("UNIT".equals(value.getType()) && variableGuard instanceof True
					        && guardGuard instanceof Whatever && "".equals(((Whatever) guardGuard).getE())) {
						guard = new Whatever("new Random().nextBoolean()");
					} else {
						guard = new And(variableGuard, guardGuard);
					}
					conditionals = new Conditional(guard, conditionals, translated);
				}
				firstCond = false;
			}
			if (firstCond) {
				conditionals = new Return();
			}

			first.setNext(conditionals);
			return translations.get(value);
		} else if (value.getKind() == Kind.INVOKE) {
			assert "".equals(value.getGuard());
			assert !value.sends().iterator().hasNext();
			assert !value.receives().iterator().hasNext();
			assert !value.locks().iterator().hasNext();
			assert !value.unlocks().iterator().hasNext();
			assert value.successors().size() <= 1;

			ASTNode code = new Return();
			for (final Entry<Pair<Pair<String, String>, String>, CFGNode> e : value.successors()) {
				final Label translated = translate(e.getValue(), translations, guards, types, checker, temporaries,
				        locals, parameters, locks, processIDs, launches);
				code = new Jump(null, translated);
			}

			final Map<String, Variable> expressions = new HashMap<String, Variable>();
			for (final Assignment a : value.assignments()) {
				if (a.getVariable() instanceof Local) {
					final Variable local = createLocal(value.getVariable(), a.getVariable(), checker,
					        ((Local) a.getVariable()).getInitialValues());
					expressions.put(a.getVariable().getId(), local);
					locals.put(a.getVariable().getJavaName(), local);
				} else {
					parameters.put("globals." + a.getVariable().getJavaName(), a.getVariable());
					expressions.put(a.getVariable().getId(), a.getVariable());
				}
			}

			final Launch launch = new Launch(code, value.getMethodName(), new ArrayList<Expression>());
			launches.add(Pair.createPair(launch, Pair.createPair(value, expressions)));
			code = launch;

			translations.put(value, new Label(code, value.getName()));
			return translations.get(value);
		} else {
			assert false;
		}
		return null;
	}
}
