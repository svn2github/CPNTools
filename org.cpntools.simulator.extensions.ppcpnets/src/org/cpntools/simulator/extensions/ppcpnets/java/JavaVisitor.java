package org.cpntools.simulator.extensions.ppcpnets.java;

import java.io.PrintStream;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * @author michael
 */
public class JavaVisitor extends Visitor<Object, Object, Object, Object> {

	/**
	 * 
	 */
	public final PrintStream out;

	private Set<ASTNode> done;

	private boolean elseif = false;

	private LinkedList<Label> entries;

	private int indent = 2;

	private int inLocks = 0;

	private String name;

	private StringBuilder parameterDeclaration;

	/**
	 * @param out
	 */
	public JavaVisitor(final PrintStream out) {
		this.out = out;
	}

	/**
	 * @param jump
	 */
	public void add(final Label jump) {
		if (done.add(jump)) {
			entries.add(jump);
		}
	}

	/**
	 * @param process
	 */
	public void makeChannelDecl(final Process process) {
		final StringBuilder sb = new StringBuilder();

		boolean seen = false;
		sb.append("\tpublic static class Channels {\n");
		for (final Variable v : process.getParameters()) {
			if (v instanceof ReceiveChannel) {
				seen = true;
				sb.append("\t\tObjectInputStream ");
				sb.append(v.getJavaName());
				sb.append(";\n");
			} else if (v instanceof SendChannel) {
				seen = true;
				sb.append("\t\tMap<String, ObjectOutputStream> ");
				sb.append(v.getJavaName());
				sb.append(";\n");
			} else {
				assert v instanceof Global;
			}
		}
		sb.append("\t}\n");
		sb.append("\tChannels channals;\n");
		sb.append("\n");
		if (seen) {
			out.print(sb);
		}
	}

	/**
	 * @param process
	 */
	public void makeConstructor(final Process process) {
		out.print("\tpublic ");
		out.print(process.getName());
		out.print("(");
		boolean globals = false, channels = false;
		final StringBuilder sb = new StringBuilder("        ");
		for (int i = process.getName().length(); i > 0; i--) {
			sb.append(' ');
		}
		final String s = sb.toString();
		for (final Variable v : process.getParameters()) {
			if (globals || channels) {
				out.print(",\n\t");
				out.print(s);
			}
			if (v instanceof Global) {
				globals = true;
				out.print(v.getType().getJavaName());
				out.print(" global");
			} else if (v instanceof SendChannel) {
				channels = true;
				out.print("Map<String, ObjectOutputStream> channel");
			} else if (v instanceof ReceiveChannel) {
				channels = true;
				out.print("ObjectInputStream channel");
			} else {
				assert false;
			}
			out.print(v.getJavaName());
		}
		boolean locals = false;
		for (final Variable v : process.getLocals()) {
			if (globals || channels || locals) {
				out.print(",\n\t");
				out.print(s);
			}
			if (v instanceof Local) {
				locals = true;
				out.print(v.getType().getJavaName());
				out.print(" local");
			} else {
				assert false;
			}
			out.print(v.getJavaName());
		}
		out.println(") {");

		if (globals) {
			out.println("\t\tglobals = new Globals();");
		}
		if (channels) {
			out.println("\t\tchannels = new Channels();");
		}

		for (final Variable v : process.getParameters()) {
			if (v instanceof Global) {
				out.print("\t\tglobals.");
			} else if (v instanceof Channel) {
				out.print("\t\tchannels.");
			} else {
				assert false;
			}
			out.print(v.getJavaName());
			out.print(" = ");
			if (v instanceof Global) {
				out.print("global");
			} else if (v instanceof Channel) {
				out.print("channel");
			}
			out.print(v.getJavaName());
			out.println(";");
		}

		for (final Variable v : process.getLocals()) {
			if (v instanceof Local) {
				out.print("\t\tthis.");
			} else {
				assert false;
			}
			out.print(v.getJavaName());
			out.print(" = local");
			out.print(v.getJavaName());
			out.println(";");
		}

		out.println("\t}");
		out.println();
	}

	/**
	 * @param process
	 */
	public void makeGlobalsDecl(final Process process) {
		final StringBuilder sb = new StringBuilder();

		boolean seen = false;
		sb.append("\tpublic static class Globals {\n");
		for (final Variable v : process.getParameters()) {
			if (v instanceof Global) {
				seen = true;
				sb.append("\t\t");
				sb.append(v.getType().getJavaName());
				sb.append(" ");
				sb.append(v.getJavaName());
				sb.append(";\n");
			} else {
				assert v instanceof Channel;
			}
		}
		sb.append("\t}\n");
		sb.append("\tGlobals globals;\n");
		sb.append("\n");
		if (seen) {
			out.print(sb);
		}
	}

	/**
	 * @param process
	 */
	public void makeLocalDecl(final Process process) {
		boolean seen = false;
		for (final Variable v : process.getLocals()) {
			if (v instanceof Local) {
				seen = true;
				out.print("\t");
				out.print(v.getType().getJavaName());
				out.print(" ");
				out.print(v.getJavaName());
				out.println(";");
			} else {
				assert false;
			}
		}
		if (seen) {
			out.println();
		}
	}

	/**
	 * @param process
	 */
	public void makeLocks(final Process process) {
		final StringBuilder sb = new StringBuilder();

		boolean seen = false;
		sb.append("\tpublic static class Locks {\n");
		for (final Lock l : process.getLocks()) {
			seen = true;
			sb.append("\t\t");
			sb.append("Semaphore ");
			sb.append(l.getJavaName());
			sb.append(" = new Semaphore(");
			sb.append(l.getCount());
			sb.append(", true);\n");
		}
		sb.append("\t}\n");
		sb.append("\tstatic Locks locks = new Locks();\n");
		sb.append("\n");
		if (seen) {
			out.print(sb);
		}
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.AbstractSyntaxTree)
	 */
	@SuppressWarnings("null")
	@Override
	public Object visit(final AbstractSyntaxTree ast) {
		out.println("import java.io.*;");
		out.println("import java.util.*;");
		out.println("import java.util.concurrent.*;");
		out.println();
		out.println("public class " + ast.getJavaName() + " {");
		out.println("\tpublic static void main(String... args) {");

		final Comparator<Variable> comparator = new Comparator<Variable>() {
			@Override
			public int compare(final Variable o1, final Variable o2) {
				return o1.getJavaName().compareTo(o2.getJavaName());
			}
		};
		final Map<String, Variable> names = new HashMap<String, Variable>();
		final SortedSet<Global> globals = new TreeSet<Global>(comparator);
		final SortedSet<Channel> channels = new TreeSet<Channel>(comparator);
		for (final Process p : ast.getProcesses()) {
			if (!p.getInitial().isEmpty()) {
				for (final Variable v : p.getParameters()) {
					names.put(v.getJavaName(), v);
// assert old == null || old == v; // Not true for channels
					if (v instanceof Global) {
						globals.add((Global) v);
					} else if (v instanceof Channel) {
						channels.add((Channel) v);
					}
				}
			}
		}
		boolean first = true;
		for (final Global g : globals) {
			first = false;
			out.print("\t\t");
			out.print(g.getType().getJavaName());
			out.print(" ");
			out.print(g.getJavaName());
			out.print(" = ");
			out.print(g.getInit());
			out.println(";");
		}
		if (!first) {
			out.println();
		}

		first = true;
		for (final Channel c : channels) {
			first = false;
			out.print("\t\tMap<String, ObjectInputStream> ");
			out.print(c.getJavaName());
			out.println("_input = new HashMap<String, ObjectInputStream>();");
			out.print("\t\tMap<String, ObjectOutputStream> ");
			out.print(c.getJavaName());
			out.println("_output = new HashMap<String, ObjectOutputStream>();");
		}
		if (!first) {
			out.println();
		}

		first = true;
		for (final Process p : ast.getProcesses()) {
			for (final Variable v : p.getParameters()) {
				if (v instanceof ReceiveChannel && channels.remove(v)) {
					for (final String instance : p.getInitial()) {
						out.print("\t\t");
						if (first) {
							out.print("PipedInputStream ");
						}
						first = false;
						out.println("inputpipe = new PipedInputStream();");
						out.print("\t\t");
						out.print(v.getJavaName());
						out.print("_input.put(\"");
						out.print(instance);
						out.println("\", new ObjectInputStream(inputpipe));");
						out.print("\t\t");
						out.print(v.getJavaName());
						out.print("_output.put(\"");
						out.print(instance);
						out.println("\", new ObjectOutputStream(new PipedOutputStream(inputpipe)));");
					}
				}
			}
		}
		if (!first) {
			out.println();
		}

		for (final Process p : ast.getProcesses()) {
			final StringBuilder sb = new StringBuilder("                ");
			for (int i = p.getName().length(); i > 0; i--) {
				sb.append(' ');
			}
			final String s = sb.toString();
			for (final String instance : p.getInitial()) {
				out.print("\t\tnew Thread(new ");
				out.print(p.getName());
				out.print("(");
				first = true;
				Variable lastVariable = null;
				for (final Variable v : p.getParameters()) {
					if (!first) {
						out.print(", \t// ");
						out.print(lastVariable.getType().getJavaName());
						out.print(" ");
						out.println(lastVariable.getJavaName());
						out.print("\t\t");
						out.print(s);
					}
					first = false;
					lastVariable = v;
					out.print(v.getJavaName());
					if (v instanceof ReceiveChannel) {
						out.print("_input.get(\"");
						out.print(instance);
						out.print("\")");
					} else if (v instanceof SendChannel) {
						out.print("_output");
					}
				}
				for (final Variable v : p.getLocals()) {
					if (!first) {
						out.print(", \t// ");
						out.print(lastVariable.getType().getJavaName());
						out.print(" ");
						out.println(lastVariable.getJavaName());
						out.print("\t\t");
						out.print(s);
					}
					first = false;
					lastVariable = v;
					if (v instanceof Local) {
						final Local l = (Local) v;
						out.print(l.getInitialValue(instance));
					}
				}
				out.print("),");
				if (lastVariable != null) {
					out.print(" \t// ");
					out.print(lastVariable.getType().getJavaName());
					out.print(" ");
					out.print(lastVariable.getJavaName());
				}
				out.print("\n\t\t           \"");
				out.print(p.getName());
				out.print(", ");
				out.print(instance);
				out.println("\").start();");
			}
		}
		out.println("\t}");
		out.println("}");

		return super.visit(ast);
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.AcquireLock)
	 */
	@Override
	public Object visit(final AcquireLock e) {
		indent();
		inLocks++;
		out.print("locks.");
		out.print(e.getLock().getJavaName());
		out.println(".acquireUninterruptibly();");
		return super.visit(e);
	}

	/**
	 * @return
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.And)
	 */
	@Override
	public Object visit(final And e) {
		visit(e.getE1());
		out.print(" && ");
		visit(e.getE2());
		return super.visit(e);
	}

	/**
	 * @return
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.AssignmentExp)
	 */
	@Override
	public Object visit(final AssignmentExp entry) {
		indent();
		if (entry.getV() instanceof Local) {
			out.print("this.");
			out.print(entry.getV().getJavaName());
		} else if (entry.getV() instanceof Global) {
			out.print("globals.");
			out.print(entry.getV().getJavaName());
		} else if (entry.getV() instanceof Resource) {
			assert false; // Not implemented yet
		} else if (entry.getV() instanceof Temporary) {
			out.print(entry.getV().getJavaName());
		} else {
			assert false;
		}
		out.print(" = ");
		visit(entry.getE());
		out.println(";");

		return null;
	}

	/**
	 * @return
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.ASTNode)
	 */
	@Override
	public Object visit(final ASTNode entry) {
		if (entry == null) { return null; }

		super.visit(entry);
		if (!(entry instanceof Label) && !(entry instanceof Jump) || entry instanceof Conditional) {
			visit(entry.getNext());
		}
		return null;
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.Comment)
	 */
	@Override
	public Object visit(final Comment e) {
		indent();
		out.print("// ");
		out.println(e.getComment());
		return super.visit(e);
	}

	/**
	 * @return
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.Conditional)
	 */
	@Override
	public Object visit(final Conditional entry) {
		indent();
		out.print("if ");
		visit(entry.getCondition());
		out.print(" { ");
		out.print(entry.getJump().getLabel());
		out.println("(); return; }");
		add(entry.getJump());

		return super.visit(entry);
	}

	/**
	 * @return
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.Declaration)
	 */
	@Override
	public Object visit(final Declaration entry) {
		indent();
		final String s = entry.getV().getType().getJavaName() + " " + entry.getV().getJavaName() + ";\n";
		parameterDeclaration.append("\t\t");
		parameterDeclaration.append(s);
		out.print(s);

		return super.visit(entry);
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.DoWhile)
	 */
	@Override
	public Object visit(final DoWhile entry) {
		indent();
		out.println("do {");
		start();
		visit(entry.getInner());
		end();
		indent();
		out.print("} while ");
		visit(entry.getCondition());
		out.println(";");
		return super.visit(entry);
	}

	/**
	 * @return
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.Equal)
	 */
	@Override
	public Object visit(final Equal e) {
		visit(e.getE1());
		out.print(" == ");
		visit(e.getE2());
		return super.visit(e);
	}

	/**
	 * @return
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.Expression)
	 */
	@Override
	public Object visit(final Expression e) {
		out.print("(");

		super.visit(e);

		out.print(")");
		return null;
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.If)
	 */
	@Override
	public Object visit(final If entry) {
		final boolean wasElseIf = elseif;
		elseif = false;
		indent();
		if (wasElseIf) {
			out.print("} else ");
		}
		out.print("if ");
		visit(entry.getCondition());
		out.println(" {");
		start();
		visit(entry.getThenBranch());
		end();
		if (entry.getElseBranch() != null) {
			if (entry.getElseBranch() instanceof If) {
				elseif = true;
				visit(entry.getElseBranch());
			} else {
				indent();
				out.println("} else {");
				start();
				visit(entry.getElseBranch());
				end();
			}
		}
		if (!wasElseIf) {
			indent();
			out.println("}");
		}
		return super.visit(entry);
	}

	/**
	 * @return
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.Jump)
	 */
	@Override
	public Object visit(final Jump entry) {
		indent();
		out.print(entry.getJump().getLabel());
		out.println("();");
		add(entry.getJump());

		return super.visit(entry);
	}

	/**
	 * @return
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.Label)
	 */
	@Override
	public Object visit(final Label entry) {
		indent();
		out.print(entry.getLabel());
		out.println("();");
		add(entry);
		return null;
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.Launch)
	 */
	@Override
	public Object visit(final Launch e) {
		name = Variable.nextName(name);
		indent();
		out.print(e.getProcedure());
		out.print(" ");
		out.print(name);
		out.println(" =");
		start();
		indent();
		out.print("new ");
		out.print(e.getProcedure());
		out.print("(");
		boolean first = true;
		final StringBuilder sb = new StringBuilder("     ");
		for (int i = e.getProcedure().length(); i > 0; i--) {
			sb.append(' ');
		}
		final String s = sb.toString();
		for (final Expression exp : e.getParameters()) {
			if (!first) {
				out.print(",\n");
				indent();
				out.print(s);
			}
			first = false;
			visit(exp);
		}
		out.println(").run();");
		end();
		return null;
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.Not)
	 */
	@Override
	public Object visit(final Not entry) {
		out.print("!");
		visit(entry.getE());
		return super.visit(entry);
	}

	/**
	 * @return
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.Process)
	 */
	@Override
	public Object visit(final Process process) {
		out.println("import java.io.*;");
		out.println("import java.util.*;");
		out.println("import java.util.concurrent.*;");
		out.println();
		out.println("public class " + process.getName() + " implements Runnable {");
		makeGlobalsDecl(process);
		makeChannelDecl(process);
		makeLocalDecl(process);
		makeLocks(process);
		makeConstructor(process);

		entries = new LinkedList<Label>();
		done = new HashSet<ASTNode>();

		name = "tmpprocedure0";
		parameterDeclaration = new StringBuilder();
		out.println("\tpublic void run() {");
		super.visit(process);
		out.println("\t}");

		while (!entries.isEmpty()) {
			final Label e = entries.removeFirst();
			out.println();
			out.print("\tprivate void ");
			out.print(e.getLabel());
			out.print("(");
			out.println(") {");
			out.print(parameterDeclaration);
			inLocks = 0;
			visit(e.getNext());
			assert inLocks == 0;
			out.println("\t}");
		}

		out.println("}");
		return null;
	}

	/**
	 * @return
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.Receive)
	 */
	@Override
	public Object visit(final Receive e) {
		out.print("(");
		out.print(e.getC().getType().getJavaName());
		out.print(") channels.");
		out.print(e.getC().getJavaName());
		out.print(".readObject()");
		return super.visit(e);
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.ReleaseLock)
	 */
	@Override
	public Object visit(final ReleaseLock e) {
		indent();
		inLocks--;
		out.print("locks.");
		out.print(e.getLock().getJavaName());
		out.println(".release();");
		return super.visit(e);
	}

	/**
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.Return)
	 */
	@Override
	public Object visit(final Return entry) {
		indent();
		out.println("return;");
		return super.visit(entry);
	}

	/**
	 * @return
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.Send)
	 */
	@Override
	public Object visit(final Send entry) {
		indent();
		out.print("synchronized (channels.");
		out.print(entry.getC().getJavaName());
		out.println(") {");
		indent();
		out.print("\tchannels.");
		out.print(entry.getC().getJavaName());
		out.print(".get(\"");
		out.print("TODO\").writeObject(");
		visit(entry.getE());
		out.println(");");
		indent();
		out.println("}");

		return super.visit(entry);
	}

	/**
	 * @return
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.Skip)
	 */
	@Override
	public Object visit(final Skip entry) {
		return super.visit(entry);
	}

	/**
	 * @param entry
	 * @return
	 */
	@Override
	public Object visit(final True entry) {
		out.print("true");
		return super.visit(entry);
	}

	/**
	 * @param v
	 */
	public void visit(final Variable v) {
		if (v instanceof Local) {
			out.print("this.");
			out.print(v.getJavaName());
		} else if (v instanceof Global) {
			out.print("globals.");
			out.print(v.getJavaName());
		} else if (v instanceof Resource) {
			assert false; // Not implemented yet
		} else if (v instanceof Temporary) {
			out.print(v.getJavaName());
		} else if (v instanceof ProcessVariable) {
			out.print(name);
			out.print(".");
			visit(((ProcessVariable) v).getVariable());
		} else {
			assert false;
		}

	}

	/**
	 * @return
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.VariableExpression)
	 */
	@Override
	public Object visit(final VariableExpression e) {
		visit(e.getV());
		return super.visit(e);
	}

	/**
	 * @return
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.Whatever)
	 */
	@Override
	public Object visit(final Whatever e) {
		out.print(e.getE());
		return super.visit(e);
	}

	private void end() {
		indent--;
	}

	private void indent() {
		for (int i = 0; i < indent; i++) {
			out.print('\t');
		}
	}

	private void start() {
		indent++;
	}

}
