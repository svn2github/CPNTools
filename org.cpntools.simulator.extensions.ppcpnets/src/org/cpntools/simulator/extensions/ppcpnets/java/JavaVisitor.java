package org.cpntools.simulator.extensions.ppcpnets.java;

import java.io.PrintStream;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Set;

/**
 * @author michael
 */
public class JavaVisitor extends Visitor<Object, Object, Object, Object> {

	/**
	 * 
	 */
	public final PrintStream out;

	private Set<ASTNode> done;

	private LinkedList<ASTNode> entries;

	private int indent = 2;

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
				sb.append("\t\tObjectOutputStream ");
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
		for (final Variable v : process.getParameters()) {
			if (globals || channels) {
				out.print(", ");
			}
			if (v instanceof Global) {
				globals = true;
				out.print(v.getType().getJavaName());
				out.print(" global");
			} else if (v instanceof SendChannel) {
				channels = true;
				out.print("ObjectOutputStream channel");
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
				out.print(", ");
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
		if (entry == null) {
			if (entries.isEmpty()) { return null; }
			out.println();
			return visit(entries.removeFirst());
		}

		super.visit(entry);
		visit(entry.getNext());
		return null;
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
		out.print(" goto ");
		out.print(entry.getJump().getLabel());
		out.println(";");
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
		out.print(entry.getV().getType().getJavaName());
		out.print(" ");
		out.print(entry.getV().getJavaName());
		out.println(";");

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
		indent();
		out.print("if ");
		visit(entry.getCondition());
		out.println(" {");
		start();
		visit(entry.getThenBranch());
		end();
		if (entry.getElseBranch() != null) {
			indent();
			out.println("} else {");
			start();
			visit(entry.getElseBranch());
			end();
		}
		indent();
		out.println("}");
		return super.visit(entry);
	}

	/**
	 * @return
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.Jump)
	 */
	@Override
	public Object visit(final Jump entry) {
		indent();
		out.print("goto ");
		out.print(entry.getJump().getLabel());
		out.println(";");
		add(entry.getJump());

		return super.visit(entry);
	}

	/**
	 * @return
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.Label)
	 */
	@Override
	public Object visit(final Label entry) {
		done.add(entry);
		indent();
		out.print(entry.getLabel());
		out.println(":");

		return super.visit(entry);
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
		out.println("public class " + process.getName() + " implements Runnable {");
		makeGlobalsDecl(process);
		makeChannelDecl(process);
		makeLocalDecl(process);
		makeConstructor(process);

		entries = new LinkedList<ASTNode>();
		done = new HashSet<ASTNode>(Collections.singleton(process.getEntry()));

		out.println("\tpublic void run() {");
		super.visit(process);
		out.println("\t}");

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
	 * @return
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.Send)
	 */
	@Override
	public Object visit(final Send entry) {
		indent();
		out.print("channels.");
		out.print(entry.getC().getJavaName());
		out.print(".writeObject(");
		visit(entry.getE());
		out.println(");");

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
	 * @return
	 * @see org.cpntools.simulator.extensions.ppcpnets.java.Visitor#visit(org.cpntools.simulator.extensions.ppcpnets.java.VariableExpression)
	 */
	@Override
	public Object visit(final VariableExpression e) {
		if (e.getV() instanceof Local) {
			out.print("this.");
			out.print(e.getV().getJavaName());
		} else if (e.getV() instanceof Global) {
			out.print("globals.");
			out.print(e.getV().getJavaName());
		} else if (e.getV() instanceof Resource) {
			assert false; // Not implemented yet
		} else if (e.getV() instanceof Temporary) {
			out.print(e.getV().getJavaName());
		} else {
			assert false;
		}
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
