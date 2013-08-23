package org.cpntools.simulator.extensions.dcr;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Observable;
import java.util.Observer;

import javax.swing.JOptionPane;

import org.cpntools.accesscpn.engine.protocol.Packet;
import org.cpntools.simulator.extensions.AbstractExtension;
import org.cpntools.simulator.extensions.Channel;
import org.cpntools.simulator.extensions.Command;
import org.cpntools.simulator.extensions.Option;

import dk.klafbang.tools.Pair;

public class DCRExtension extends AbstractExtension  implements Observer {

	public static final int ID = 10011;
	private final Map<String, DCRGraph> dcrgraphs = new HashMap<String, DCRGraph>();
	private final Map<String, DCRMarking> markings = new HashMap<String, DCRMarking>();
	private final Option<Boolean> DISABLE = Option.create("Disable DCR Extension (Cannot be undone and needs to be done before adding constraints)", "disable", Boolean.class);
	private Integer extDisabled = 0; // 0 = enabled // 1 = disabled but needs to pass constraints to Declare // 2 = disabled and is not claiming contraints anymore

	/**
	 * 
	 */
	public DCRExtension() {
		//JOptionPane.showMessageDialog(null,"ALERT MESSAGE2","TITLE2",JOptionPane.WARNING_MESSAGE);
		addOption(DISABLE);
		addSubscription(new Command(10000, 10001, true), new Command(500, 12, true), // Execute transition
		        new Command(500, 13, true), // Check transition for enabledness
		        new Command(500, 14, true), // Checked enabledness without scheduler
		        new Command(500, 15, true), // Manual binding
		        new Command(500, 20, true), // Init state
		        new Command(500, 21, true), // Create + reset scheduler
		        new Command(500, 35, true), // Check enabling of list of transitions
		        new Command(500, 36, true), // Check enabling of transitions without scheduler
		        new Command(800, 1, true) // Set state space options
		);
		addObserver(this);
	}

	@Override
	public int getIdentifier() {
		return DCRExtension.ID;
	}

	@Override
	public String getName() {
		return "DCR Graphs";
	}

	@Override
	public Packet handle(final Packet p) {
		// System.out.println("Handle for: ");
		// System.out.println(p.toString());
		// System.out.println("------");
		return null;
	}

	@Override
	public Packet handle(final Packet p, final Packet response) {
		p.reset();
		if (extDisabled == 1) return response;
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

	@Override
	public Packet prefilter(final Packet p) {
		// makeLazySubscriptions();
		p.reset();

		// System.out.println("Prefilter for: ");
		// System.out.println(p.toString());
		// System.out.println("------");

		p.reset();

		final int command = p.getInteger();
		if (command == 10000) {
			p.getInteger();
			final int subcommand = p.getInteger();
			p.reset();

			if (subcommand == 1) {
				if (extDisabled == 0)
				{
					return handleCheckPage(p);
				}
				else
					return handleCheckPageRemoveOnly(p);
			}
            else
	            return null;
		}

		return null;
	}

	@Override
	public void setChannel(final Channel c) {
		super.setChannel(c);
	}

	private Packet enabled(final Packet p, final Packet response) {
		response.reset();
		p.reset();
		if (response.getBoolean()) {
			final Packet result = new Packet(7, 1);
			result.addBoolean(enabled(p.getString(), p.getInteger()));
			return result;
		}
		return response;
	}

	private boolean enabled(final String task, final int integer) {
		for (final String pageId : new ArrayList<String>(dcrgraphs.keySet())) {
			final DCRGraph d = dcrgraphs.get(pageId);
			final DCRMarking m = markings.get(pageId);
			if (!d.Enabled(m, task)) return false;
		}
		return true;
	}

	private void execute(final Packet p) {
		p.reset();
		final String task = p.getString();
		for (final String pageId : new ArrayList<String>(dcrgraphs.keySet())) {
			final DCRGraph d = dcrgraphs.get(pageId);
			final DCRMarking m = markings.get(pageId);
			final DCRMarking next = d.Execute(m, task);
			// System.out.println("Marking after execution:");
			// System.out.println(next.toString());
			markings.put(pageId, next);
		}
	}

	private Packet handleCheckPage(final Packet p) {
		final Packet f = new Packet(p.getOpcode(), 10000);

		p.reset();
		p.getInteger(); // command
		p.getInteger(); // extension
		p.getInteger(); // subcmd
		final int count = p.getInteger();

		final String pageId = p.getString();
		// System.out.println("pageId:" + pageId);

		DCRGraph d;
		if (!dcrgraphs.containsKey(pageId)) {
			markings.put(pageId, new DCRMarking());
			d = new DCRGraph();
			dcrgraphs.put(pageId, d);
		} else {
			d = dcrgraphs.get(pageId);
		}

		int newCount = count;
		if (count != 0) {
			for (int i = 0; i < count; i++) {
				final int parameters = p.getInteger();
				final String relationID = p.getString();
				final String name = p.getString();
				final String formula = p.getString();
				p.getString();
				// System.out.println("RelationID: " + relationID + "Name: " + name + " formula: " + formula);

				String param1 = "";
				String param2 = "";
				for (int j = 0; j < parameters; j++) {
					final String tid = p.getString();
					if (j == 0) {
						param1 = tid;
					}
					if (j == 1) {
						param2 = tid;
					}
					if (!d.events.contains(tid)) {
						markings.get(pageId).included.add(tid);
						d.events.add(tid);
					}
				}

				final Pair<String, String> relation = Pair.createPair(param1, param2);

				if (name.equals("precedence")) {
					d.conditions.add(relation);
					d.relationID.put(relationID, Pair.createPair(1, relation));
					newCount = newCount - 1;
				}
				if (name.equals("response")) {
					d.responses.add(relation);
					d.relationID.put(relationID, Pair.createPair(2, relation));
					newCount = newCount - 1;
				}
				if (name.equals("include")) {
					d.includes.add(relation);
					d.relationID.put(relationID, Pair.createPair(3, relation));
					newCount = newCount - 1;
				}
				if (name.equals("exclude")) {
					d.excludes.add(relation);
					d.relationID.put(relationID, Pair.createPair(4, relation));
					newCount = newCount - 1;
				}
				if (name.equals("milestone")) {
					d.milestones.add(relation);
					d.relationID.put(relationID, Pair.createPair(5, relation));
					newCount = newCount - 1;
				}
			}
		}

		final int delcount = p.getInteger();
		int newdelcount = delcount;
		for (int i = 0; i < delcount; i++) {
			final String delid = p.getString();
			System.out.println("test1");
			if (d.relationID.containsKey(delid)) {
				System.out.println("test2");
				d.relationID.get(delid).getFirst();
				newdelcount = newdelcount - 1;
				d.RemoveRealtion(delid);
			}
		}

		p.reset();
		p.getInteger();
		f.addInteger(p.getInteger()); // extension
		f.addInteger(p.getInteger()); // subcmd

		p.getInteger();
		f.addInteger(newCount);
		p.getString();
		f.addString(pageId);

		for (int i = 0; i < count; i++) {
			boolean b = true;
			final int parameters = p.getInteger();
			final String s1 = p.getString();
			final String name = p.getString();
			final String formula = p.getString();
			final String inscription = p.getString();

			if (name.equals("precedence") || name.equals("response") || name.equals("include")
			        || name.equals("exclude") || name.equals("milestone")) {
				b = false;
			} else {
				f.addInteger(parameters);
				f.addString(s1);
				f.addString(name);
				f.addString(formula);
				f.addString(inscription);
			}

			for (int j = 0; j < parameters; j++) {
				if (!b) {
					p.getString();
				} else {
					f.addString(p.getString());
				}
			}
		}
		f.addInteger(newdelcount);
		for (int i = 0; i < delcount; i++) {
			final String delid = p.getString();
			if (!d.relationID.containsKey(delid)) {
				f.addString(delid);
			}
		}

		dcrgraphs.put(pageId, d);

		// System.out.println("DCR Graph:");
		// System.out.println(d.toString());
		// System.out.println("-----------------");

		// System.out.println("Outgoing packet:");
		// System.out.println(f.toString());
		// System.out.println("-----------------");

		return f;
	}

	private Packet multipleEnabled(final Packet p, final Packet response) {
		p.reset();
		response.reset();
		if (response.getInteger() != 1) return response;
		final Packet result = new Packet(7, 1);
		p.getInteger();
		p.getInteger(); // Skip command and subcmd
		final int count = p.getInteger();
		result.addInteger(count);
		for (int i = 0; i < count; i++) {
			if (response.getBoolean()) {
				result.addBoolean(enabled(p.getString(), p.getInteger()));
			} else {
				p.getString();
				p.getInteger();
				result.addBoolean(false);
			}
			result.addString(response.getString());
		}
		// System.out.println("Multipleenabled result: ");
		// System.out.println(result.toString());
		// System.out.println("------");
		return result;
	}

	private void reset() {
		for (final String page : new ArrayList<String>(dcrgraphs.keySet())) {
			markings.put(page, dcrgraphs.get(page).InitialMarking());
		}
	}
	
	@SuppressWarnings("unchecked")
	public void update(final Observable arg0, final Object arg1) {		
		  if (arg0 == this) { 
			  if (arg1 instanceof Option) { 
				  final Option<?> option = (Option<?>) arg1; 
				  if ("disable".equals(option.getKey())) { 
					  //System.out.println("test");
					  //JOptionPane.showMessageDialog(null,"ALERT MESSAGE","TITLE",JOptionPane.WARNING_MESSAGE);					  
					  if ((Boolean) getOption(option))
					  {
						  //JOptionPane.showMessageDialog(null,"TRUE","TITLE",JOptionPane.WARNING_MESSAGE);						  
						  if (extDisabled == 0) extDisabled = 1;
					  }
					  else
					  {
						  //JOptionPane.showMessageDialog(null,"FALSE","TITLE",JOptionPane.WARNING_MESSAGE);
						  setOption((Option<Boolean>) option, true);
					  }						  
					  }
				  }
		  }	
	 }	
	
	private Packet handleCheckPageRemoveOnly(final Packet p) {
		final Packet f = new Packet(p.getOpcode(), 10000);

		p.reset();
		p.getInteger(); // command
		p.getInteger(); // extension
		p.getInteger(); // subcmd
		final int count = p.getInteger();

		final String pageId = p.getString();
		// System.out.println("pageId:" + pageId);

		DCRGraph d;
		if (!dcrgraphs.containsKey(pageId)) {
			markings.put(pageId, new DCRMarking());
			d = new DCRGraph();
			dcrgraphs.put(pageId, d);
		} else {
			d = dcrgraphs.get(pageId);
		}

		int newCount = count;
		if (count != 0) {
			for (int i = 0; i < count; i++) {
				final int parameters = p.getInteger();
				final String relationID = p.getString();
				final String name = p.getString();
				final String formula = p.getString();
				p.getString();
				// System.out.println("RelationID: " + relationID + "Name: " + name + " formula: " + formula);

				String param1 = "";
				String param2 = "";
				for (int j = 0; j < parameters; j++) {
					final String tid = p.getString();
					if (j == 0) {
						param1 = tid;
					}
					if (j == 1) {
						param2 = tid;
					}
					if (!d.events.contains(tid)) {
						markings.get(pageId).included.add(tid);
						d.events.add(tid);
					}
				}

				final Pair<String, String> relation = Pair.createPair(param1, param2);

				if (name.equals("include")) {
					d.includes.add(relation);
					d.relationID.put(relationID, Pair.createPair(3, relation));
					newCount = newCount - 1;
				}
				if (name.equals("exclude")) {
					d.excludes.add(relation);
					d.relationID.put(relationID, Pair.createPair(4, relation));
					newCount = newCount - 1;
				}
				if (name.equals("milestone")) {
					d.milestones.add(relation);
					d.relationID.put(relationID, Pair.createPair(5, relation));
					newCount = newCount - 1;
				}
			}
		}

		final int delcount = p.getInteger();
		int newdelcount = delcount;
		for (int i = 0; i < delcount; i++) {
			final String delid = p.getString();
			System.out.println("test1");
			if (d.relationID.containsKey(delid)) {
				System.out.println("test2");
				d.relationID.get(delid).getFirst();
				newdelcount = newdelcount - 1;
				d.RemoveRealtion(delid);
			}
		}

		p.reset();
		p.getInteger();
		f.addInteger(p.getInteger()); // extension
		f.addInteger(p.getInteger()); // subcmd

		p.getInteger();
		f.addInteger(newCount);
		p.getString();
		f.addString(pageId);

		for (int i = 0; i < count; i++) {
			boolean b = true;
			final int parameters = p.getInteger();
			final String s1 = p.getString();
			final String name = p.getString();
			final String formula = p.getString();
			final String inscription = p.getString();

			if (name.equals("include")
			        || name.equals("exclude") || name.equals("milestone")) {
				b = false;
			} else {
				f.addInteger(parameters);
				f.addString(s1);
				f.addString(name);
				f.addString(formula);
				f.addString(inscription);
			}

			for (int j = 0; j < parameters; j++) {
				if (!b) {
					p.getString();
				} else {
					f.addString(p.getString());
				}
			}
		}
		f.addInteger(newdelcount);
		for (int i = 0; i < delcount; i++) {
			final String delid = p.getString();
			if (!d.relationID.containsKey(delid)) {
				f.addString(delid);
			}
		}

		dcrgraphs.put(pageId, d);

		// System.out.println("DCR Graph:");
		// System.out.println(d.toString());
		// System.out.println("-----------------");

		// System.out.println("Outgoing packet:");
		// System.out.println(f.toString());
		// System.out.println("-----------------");
		//JOptionPane.showMessageDialog(null,p.toString(),"TITLE",JOptionPane.WARNING_MESSAGE);
		 //JOptionPane.showMessageDialog(null,f.toString(),"TITLE",JOptionPane.WARNING_MESSAGE);
		return f;
	}	

}
