package org.cpntools.simulator.extensions.dcr;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Frame;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Observable;
import java.util.Observer;
import java.util.SortedSet;
import java.util.TreeSet;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.DefaultListModel;
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;
import javax.swing.SwingConstants;


import org.cpntools.accesscpn.engine.protocol.Packet;
import org.cpntools.simulator.extensions.AbstractExtension;
import org.cpntools.simulator.extensions.Channel;
import org.cpntools.simulator.extensions.Command;
import org.cpntools.simulator.extensions.Option;
import org.cpntools.simulator.extensions.declare.DeclareExtension;
import org.cpntools.simulator.extensions.scraper.Page;
import org.cpntools.simulator.extensions.scraper.Scraper;
import org.cpntools.simulator.extensions.scraper.Transition;
import org.cpntools.simulator.extensions.server.Handler;

import java.util.Date;

public class DCRFilterExtension  extends AbstractExtension {
	
	public static final int ID = 10011;
	private JDialog dialog;
	private JTabbedPane tabs;
	private JTextField stringOption;
	private JTextField integerOption;
	private JCheckBox booleanOption;
	
	
	private final Map<String, DCRGraph> dcrgraphs = new HashMap<String, DCRGraph>();
	private final Map<String, DCRMarking> markings = new HashMap<String, DCRMarking>();

	/**
	 * 
	 */
	public DCRFilterExtension() {
		//addOption(Option.create("String", "string", String.class), Option.create("Integer", "integer", Integer.class),
		//        Option.create("Boolean", "boolean", Boolean.class));
		//addLazySubscription(new Command(400, 2));				
		
		addSubscription(//new Command(200, 9), 
				//new Command(400, 2, true)//, // Syntax check page
				new Command(10000, 10001, true),
		        //new Command(500, 3, true), // Generate instances
		        //new Command(500, 4, true), // Update instances
		        //new Command(500, 11, true), // Start run
		        new Command(500, 12, true), // Execute transition
		        new Command(500, 13, true), // Check transition for enabledness
		        new Command(500, 14, true), // Checked enabledness without scheduler
		        new Command(500, 15, true), // Manual binding
		        new Command(500, 20, true), // Init state
		        new Command(500, 21, true), // Create + reset scheduler
		        new Command(500, 35, true), // Check enabling of list of transitions
		        new Command(500, 36, true), // Check enabling of transitions without scheduler
		        new Command(800, 1, true) // Set state space options
				);
		//addObserver(this);
	}

	@Override
	public void setChannel(final Channel c) {
		super.setChannel(c);

		try {
			System.out.println("107");
			//final Scraper scraper = c.getExtension(Scraper.class);
			//if (scraper != null) {
			//	scraper.addObserver(this);
			//}
		} catch (final Exception e) {
			e.printStackTrace();
		}
	}

	@Override
	public int getIdentifier() {
		return ID;
	}

	@Override
	public String getName() {
		return "DCR Filter Extension";
	}
	
	

	
	@Override
	public Packet prefilter(final Packet p) {		
		//makeLazySubscriptions();		
		p.reset();
		
		System.out.println("Prefilter for: ");
		System.out.println(p.toString());
		System.out.println("------");
		
		p.reset();
		
		final int command = p.getInteger();
		if (command == 10000) {
			//System.out.print(command);
			//System.out.print(" ");
			final int extension = p.getInteger();
			//System.out.print(extension);
			//System.out.print(" ");
			final int subcommand = p.getInteger();
			//System.out.println(subcommand);
			p.reset();
		
			if (subcommand == 1) 
				return handleCheckPage(p);
				//return null;
			else
				return null;
		}
		
		return null;
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

	
	@Override
	public Packet handle(final Packet p) {
		//System.out.println("Handle for: ");
		//System.out.println(p.toString());
		//System.out.println("------");
		
		return null;
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
		//Object task = tasks.get(string);
		
		for (final String pageId : new ArrayList<String>(dcrgraphs.keySet())) {
			final DCRGraph d = dcrgraphs.get(pageId);
			final DCRMarking m = markings.get(pageId);			
			if (!d.Enabled(m, task)) return false;
			//final int state = states.get(pageId);
			//if (!acceptable(a, state, task)) { return false; }
		}
		return true;
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
				p.getString();
				p.getInteger();
				result.addBoolean(false);
			}
			result.addString(response.getString());
		}
		System.out.println("Multipleenabled result: ");
		System.out.println(result.toString());
		System.out.println("------");		
		return result;
	}
	
	private void execute(final Packet p) {
		p.reset();
		String task = p.getString();
		for (final String pageId : new ArrayList<String>(dcrgraphs.keySet())) {
			final DCRGraph d = dcrgraphs.get(pageId);
			final DCRMarking m = markings.get(pageId);
			DCRMarking next = d.Execute(m, task);
			System.out.println("Marking after execution:");
			System.out.println(next.toString());
			markings.put(pageId, next);
		}
	}
	
	
	private void reset() {
		for (final String page : new ArrayList<String>(dcrgraphs.keySet())) {
			markings.put(page, dcrgraphs.get(page).InitialMarking());
		}
	}	
	
	

	private final Map<String, DefaultListModel> lists = new HashMap<String, DefaultListModel>();
	private final Map<String, Integer> indexes = new HashMap<String, Integer>();
	

	private Packet handleCheckPage(final Packet p) {
		System.out.println("In handleCheckPage");
		//final Packet result = new Packet(7, 1);
		final Packet f = new Packet(p.getOpcode(), 10000);
		
		//try {
			p.reset();
			p.getInteger(); // command
			p.getInteger(); // extension
			p.getInteger(); // subcmd
			final int count = p.getInteger();
			System.out.println("Count:" + Integer.toString(count));
			// f.addInteger(count);
			final String pageId = p.getString();
			System.out.println("pageId:" + pageId);
			// f.addString(pageId);
			
			/*
			if (count == 0) {
				System.out.println("count is 0");
				p.reset();
				p.getInteger(); // command - not added				
				f.addInteger(p.getInteger()); // extension
				f.addInteger(p.getInteger()); // subcmd
				f.addInteger(p.getInteger());				
				f.addString(p.getString());
				return f;
			}
			System.out.println("count is not 0");
			*/

			
			DCRGraph d;
			if (!dcrgraphs.containsKey(pageId))
			{
				markings.put(pageId, new DCRMarking());
				d = new DCRGraph();
				dcrgraphs.put(pageId, d);
			}			
			else
			{
				//lists.get(pageId).clear();
				d = dcrgraphs.get(pageId);
			}	
			
			int newCount = count;
			if (count != 0)
			{

			
			Date date = new Date();
			System.out.println(date.toString() + "Going for count:" + Integer.toString(count));
			for (int i = 0; i < count; i++) {
				final int parameters = p.getInteger();
				//p.getString();
				final String relationID = p.getString();
				final String name = p.getString();
				final String formula = p.getString();
				final String inscription = p.getString();
				//lists.get(pageId).addElement(name.toString() + ", " + formula.toString() + "," + parameters);
				System.out.println("RelationID: " + relationID + "Name: " + name + " formula: " + formula);
				//final Constraint c = new Constraint(name, formula, parameters);
				
				String param1 = "";
				String param2 = "";
				String paramString = "";
				for (int j = 0; j < parameters; j++) {
					final String tid = p.getString();					
					paramString += tid + ",";
					//Task t = tasks.get(tid);
					//if (t == null) {
					//	t = new Task();
					//	t.setName(tid);
					//	tasks.put(tid, t);
					//}
					//c.setParameters(j, t);
					if (j == 0) param1 = tid;
					if (j == 1) param2 = tid;
					if (!d.events.contains(tid)) 
						{
							markings.get(pageId).included.add(tid);						
							d.events.add(tid);					
						}											
				}
				
				
				Tuple<String, String> relation = new Tuple<String, String>(param1, param2);
				
				if (name.equals("precedence"))
				{
					d.conditions.add(relation);
					d.relationID.put(relationID, new Tuple<Integer, Tuple<String, String>>(1, relation));
					newCount = newCount - 1;
				}
				if (name.equals("response"))
				{
					d.responses.add(relation);
					d.relationID.put(relationID, new Tuple<Integer, Tuple<String, String>>(2, relation));
					newCount = newCount - 1;
				}
				if (name.equals("include"))
				{
					d.includes.add(relation);
					d.relationID.put(relationID, new Tuple<Integer, Tuple<String, String>>(3, relation));
					newCount = newCount - 1;
				}
				if (name.equals("exclude"))
				{
					d.excludes.add(relation);
					d.relationID.put(relationID, new Tuple<Integer, Tuple<String, String>>(4, relation));
					newCount = newCount - 1;
				}
				if (name.equals("milestone"))
				{
					d.milestones.add(relation);
					d.relationID.put(relationID, new Tuple<Integer, Tuple<String, String>>(5, relation));
					newCount = newCount - 1;
				}
				
				/*if (!indexes.containsKey(pageId))
				{
					final DefaultListModel model = new DefaultListModel();
					final JList list = new JList(model);
					indexes.put(pageId, tabs.getTabCount());
					tabs.addTab(pageId, new JScrollPane(list));
					model.addElement(name.toString() + ", " + formula.toString() + "," + paramString);
					lists.put(pageId, model);					
				}
				else*/
					//lists.get(pageId).addElement(name.toString() + ", " + formula.toString() + "," + paramString);
				
				//m.addConstraint(c);
			}
			}
			
			final int delcount = p.getInteger();
			int newdelcount = delcount;
			for (int i = 0; i < delcount; i++) {
				String delid = p.getString();
				System.out.println("test1");
				if (d.relationID.containsKey(delid))
				{
					System.out.println("test2");
					int reltype = d.relationID.get(delid).getLeft();
					//if ((reltype == 1) || (reltype == 2))
					//{
						newdelcount = newdelcount - 1;
					//}
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
			
			for (int i = 0; i < count; i++) 
			{
				boolean b = true; 
				final int parameters = p.getInteger();
				final String s1 = p.getString();
				final String name = p.getString();
				final String formula = p.getString();
				final String inscription = p.getString();
				
				if ((name.equals("precedence")) || (name.equals("response")) || (name.equals("include")) || (name.equals("exclude")) || (name.equals("milestone")))
				{
					b = false;					
				}
				else
				{
					f.addInteger(parameters);
					f.addString(s1);
					f.addString(name);
					f.addString(formula);
					f.addString(inscription);
				}
				
				for (int j = 0; j < parameters; j++) 
				{
					if (!b)
					{
						final String tid = p.getString();
					}
					else
					{
						f.addString(p.getString());
					}
				}
			}			
			f.addInteger(newdelcount);
			for (int i = 0; i < delcount; i++) {
				String delid = p.getString();
				if (d.relationID.containsKey(delid))
				{
					//int reltype = d.relationID.get(delid).getLeft();
					//if (!((reltype == 1) || (reltype == 2)))
					//{
					//	f.addString(delid);
					//}
				}
				else
					f.addString(delid);
			}

			dcrgraphs.put(pageId, d);

			System.out.println("DCR Graph:");
			System.out.println(d.toString());
			System.out.println("-----------------");

			
			System.out.println("Outgoing packet:");
			System.out.println(f.toString());
			System.out.println("-----------------");
			
			//markings.put(pageId,  d.InitialMarking());
		//} catch (final Exception e) {
		//	return null;
		//}
		return f;
	}	
	
	
	

	/**
	 * @see java.util.Observer#update(java.util.Observable, java.lang.Object)
	 */
	//@Override
	//public void update(final Observable arg0, final Object arg1) {
		/*
		if (arg0 == this) {
			if (arg1 instanceof Option) {
				final Option<?> option = (Option<?>) arg1;
				if ("string".equals(option.getKey())) {
					stringOption.setText((String) getOption(option));
				} else if ("integer".equals(option.getKey())) {
					integerOption.setText("" + getOption(option));
				} else if ("boolean".equals(option.getKey())) {
					booleanOption.setSelected((Boolean) getOption(option));
				}
			}
		}
		if (arg0 instanceof Scraper && arg1 instanceof Scraper.Event) {
			final Scraper.Event event = (Scraper.Event) arg1;
			if (event.getElm() instanceof Page) {
				final Page p = (Page) event.getElm();
				switch (event.getType()) {
				case ADDED: {
					final DefaultListModel model = new DefaultListModel();
					final JList list = new JList(model);
					indexes.put(p, tabs.getTabCount());
					tabs.addTab(p.getName(), new JScrollPane(list));
					lists.put(p, model);
				}
				//$FALL-THROUGH$
				case CHANGED: {
					try {
						final DefaultListModel model = lists.get(p);
						tabs.setTitleAt(indexes.get(p), p.getName());
						final SortedSet<String> transitionNames = new TreeSet<String>();
						for (final Transition t : p.transitions()) {
							transitionNames.add(t.getName());
						}
						model.removeAllElements();
						for (final String name : transitionNames) {
							model.addElement(name);
						}
					} catch (final Exception e) {
						e.printStackTrace();
					}
				}
					break;
				case REMOVED:
					break;
				}
			}
		}*/
	//}

}
