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
import org.cpntools.simulator.extensions.scraper.Page;
import org.cpntools.simulator.extensions.scraper.Scraper;
import org.cpntools.simulator.extensions.scraper.Transition;
import org.cpntools.simulator.extensions.server.Handler;

public class DCRExtension  extends AbstractExtension {
	
	public static final int ID = 10001;
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
	public DCRExtension() {
		//addOption(Option.create("String", "string", String.class), Option.create("Integer", "integer", Integer.class),
		//        Option.create("Boolean", "boolean", Boolean.class));
		addSubscription(//new Command(200, 9), 
				new Command(400, 2), // Syntax check page
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
		//addObserver(this);
	}

	@Override
	public void setChannel(final Channel c) {
		super.setChannel(c);

		try {
			dialog = new JDialog((Frame) null, "DCR Extension", false);
			dialog.setSize(new Dimension(600, 400));
			dialog.setLayout(new BorderLayout());
			tabs = new JTabbedPane(SwingConstants.LEFT, JTabbedPane.SCROLL_TAB_LAYOUT);
			final JPanel rightPanel = new JPanel();
			rightPanel.setLayout(new BoxLayout(rightPanel, BoxLayout.Y_AXIS));
			final JPanel stringPanel = new JPanel(new BorderLayout());
			stringPanel.setBorder(BorderFactory.createTitledBorder("String"));
			stringOption = new JTextField();
			stringOption.setEditable(false);
			stringPanel.add(stringOption);
			rightPanel.add(stringPanel);
			final JPanel integerPanel = new JPanel(new BorderLayout());
			integerPanel.setBorder(BorderFactory.createTitledBorder("Integer"));
			integerOption = new JTextField();
			integerOption.setEditable(false);
			integerPanel.add(integerOption);
			rightPanel.add(integerPanel);
			final JPanel booleanPanel = new JPanel(new BorderLayout());
			booleanPanel.setBorder(BorderFactory.createTitledBorder("Boolean"));
			booleanOption = new JCheckBox();
			booleanPanel.add(booleanOption);
			rightPanel.add(booleanPanel);
			final JSplitPane pane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, tabs, rightPanel);
			dialog.add(pane);
			dialog.setVisible(true);
			pane.setDividerLocation(0.66);

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
		return "DCR Extension";
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
	
	@Override
	public Packet handle(final Packet p, final Packet response) {
		p.reset();
		final int command = p.getInteger();
		final int subcommand = p.getInteger();
		if (command == 200 && subcommand == 9) {
			dialog.setTitle("DCR Extension [" + p.getString() + "]");
		}
		if (command == 500) {			
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
	
	private Packet enabled(final Packet p, final Packet response) {
		p.reset();
		if (p.getBoolean()) {
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
				result.addBoolean(false);
			}
			result.addString(response.getString());
		}
		return result;
	}	
	
	private void execute(final Packet p) {
		p.reset();
		String task = p.getString();
		for (final String pageId : new ArrayList<String>(dcrgraphs.keySet())) {
			final DCRGraph d = dcrgraphs.get(pageId);
			final DCRMarking m = markings.get(pageId);
			DCRMarking next = d.Execute(m, task);
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
		final Packet result = new Packet(7, 1);
		try {
			p.reset();
			p.getInteger(); // command
			p.getInteger(); // extension
			p.getInteger(); // subcmd
			final int count = p.getInteger();
			final String pageId = p.getString();
			if (count == 0) {
				return result;
			}
			
			DCRGraph oldD = null;
			if (!indexes.containsKey(pageId))
			{
				final DefaultListModel model = new DefaultListModel();
				final JList list = new JList(model);
				indexes.put(pageId, tabs.getTabCount());
				tabs.addTab(pageId, new JScrollPane(list));				
				lists.put(pageId, model);
				markings.put(pageId, new DCRMarking());
			}			
			else
			{
				lists.get(pageId).clear();
				oldD = dcrgraphs.get(pageId);
			}			
			
			DCRGraph d = new DCRGraph();
			
			for (int i = 0; i < count; i++) {
				final int parameters = p.getInteger();
				p.getString();
				final String name = p.getString();
				final String formula = p.getString();
				//lists.get(pageId).addElement(name.toString() + ", " + formula.toString() + "," + parameters);
				
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
					if (!d.events.contains(tid)) d.events.add(tid);					
					
					if (oldD == null)
						markings.get(pageId).included.add(tid);
					else
						if (!oldD.events.contains(tid)) 
							markings.get(pageId).included.add(tid);					
				}
				
				
				if (name.equals("precedence"))
					d.conditions.add(new Tuple<String, String>(param1, param2));				
				if (name.equals("response"))
					d.responses.add(new Tuple<String, String>(param1, param2));
				if (name.equals("include"))
					d.includes.add(new Tuple<String, String>(param1, param2));
				if (name.equals("exclude"))
					d.excludes.add(new Tuple<String, String>(param1, param2));
				if (name.equals("milestone"))
					d.milestones.add(new Tuple<String, String>(param1, param2));				
				
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
					lists.get(pageId).addElement(name.toString() + ", " + formula.toString() + "," + paramString);
				
				//m.addConstraint(c);
			}
			dcrgraphs.put(pageId, d);
			//markings.put(pageId,  d.InitialMarking());
		} catch (final Exception e) {
			result.addBoolean(false);
			result.addString(e.toString());
		}
		return result;
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
