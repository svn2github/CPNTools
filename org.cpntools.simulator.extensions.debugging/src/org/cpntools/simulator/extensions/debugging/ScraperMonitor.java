package org.cpntools.simulator.extensions.debugging;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.HashMap;
import java.util.Map;
import java.util.Observable;
import java.util.Observer;
import java.util.Timer;
import java.util.TimerTask;

import javax.swing.BorderFactory;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextArea;
import javax.swing.ListSelectionModel;
import javax.swing.SwingConstants;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import org.cpntools.simulator.extensions.Channel;
import org.cpntools.simulator.extensions.scraper.Arc;
import org.cpntools.simulator.extensions.scraper.Element;
import org.cpntools.simulator.extensions.scraper.HasName;
import org.cpntools.simulator.extensions.scraper.Node;
import org.cpntools.simulator.extensions.scraper.Page;
import org.cpntools.simulator.extensions.scraper.Place;
import org.cpntools.simulator.extensions.scraper.Scraper;
import org.cpntools.simulator.extensions.scraper.Scraper.Event;
import org.cpntools.simulator.extensions.scraper.Transition;

/**
 * @author michael
 */
public class ScraperMonitor extends DebuggingPanel implements Observer {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private final JTabbedPane tabs;
	final JTextArea log;
	private final Map<String, PagePanel> pages = new HashMap<String, ScraperMonitor.PagePanel>();
	final Timer timer;
	TimerTask milestoner;
	protected boolean auto;

	private final class Milestoner extends TimerTask {
		public Milestoner() {
		}

		@Override
		public void run() {
			log("=================================================");
		}
	}

	private static class PagePanel extends JPanel {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		private final int index;
		private final Page page;
		private final DefaultListModel placeModel;
		private final DefaultListModel transitionModel;
		Node selected = null;
		boolean propagate = true;

		public PagePanel(final int index, final Page page) {
			this.index = index;
			this.page = page;

			setLayout(new BorderLayout());

			final JPanel arcs = new JPanel();
			arcs.setLayout(new GridLayout(3, 2));
			in = new DefaultListModel();
			arcs.add(border("In", new JScrollPane(new JList(in))));
			out = new DefaultListModel();
			arcs.add(border("Out", new JScrollPane(new JList(out))));
			test = new DefaultListModel();
			arcs.add(border("Test", new JScrollPane(new JList(test))));
			reset = new DefaultListModel();
			arcs.add(border("Reset", new JScrollPane(new JList(reset))));
			inhibitor = new DefaultListModel();
			arcs.add(border("Inhibitor", new JScrollPane(new JList(inhibitor))));
			info = new JTextArea();
			arcs.add(border("Info", new JScrollPane(info)));
			placeModel = new DefaultListModel();
			transitionModel = new DefaultListModel();
			places = new JList(placeModel);
			places.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
			transitions = new JList(transitionModel);
			transitions.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
			places.addListSelectionListener(new ListSelectionListener() {
				@Override
				public void valueChanged(final ListSelectionEvent arg0) {
					final NodeViewer view = (NodeViewer) places.getSelectedValue();
					if (view == null) { return; }
					final boolean old = propagate;
					propagate = false;
					if (old) {
						transitions.clearSelection();
						showNode(view.getNode());
						showArcs(view.getNode());
						propagate = true;
					}
				}
			});
			transitions.addListSelectionListener(new ListSelectionListener() {
				@Override
				public void valueChanged(final ListSelectionEvent arg0) {
					final NodeViewer view = (NodeViewer) transitions.getSelectedValue();
					if (view == null) { return; }
					final boolean old = propagate;
					propagate = false;
					if (old) {
						places.clearSelection();
						showNode(view.getNode());
						showArcs(view.getNode());
						propagate = true;
					}
				}
			});
			add(new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, true, new JSplitPane(JSplitPane.VERTICAL_SPLIT, true,
			        border("Places", new JScrollPane(places)), border("Transitions", new JScrollPane(transitions))),
			        arcs));
		}

		void showNode(final Node n) {
			selected = n;
			info.setText("");
			if (n instanceof Place) {
				final Place place = (Place) n;
				info.append("Type: " + place.getType());
				info.append("\nInitmark: " + place.getInitMark());
			} else if (n instanceof Transition) {
				final Transition transition = (Transition) n;
				info.append("Time: " + transition.getTime());
				info.append("\nGuard: " + transition.getGuard());
				info.append("\nPriority: " + transition.getPriority());
				info.append("\nChannel: " + transition.getChannel());
				info.append("\nCode: " + transition.getCode());
			} else {
				info.append("Unknown node");
			}
		}

		void showArcs(final Node place) {
			showArcs(place, place.in(), in);
			showArcs(place, place.out(), out);
			showArcs(place, place.test(), test);
			showArcs(place, place.reset(), reset);
			showArcs(place, place.inhibitor(), inhibitor);
		}

		private void showArcs(final Node node, final Iterable<Arc> arcs, final DefaultListModel list) {
			list.clear();
			for (final Arc a : arcs) {
				final Node other = other(a, node);
				list.addElement(other.getName() + " [" + other.getId() + "]: " + a.getInscription() + " [" + a.getId()
				        + "]");
			}
		}

		private Node other(final Arc a, final Node node) {
			final Node n = a.getPlace();
			if (n == node) { return a.getTransition(); }
			return n;
		}

		private JPanel border(final String string, final JComponent component) {
			final JPanel result = new JPanel(new BorderLayout());
			result.setBorder(BorderFactory.createTitledBorder(string));
			result.add(component);
			return result;
		}

		private final class NodeViewer {
			private final Node n;

			public NodeViewer(final Node n) {
				this.n = n;

			}

			public Node getNode() {
				return n;
			}

			@Override
			public String toString() {
				return n.getName() + " [" + n.getId() + "]";
			}
		}

		Map<String, NodeViewer> views = new HashMap<String, ScraperMonitor.PagePanel.NodeViewer>();
		final JList places;
		final JList transitions;
		final JTextArea info;
		private final DefaultListModel inhibitor;
		private final DefaultListModel reset;
		private final DefaultListModel test;
		private final DefaultListModel out;
		private final DefaultListModel in;

		public void addPlace(final Place p) {
			final NodeViewer view = new NodeViewer(p);
			views.put(p.getId(), view);
			placeModel.addElement(view);
		}

		public void addTransition(final Transition elm) {
			final NodeViewer view = new NodeViewer(elm);
			views.put(elm.getId(), view);
			transitionModel.addElement(view);
		}

		public void removePlace(final Place p) {
			placeModel.removeElement(views.remove(p.getId()));
			if (p == selected) {
				clear();
			}
		}

		private void clear() {
			info.setText("");
			in.clear();
			out.clear();
			test.clear();
			reset.clear();
			inhibitor.clear();
		}

		public void removeTransition(final Transition elm) {
			transitionModel.removeElement(views.remove(elm.getId()));
			if (elm == selected) {
				clear();
			}
		}

		public void changedPlace(final Place p) {
			places.repaint();
			if (p == selected) {
				showNode(p);
			}
		}

		public void changedTransition(final Transition elm) {
			transitions.repaint();
			if (elm == selected) {
				showNode(elm);
			}
		}

		public int getIndex() {
			return index;
		}

		@SuppressWarnings("unused")
		public Page getPage() {
			return page;
		}

		public void changedArcs(final Node elm) {
			if (elm == selected) {
				showArcs(elm);
			}
		}
	}

	/**
	 * 
	 */
	public ScraperMonitor() {
		tabs = new JTabbedPane(SwingConstants.LEFT);
		log = new JTextArea();
		log.setEditable(false);
		add(new JSplitPane(JSplitPane.VERTICAL_SPLIT, true, tabs, new JScrollPane(log)));
		final JPanel buttons = new JPanel(new FlowLayout(FlowLayout.CENTER));
		final JButton clear = new JButton("Clear log");
		clear.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(final ActionEvent e) {
				log.setText("");
			}
		});
		buttons.add(clear);
		final JButton milestone = new JButton("Milestone");
		milestone.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(final ActionEvent arg0) {
				log("=================================================");
			}
		});
		buttons.add(milestone);
		final JButton autoMilestone = new JButton("Auto milestone");
		timer = new Timer("Auto milestone scheduler", true);
		milestoner = new Milestoner();
		final JButton stopMilestone = new JButton("Stop milestone");
		stopMilestone.setEnabled(false);
		autoMilestone.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(final ActionEvent e) {
				auto = true;
				autoMilestone.setEnabled(false);
				stopMilestone.setEnabled(true);
				new Milestoner().run();
			}
		});
		stopMilestone.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(final ActionEvent e) {
				if (milestoner != null) {
					milestoner.cancel();
				}
				timer.purge();
				auto = false;
				autoMilestone.setEnabled(true);
				stopMilestone.setEnabled(false);
			}
		});
		buttons.add(autoMilestone);
		buttons.add(stopMilestone);
		add(buttons, BorderLayout.SOUTH);

	}

	/**
	 * @see org.cpntools.simulator.extensions.debugging.DebuggingPanel#getName()
	 */
	@Override
	public String getName() {
		return "Scraper";
	}

	/**
	 * @see org.cpntools.simulator.extensions.debugging.DebuggingPanel#setChannel(org.cpntools.simulator.extensions.debugging.DebuggingExtension,
	 *      org.cpntools.simulator.extensions.Channel)
	 */
	@Override
	public void setChannel(final DebuggingExtension orphanage, final Channel c) {
		super.setChannel(orphanage, c);
		if (c != null) {
			final Scraper scraper = c.getExtension(Scraper.class);
			if (scraper != null) {
				scraper.addObserver(this);
			}
		}
	}

	/**
	 * @see java.util.Observer#update(java.util.Observable, java.lang.Object)
	 */
	@Override
	public void update(final Observable arg0, final Object arg1) {
		if (arg0 instanceof Scraper && arg1 instanceof Event) {
			if (auto) {
				if (milestoner != null) {
					milestoner.cancel();
				}
				timer.purge();
				milestoner = new Milestoner();
				timer.schedule(milestoner, 5000);
			}
			final Event e = (Event) arg1;
			switch (e.getType()) {
			case ADDED:
				added(e.getElm());
				break;
			case REMOVED:
				removed(e.getElm());
				break;
			case CHANGED:
				changed(e.getElm());
				break;
			case ARC_CHANGED:
				arcChanged(e.getElm());
				break;
			}
		} else {
			log("UNEXPECTED: update with " + arg0 + " and " + arg1);
		}

	}

	private void arcChanged(final Element elm) {
		log("Changed arcs " + e(elm));
		if (elm instanceof Node) {
			final Node n = (Node) elm;
			final PagePanel p = pages.get(n.getPage().getId());
			p.changedArcs(n);
		} else {
			log("UNEXPECTED: arcChanged(Node) with " + elm);
		}
	}

	private void changed(final Page elm) {
		final PagePanel p = pages.get(elm.getId());
		tabs.setTitleAt(p.getIndex(), elm.getName());
	}

	private void changed(final Node elm) {
		final PagePanel p = pages.get(elm.getPage().getId());
		if (elm instanceof Place) {
			p.changedPlace((Place) elm);
		} else if (elm instanceof Transition) {
			p.changedTransition((Transition) elm);
		} else {
			log("UNEXPECTED: changed(Node) with " + elm);
		}
	}

	private void changed(final Arc elm) {
		log("UNEXPECTED: changed(Arc) with " + elm);
	}

	private void changed(final Element elm) {
		log("Changed " + e(elm));
		if (elm instanceof Page) {
			changed((Page) elm);
		} else if (elm instanceof Node) {
			changed((Node) elm);
		} else if (elm instanceof Arc) {
			changed((Arc) elm);
		} else {
			log("UNEXPECTED: changed(Element) with " + elm);
		}
	}

	private void removed(final Page elm) {
		// FIXME Handle removal of pages
	}

	private void removed(final Node elm) {
		final PagePanel p = pages.get(elm.getPage().getId());
		if (elm instanceof Place) {
			p.removePlace((Place) elm);
		} else if (elm instanceof Transition) {
			p.removeTransition((Transition) elm);
		} else {
			log("UNEXPECTED: removed(Node) with " + elm);
		}
	}

	private void removed(final Arc elm) {
		log("UNEXPECTED: removed(Arc)");
	}

	private void removed(final Element elm) {
		log("Removed " + e(elm));
		if (elm instanceof Page) {
			removed((Page) elm);
		} else if (elm instanceof Node) {
			removed((Node) elm);
		} else if (elm instanceof Arc) {
			removed((Arc) elm);
		} else {
			log("UNEXPECTED: removed(Element) with " + elm);
		}
	}

	private synchronized void added(final Page elm) {
		final int index = tabs.getTabCount();
		final PagePanel p = new PagePanel(index, elm);
		tabs.addTab(elm.getName(), p);
		pages.put(elm.getId(), p);
	}

	private void added(final Node elm) {
		final PagePanel p = pages.get(elm.getPage().getId());
		if (elm instanceof Place) {
			p.addPlace((Place) elm);
		} else if (elm instanceof Transition) {
			p.addTransition((Transition) elm);
		} else {
			log("UNEXPECTED: added(Node) with " + elm);
		}
	}

	private void added(final Arc elm) {
		log("UNEXPECTED: added(Arc)");
	}

	void log(final String msg) {
		log.append(msg + '\n');
		log.setCaretPosition(log.getText().length());
	}

	private String e(final Element elm) {
		if (elm instanceof HasName) { return elm.getClass().getSimpleName() + " [" + ((HasName) elm).getName() + "; "
		        + elm.getId() + "]"; }
		return elm.getClass().getSimpleName() + " [" + elm.getId() + "]";
	}

	private void added(final Element elm) {
		log("Adding " + e(elm));
		if (elm instanceof Page) {
			added((Page) elm);
		} else if (elm instanceof Node) {
			added((Node) elm);
		} else if (elm instanceof Arc) {
			added((Arc) elm);
		} else {
			log("UNEXPECTED: added(Element) with " + elm);
		}
	}
}
