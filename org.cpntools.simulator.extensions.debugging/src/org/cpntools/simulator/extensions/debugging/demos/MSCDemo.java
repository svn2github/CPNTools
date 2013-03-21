package org.cpntools.simulator.extensions.debugging.demos;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.HashMap;
import java.util.Map;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JPanel;
import javax.swing.JTextField;

import dk.klafbang.msc.model.MSCController;

/**
 * @author michael
 */
public class MSCDemo extends DemoPanel {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	MSCController lastChart;

	/**
	 * 
	 */
	public MSCDemo() {
		setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
		final JPanel newChart = new JPanel(new BorderLayout());
		final JComboBox internal = new JComboBox();
		final JComboBox from = new JComboBox();
		final JComboBox to = new JComboBox();
		final JComboBox start = new JComboBox();
		final JComboBox end = new JComboBox();
		final JComboBox event = new JComboBox();
		final JComboBox drop = new JComboBox();
		final Map<String, String> nameToId = new HashMap<String, String>();
		final JTextField name = new JTextField();
		newChart.add(name);
		final JButton create = new JButton("Create new chart");
		create.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(final ActionEvent arg0) {
				try {
					lastChart = new MSCController(channel, name.getText().trim());
					internal.removeAllItems();
					from.removeAllItems();
					to.removeAllItems();
					start.removeAllItems();
					end.removeAllItems();
					event.removeAllItems();
					drop.removeAllItems();
					nameToId.clear();
				} catch (final Exception e) {
					// Ignore
				}
			}
		});
		newChart.add(create, BorderLayout.EAST);
		add(newChart, BorderLayout.NORTH);

		final JPanel processPanel = new JPanel(new BorderLayout());
		final JTextField process = new JTextField();
		processPanel.add(process);
		final JButton addField = new JButton("Create process");
		addField.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(final ActionEvent arg0) {
				if (lastChart != null) {
					@SuppressWarnings("hiding")
					final String name = process.getText().trim();
					lastChart.addProcess(name);
					internal.addItem(name);
					from.addItem(name);
					to.addItem(name);
					start.addItem(name);
					end.addItem(name);
				}
			}
		});
		processPanel.add(addField, BorderLayout.EAST);
		add(processPanel);

		final JPanel internalPanel = new JPanel(new BorderLayout());
		final JTextField internalName = new JTextField();
		internalPanel.add(internal, BorderLayout.WEST);
		internalPanel.add(internalName);
		final JButton addInternal = new JButton("Create internal event");
		addInternal.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(final ActionEvent arg0) {
				if (lastChart != null) {
					try {
						lastChart
						        .addInternalEvent(internal.getSelectedItem().toString(), internalName.getText().trim());
					} catch (final Exception e) { // Ignore
					}
				}
			}
		});
		internalPanel.add(addInternal, BorderLayout.EAST);
		add(internalPanel);

		final JPanel eventField = new JPanel(new BorderLayout());
		final JPanel fields = new JPanel();
		fields.setLayout(new BoxLayout(fields, BoxLayout.X_AXIS));
		fields.add(from);
		fields.add(to);
		final JTextField eventName = new JTextField();
		fields.add(eventName);
		eventField.add(fields);
		final JButton addEvent = new JButton("Add event");
		addEvent.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(final ActionEvent arg0) {
				if (lastChart != null) {
					try {
						lastChart.addEvent(from.getSelectedItem().toString(), to.getSelectedItem().toString(),
						        eventName.getText().trim());
					} catch (final Exception e) { // Ignore
					}
				}
			}
		});
		eventField.add(addEvent, BorderLayout.EAST);
		add(eventField);

		final JPanel startPanel = new JPanel(new BorderLayout());
		final JTextField startName = new JTextField();
		startPanel.add(start, BorderLayout.WEST);
		startPanel.add(startName);
		final JButton addstart = new JButton("Start event");
		addstart.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(final ActionEvent arg0) {
				if (lastChart != null) {
					@SuppressWarnings("hiding")
					final String eventName = startName.getText().trim();
					try {
						final String id = lastChart.startEvent(start.getSelectedItem().toString(), eventName);
						final String text = eventName + ": " + start.getSelectedItem() + " ->";
						event.addItem(text);
						drop.addItem(text);
						nameToId.put(text, id);
					} catch (final Exception e) { // Ignore
					}

				}
			}
		});
		startPanel.add(addstart, BorderLayout.EAST);
		add(startPanel);

		final JPanel endPanel = new JPanel(new BorderLayout());
		endPanel.add(event, BorderLayout.WEST);
		endPanel.add(end);
		final JButton addend = new JButton("End event");
		addend.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(final ActionEvent arg0) {
				if (lastChart != null) {
					@SuppressWarnings("hiding")
					final Object name = event.getSelectedItem();
					final String id = nameToId.get(name);
					try {
						lastChart.endEvent(id, end.getSelectedItem().toString());
					} catch (final Exception e) { // Ignore
					}
					event.removeItem(name);
					drop.removeItem(name);
				}
			}
		});
		endPanel.add(addend, BorderLayout.EAST);
		add(endPanel);

		final JPanel dropPanel = new JPanel(new BorderLayout());
		dropPanel.add(drop);
		final JButton adddrop = new JButton("Drop event");
		adddrop.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(final ActionEvent arg0) {
				if (lastChart != null) {
					@SuppressWarnings("hiding")
					final Object name = event.getSelectedItem();
					final String id = nameToId.get(name);
					try {
						lastChart.dropEvent(id);
					} catch (final Exception e) { // Ignore
					}
					event.removeItem(name);
					drop.removeItem(name);
				}
			}
		});
		dropPanel.add(adddrop, BorderLayout.EAST);
		add(dropPanel);

		final JPanel linePanel = new JPanel(new BorderLayout());
		final JTextField line = new JTextField();
		linePanel.add(line);
		final JButton addline = new JButton("Add line");
		addline.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(final ActionEvent arg0) {
				if (lastChart != null) {
					final String text = line.getText().trim();
					try {
						lastChart.addLine(text);
					} catch (final Exception e) { // Ignore
					}
				}
			}
		});
		linePanel.add(addline, BorderLayout.EAST);
		add(linePanel);
	}

	/**
	 * @see org.cpntools.simulator.extensions.debugging.demos.DemoPanel#getName()
	 */
	@Override
	public String getName() {
		return "MSC";
	}
}
