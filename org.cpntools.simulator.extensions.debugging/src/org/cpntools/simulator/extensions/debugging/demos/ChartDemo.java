package org.cpntools.simulator.extensions.debugging.demos;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.HashMap;
import java.util.Map;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JColorChooser;
import javax.swing.JComboBox;
import javax.swing.JPanel;
import javax.swing.JTextField;

import org.cpntools.simulator.extensions.graphics.charts.Bar;
import org.cpntools.simulator.extensions.graphics.charts.BarChart;

/**
 * @author michael
 */
public class ChartDemo extends DemoPanel {

	@Override
	public String getName() {
		return "Bar Chart";
	}

	BarChart lastChart;
	Map<String, Bar> bars;

	public ChartDemo() {
		setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
		final JComboBox barsDropdown = new JComboBox();
		final JComboBox fieldName = new JComboBox();

		final JColorChooser colorChooser = new JColorChooser(Color.RED);
		add(colorChooser);
		final JPanel newChart = new JPanel(new BorderLayout());
		final JTextField name = new JTextField();
		newChart.add(name);
		final JButton create = new JButton("Create new chart");
		create.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(final ActionEvent arg0) {
				try {
					lastChart = new BarChart(channel, name.getText().trim(), 588, 462, colorChooser.getColor());
					bars = new HashMap<String, Bar>();
					barsDropdown.removeAllItems();
					fieldName.removeAllItems();
				} catch (final Exception e) {
				}
			}
		});
		newChart.add(create, BorderLayout.EAST);
		add(newChart, BorderLayout.NORTH);

		final JPanel field = new JPanel(new BorderLayout());
		final JPanel fields = new JPanel();
		fields.setLayout(new BoxLayout(fields, BoxLayout.X_AXIS));
		final JTextField value = new JTextField();
		value.setText("0");
		fields.add(value);
		fieldName.setEditable(true);
		fields.add(fieldName);
		field.add(fields);
		final JButton addField = new JButton("Add/modify bar");
		addField.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(final ActionEvent arg0) {
				if (lastChart != null) {
					final String name = fieldName.getSelectedItem().toString().trim();
					final int height = Integer.parseInt(value.getText());
					final Bar b = bars.get(name);
					if (b == null) {
						bars.put(name, lastChart.addBar(name, height));
						barsDropdown.addItem(name);
						fieldName.addItem(name);
					} else {
						b.setValue(height);
					}
				}
			}
		});
		field.add(addField, BorderLayout.EAST);
		add(field);

		final JPanel delete = new JPanel(new BorderLayout());
		delete.add(barsDropdown);
		final JButton removeButton = new JButton("Remove bar");
		removeButton.addActionListener(new ActionListener() {

			@Override
			public void actionPerformed(final ActionEvent arg0) {
				final Object selectedItem = barsDropdown.getSelectedItem();
				final String barName = selectedItem.toString();
				final Bar bar = bars.remove(barName);
				barsDropdown.removeItem(selectedItem);
				fieldName.removeItem(selectedItem);
				bar.delete();
			}
		});
		delete.add(removeButton, BorderLayout.EAST);
		add(delete);
	}
}
