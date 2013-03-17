package org.cpntools.simulator.extensions.debugging.demos;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.geom.Point2D;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import org.cpntools.simulator.extensions.graphics.charts.ScatterPlot;

/**
 * @author michael
 */
public class ScatterDemo extends DemoPanel {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	ScatterPlot lastChart;

	/**
	 * 
	 */
	public ScatterDemo() {
		setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
		final JCheckBox lineChecker = new JCheckBox();
		final JCheckBox sortedLineChecker = new JCheckBox();
		final JCheckBox trendChecker = new JCheckBox();
		final JPanel newChart = new JPanel(new BorderLayout());
		final JTextField name = new JTextField();
		newChart.add(name);
		final JButton create = new JButton("Create new chart");
		create.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(final ActionEvent arg0) {
				try {
					lastChart = new ScatterPlot(channel, name.getText().trim(), 588, 462);
					lineChecker.setSelected(false);
					sortedLineChecker.setSelected(false);
					trendChecker.setSelected(false);
				} catch (final Exception e) {
					// Ignore
				}
			}
		});
		newChart.add(create, BorderLayout.EAST);
		add(newChart, BorderLayout.NORTH);

		final JPanel field = new JPanel(new BorderLayout());
		final JPanel fields = new JPanel();
		fields.setLayout(new BoxLayout(fields, BoxLayout.X_AXIS));
		final JTextField valueX = new JTextField();
		valueX.setText("0");
		fields.add(valueX);
		final JTextField valueY = new JTextField();
		valueY.setText("0");
		fields.add(valueY);
		field.add(fields);
		final JButton addField = new JButton("Add point");
		addField.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(final ActionEvent arg0) {
				if (lastChart != null) {
					final double x = Double.parseDouble(valueX.getText());
					final double y = Double.parseDouble(valueY.getText());
					lastChart.addPoint(new Point2D.Double(x, y));
				}
			}
		});
		field.add(addField, BorderLayout.EAST);
		add(field);

		JPanel checkField = new JPanel(new BorderLayout());
		checkField.add(new JLabel("Show line"));
		checkField.add(lineChecker, BorderLayout.WEST);
		lineChecker.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(final ActionEvent arg0) {
				if (lastChart != null) {
					lastChart.setShowLine(lineChecker.isSelected());
				}
			}
		});
		add(checkField);
		checkField = new JPanel(new BorderLayout());
		checkField.add(new JLabel("Show sorted line"));
		checkField.add(sortedLineChecker, BorderLayout.WEST);
		sortedLineChecker.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(final ActionEvent arg0) {
				if (lastChart != null) {
					lastChart.setShowSortedLine(sortedLineChecker.isSelected());
				}
			}
		});
		add(checkField);
		checkField = new JPanel(new BorderLayout());
		checkField.add(new JLabel("Show trend"));
		checkField.add(trendChecker, BorderLayout.WEST);
		trendChecker.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(final ActionEvent arg0) {
				if (lastChart != null) {
					lastChart.setShowTrend(trendChecker.isSelected());
				}
			}
		});
		add(checkField);
	}

	/**
	 * @see org.cpntools.simulator.extensions.debugging.demos.DemoPanel#getName()
	 */
	@Override
	public String getName() {
		return "Scatter Plot";
	}
}
