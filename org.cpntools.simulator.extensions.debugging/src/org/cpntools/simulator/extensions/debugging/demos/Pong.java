package org.cpntools.simulator.extensions.debugging.demos;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;

import org.cpntools.simulator.extensions.graphics.Canvas;
import org.cpntools.simulator.extensions.graphics.Ellipsis;
import org.cpntools.simulator.extensions.graphics.Rectangle;

/**
 * @author michael
 */
public class Pong extends DemoPanel {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * 
	 */
	public Pong() {
		final JButton button = new JButton("Start Pong");
		add(button);
		button.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(final ActionEvent e) {
				try {
					final Canvas c = new Canvas(channel, "Pong");
					c.suspend(true);
					c.add(new Rectangle(new java.awt.Rectangle(0, 0, 600, 20))).setBackground(Color.BLACK);
					c.add(new Rectangle(new java.awt.Rectangle(0, 460, 600, 20))).setBackground(Color.BLACK);
					c.add(new Rectangle(new java.awt.Rectangle(580, 20, 20, 440))).setBackground(Color.BLACK);
					c.add(new Rectangle(new java.awt.Rectangle(0, 20, 20, 440))).setBackground(Color.BLACK);
					c.add(new Ellipsis(new Point(350, 200), new Dimension(15, 15)));
					c.add(new Rectangle(new java.awt.Rectangle(550, 180, 20, 100)));
					final Rectangle pad = c.add(new Rectangle(new java.awt.Rectangle(50, 280, 20, 100)));
					pad.subscribe();
					c.suspend(false);
					c.center();
				} catch (final Exception e1) {
					// Ignore
				}
			}
		});
	}

	/**
	 * @see org.cpntools.simulator.extensions.debugging.demos.DemoPanel#getName()
	 */
	@Override
	public String getName() {
		return "Pong";
	}

}
