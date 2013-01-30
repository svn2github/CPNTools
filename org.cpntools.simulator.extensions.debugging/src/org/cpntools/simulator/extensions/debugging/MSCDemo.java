package org.cpntools.simulator.extensions.debugging;

import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;

import org.cpntools.accesscpn.engine.protocol.Packet;

public class MSCDemo extends DebuggingPanel {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private int step = 0;
	private String canvas = null;
	private int pos;
	String p1, p2, p3;
	String highlighted = null;

	public MSCDemo() {
		final JButton button = new JButton("Start Demo");
		add(button);
		button.addActionListener(new ActionListener() {

			@Override
			public void actionPerformed(final ActionEvent e) {
				if (step == 0) {
					try {
						canvas = createCanvas("Message Sequence Chart");
						button.setText("Step 2");
						step = 1;
					} catch (final Exception _) {

					}
				} else if (step == 1) {
					try {
						p1 = createProcess(canvas, "Sender", -168);
						p2 = createProcess(canvas, "Network", 0);
						p3 = createProcess(canvas, "Receiver", 168);
						button.setText("Step 3");
						step = 2;
					} catch (final Exception _) {

					}
				} else if (step == 2) {
					try {
						createInternal(canvas, p1, -168, "Compute");
						sleep(500);
						createMessage(canvas, p1, -168, 0, "Send");
						sleep(500);
						createInternal(canvas, p2, 0, "Process");
						sleep(500);
						createMessage(canvas, p2, 0, 168, "Forward");
						sleep(500);
						createInternal(canvas, p3, 168, "Verify");
						sleep(500);
						createMessage(canvas, p3, 168, 0, "Respond");
						sleep(500);
						createMessage(canvas, p2, 0, -168, "Forward");
						sleep(500);
						lolight();
						button.setText("Start Again");
						step = 0;
					} catch (final Exception _) {

					}
				}
			}

			private void sleep(final int i) {
				try {
					Thread.sleep(i);
				} catch (final InterruptedException e) {
				}
			}
		});
	}

	protected void createMessage(final String canvas, final String pid, final int x1, final int x2, final String string)
	        throws Exception {
		highlight(pid);
		pos++;
		final int dx = x1 < x2 ? -10 : 10;
		createLine(canvas, new Point(x1, -168 + 42 * pos), new Point(x2, -168 + 42 * pos), new Point(x2 + dx, -168 + 42
		        * pos - 10), new Point(x2 + dx, -168 + 42 * pos + 10), new Point(x2, -168 + 42 * pos));
		createText(canvas, Math.min(x1, x2) + 21, -168 + 42 * pos - 5, string);
	}

	protected void createInternal(final String canvas, final String pid, final int x, final String string)
	        throws Exception {
		highlight(pid);
		pos++;
		createBox(canvas, x, -168 + 42 * pos, 5, 5);
		createText(canvas, x + 5, -168 + 42 * pos - 5, string);
	}

	protected void highlight(final String pid) throws Exception {
		lolight();
		setBackground(pid, "green");
		highlighted = pid;
	}

	private void lolight() throws Exception {
		if (highlighted != null) {
			setBackground(highlighted, "white");
		}
		highlighted = null;
	}

	private void setBackground(final String pid, final String color) throws Exception {
		Packet p = new Packet(3, 4);
		p.addString(pid);
		p.addString("black");
		p.addString(color);
		p = channel.send(p);
		if (p.getInteger() == 1) { return; }
		throw new Exception("Wrong result");
	}

	protected String createProcess(final String canvas, final String name, final int x) throws Exception {
// createVerticalGuideline(canvas, x);
		createLine(canvas, new Point(x, -168), new Point(x, 168));
		createBox(canvas, x, 168, 21, 10);
		final String id = createBox(canvas, x, -168, 126, 42);
		createText(canvas, x - 40, -163, name);
		return id;
	}

	protected void createVerticalGuideline(final String canvas, final int x) throws Exception {
		Packet p = new Packet(3, 3);
		p.addString(canvas);
		p.addInteger(12);
		p.addInteger(x);
		p = channel.send(p);
		if (p.getInteger() == 1) { return; }
		throw new Exception("Wrong result");
	}

	protected String createText(final String canvas, final int x, final int y, final String name) throws Exception {
		Packet p = new Packet(3, 3);
		p.addString(canvas);
		p.addInteger(3);
		p.addInteger(x);
		p.addInteger(y);
		p.addString(name);
		p = channel.send(p);
		if (p.getInteger() == 1) { return p.getString(); }
		throw new Exception("Wrong result");
	}

	protected String createLine(final String canvas, final Point... points) throws Exception {
		Packet p = new Packet(3, 3);
		p.addString(canvas);
		p.addInteger(4);
		p.addInteger(points.length);
		for (final Point point : points) {
			p.addInteger((int) point.getX());
			p.addInteger((int) point.getY());
		}
		p = channel.send(p);
		if (p.getInteger() == 1) { return p.getString(); }
		throw new Exception("Wrong result");
	}

	protected String createBox(final String canvas, final int x, final int y, final int w, final int h)
	        throws Exception {
		Packet p = new Packet(3, 3);
		p.addString(canvas);
		p.addInteger(1);
		p.addInteger(x);
		p.addInteger(y);
		p.addInteger(w);
		p.addInteger(h);
		p = channel.send(p);
		if (p.getInteger() == 1) { return p.getString(); }
		throw new Exception("Wrong result");
	}

	protected String createCanvas(final String name) throws Exception {
		Packet p = new Packet(3, 2);
		p.addBoolean(false);
		p.addBoolean(false);
		p.addString(name);
		p = channel.send(p);
		if (p.getInteger() == 1) { return p.getString(); }
		throw new Exception("Wrong result");
	}

	@Override
	public String getName() {
		return "Demo";
	}

}
