package org.cpntools.simulator.extensions.debugging.demos;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;

import org.cpntools.simulator.extensions.graphics.Canvas;
import org.cpntools.simulator.extensions.graphics.Ellipse;
import org.cpntools.simulator.extensions.graphics.Rectangle;
import org.cpntools.simulator.extensions.graphics.Text;

/**
 * @author michael
 */
public class Pong extends DemoPanel {

	/**
	 * @author michael
	 */
	public class PlayerThread extends Thread {

		private final static int BALL_MOVE = 10;
		private final static int COMPUTER_MOVE = 4;
		private int angle = 45;
		private final Ellipse ball;
		private final Rectangle[] bounds;
		private final Canvas canvas;
		private final Rectangle computerPad;
		private Text computerScore;
		private final Rectangle playerPad;

		private Text playerScore;

		int player = 0, computer = 0;

		/**
		 * @param c
		 * @param ball
		 * @param computerPad
		 * @param playerPad
		 * @param bounds
		 */
		public PlayerThread(final Canvas c, final Ellipse ball, final Rectangle computerPad,
		        final Rectangle playerPad, final Rectangle... bounds) {
			super("PongPlayer");
			canvas = c;
			this.ball = ball;
			this.computerPad = computerPad;
			this.playerPad = playerPad;
			this.bounds = bounds;
			try {
				playerScore = new Text(100, 50, "0");
				canvas.add(playerScore);
				computerScore = new Text(500, 50, "0");
				canvas.add(computerScore);
			} catch (final Exception _) {
				// Ignore
			}
		}

		/**
		 * @see java.lang.Thread#run()
		 */
		@Override
		public void run() {
			while (true) {
				try {
					int padY = (int) computerPad.getBounds().getCenterY();
					final int ballY = (int) ball.getBounds().getCenterY();
					canvas.suspend(true);
					if (ballY < padY) {
						computerPad.move(new Point(0, -PlayerThread.COMPUTER_MOVE));
					}
					if (ballY > padY) {
						computerPad.move(new Point(0, PlayerThread.COMPUTER_MOVE));
					}
					padY = (int) playerPad.getBounds().getCenterY();
					if (ballY < padY) {
						playerPad.move(new Point(0, -PlayerThread.COMPUTER_MOVE));
					}
					if (ballY > padY) {
						playerPad.move(new Point(0, PlayerThread.COMPUTER_MOVE));
					}
					for (final Rectangle bound : bounds) {
						angle = bounce(bound, ball, angle, false);
					}
					angle = bounce(computerPad, ball, angle, true);
					angle = bounce(playerPad, ball, angle, true);
					final double radian = angle * Math.PI / 180;
					final double dx = Math.cos(radian) * PlayerThread.BALL_MOVE;
					final double dy = Math.sin(radian) * PlayerThread.BALL_MOVE;
					ball.move(new Point((int) Math.round(dx), (int) Math.round(dy)));
					canvas.suspend(false);
					Thread.sleep(100);
				} catch (final Exception _) {
					// Ignore
				}
			}
		}

		@SuppressWarnings("hiding")
		private int bounce(final Rectangle r, final Ellipse ball, final int angle, final boolean pad) throws Exception {
			final java.awt.Rectangle bounds = r.getBounds();
			final java.awt.Rectangle ballBounds = ball.getBounds();
			if (ballBounds.getCenterY() < bounds.getMaxY() && ballBounds.getCenterY() > bounds.getMinY()) {
				// Bounce on vertical wall
				if (ballBounds.getMaxX() >= bounds.getMinX() && ballBounds.getMaxX() < bounds.getMaxX()) {
					if (!pad) {
						player++;
						canvas.remove(playerScore);
						playerScore = new Text(100, 50, "" + player);
						canvas.add(playerScore);
					}
					return 180 - angle;
				}
				if (ballBounds.getMinX() <= bounds.getMaxX() && ballBounds.getMinX() > bounds.getMinX()) {
					if (!pad) {
						computer++;
						canvas.remove(computerScore);
						computerScore = new Text(500, 50, "" + computer);
						canvas.add(computerScore);
					}
					return 180 - angle;
				}
			}
			if (ballBounds.getCenterX() < bounds.getMaxX() && ballBounds.getCenterX() > bounds.getMinX()) {
				// Bounce on horizontal wall
				if (ballBounds.getMaxY() >= bounds.getMinY() && ballBounds.getMaxY() < bounds.getMaxY()) { return 360 - angle; }
				if (ballBounds.getMinY() <= bounds.getMaxY() && ballBounds.getMinY() > bounds.getMinY()) { return 360 - angle; }
			}
			return angle;
		}
	}

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
					final Rectangle r1 = c.add(new Rectangle(new java.awt.Rectangle(0, 0, 600, 20))).setBackground(
					        Color.BLACK);
					final Rectangle r2 = c.add(new Rectangle(new java.awt.Rectangle(0, 460, 600, 20))).setBackground(
					        Color.BLACK);
					final Rectangle r3 = c.add(new Rectangle(new java.awt.Rectangle(580, 20, 20, 440))).setBackground(
					        Color.BLACK);
					final Rectangle r4 = c.add(new Rectangle(new java.awt.Rectangle(0, 20, 20, 440))).setBackground(
					        Color.BLACK);
					final Ellipse ball = c.add(new Ellipse(new Point(350, 200), new Dimension(15, 15)));
					final Rectangle computerPad = c.add(new Rectangle(new java.awt.Rectangle(530, 180, 40, 100)));
					final Rectangle playerPad = c.add(new Rectangle(new java.awt.Rectangle(30, 280, 40, 100)));
					c.suspend(false);
					c.center();
					new PlayerThread(c, ball, computerPad, playerPad, r1, r2, r3, r4).start();
				} catch (final Exception e1) {
					e1.printStackTrace();
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
