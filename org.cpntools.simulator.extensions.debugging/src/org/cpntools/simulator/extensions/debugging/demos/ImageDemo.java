package org.cpntools.simulator.extensions.debugging.demos;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.image.BufferedImage;

import javax.imageio.ImageIO;
import javax.swing.JButton;
import javax.swing.JFileChooser;

import org.cpntools.simulator.extensions.graphics.Canvas;
import org.cpntools.simulator.extensions.graphics.Rectangle;
import org.cpntools.simulator.extensions.graphics.charts.BarChart;

/**
 * @author michael
 */
public class ImageDemo extends DemoPanel {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	BarChart lastChart;

	/**
	 * 
	 */
	public ImageDemo() {
		final JButton button = new JButton("Load...");
		button.addActionListener(new ActionListener() {

			@Override
			public void actionPerformed(final ActionEvent e) {
				final JFileChooser dialog = new JFileChooser();
				dialog.showOpenDialog(ImageDemo.this);
				try {
					final BufferedImage image = ImageIO.read(dialog.getSelectedFile());
					final int size = Math.max(image.getHeight() / 30, image.getWidth() / 30);
					final Image scaledImage = image.getScaledInstance(image.getWidth() / size,
					        image.getHeight() / size, Image.SCALE_SMOOTH);
					final BufferedImage imageBuff = new BufferedImage(image.getWidth() / size,
					        image.getHeight() / size, BufferedImage.TYPE_INT_RGB);
					final Graphics g = imageBuff.createGraphics();
					g.drawImage(scaledImage, 0, 0, new Color(0, 0, 0), null);
					g.dispose();
					final Canvas c = new Canvas(channel, dialog.getSelectedFile().getName());
					for (int x = 0; x < imageBuff.getWidth(); x++) {
						c.suspend(true);
						for (int y = 0; y < imageBuff.getHeight(); y++) {
							final Rectangle r = new Rectangle(x * size, -y * size, size, size);
							final Color color = new Color(imageBuff.getRGB(x, y));
							r.setBackground(color);
							r.setForeground(color);
							c.add(r);
						}
						c.center(true);
						c.suspend(false);
					}
				} catch (final Exception e1) {
					// TODO Auto-generated catch block
					e1.printStackTrace();
				}

			}
		});
		add(button);
	}

	/**
	 * @see org.cpntools.simulator.extensions.debugging.demos.DemoPanel#getName()
	 */
	@Override
	public String getName() {
		return "Image";
	}
}
