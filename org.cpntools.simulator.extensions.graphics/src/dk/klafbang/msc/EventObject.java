/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *  BRITNeY Suite                                                          *
 *                                                                         *
 *  Copyright (C) 2004-2006 Michael Westergaard and others                 *
 *                                                                         *
 *  This program is free software; you can redistribute it and/or          *
 *  modify it under the terms of the GNU General Public License            *
 *  as published by the Free Software Foundation; either version 2         *
 *  of the License, or (at your option) any later version.                 *
 *                                                                         *
 *  This program is distributed in the hope that it will be useful,        *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of         *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the          *
 *  GNU General Public License for more details.                           *
 *                                                                         *
 *  You should have received a copy of the GNU General Public License      *
 *  along with this program; if not, write to the Free Software            *
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, *
 *  USA.                                                                   *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
package dk.klafbang.msc;

import java.awt.Point;

import org.cpntools.simulator.extensions.graphics.Group;
import org.cpntools.simulator.extensions.graphics.Line;
import org.cpntools.simulator.extensions.graphics.Text;

import dk.klafbang.msc.model.SimpleEvent;

/**
 * @author Michael Westergaard
 */
public abstract class EventObject extends Group {
	protected static Point[] copy(final int count, final Point... points) {
		final Point[] result = new Point[points.length + count];
		for (int i = 0; i < points.length; i++) {
			result[i] = points[i];
		}
		return result;
	}

	protected static Point createPoint(final int x, final int y, final Point[] points) {
		return EventObject.createPoint(x, y, points, false);
	}

	protected static Point createPoint(final int x, final int y, final Point[] points, final boolean rotate) {
		int dx = x;
		int dy = y;
		if (rotate && (x != 0 || y != 0)) {
			final double d = Math.atan2(points[points.length - 1].getY() - points[points.length - 2].getY(),
			        points[points.length - 1].getX() - points[points.length - 2].getX());
			final double a = Math.atan2(y, x);
			final double r = Math.sqrt(x * x + y * y);
			dx = (int) Math.round(r * Math.cos(d + a));
			dy = (int) Math.round(r * Math.sin(d + a));
		}
		return new Point((int) points[points.length - 1].getX() + dx, (int) points[points.length - 1].getY() + dy);
	}

	private final Text label;

	private final Line line;

	/**
	 * @param e
	 * @param from
	 * @param points
	 * @throws Exception
	 */
	public EventObject(final SimpleEvent e, final ProcessObject from, final Point... points) throws Exception {
		line = new Line(arrowTip(points));
		line.setForeground(e.getForeground());
		add(line);
		setPosition(new Point((int) from.getPosition().getX(), -e.getPosition() * 42 - 42));
		label = new Text(getLabelPosition(points), e.getText());
		label.setForeground(e.getForeground());
		add(label);
	}

	protected abstract Point[] arrowTip(Point... points);

	protected Point getLabelPosition(final Point... points) {
		return getMidpoint(points);
	}

	protected Point getMidpoint(final Point... points) {
		if (points.length == 0) { return new Point(); }
		if (points.length == 1) { return points[0]; }
		return new Point((int) (points[0].getX() + points[1].getX()) / 2,
		        (int) (points[0].getY() + points[1].getY()) / 2);
	}
}
