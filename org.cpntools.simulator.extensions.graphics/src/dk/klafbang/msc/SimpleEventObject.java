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

import dk.klafbang.msc.model.SimpleEvent;

/**
 * Graphical representation of an {@link SimpleEvent}.
 * 
 * @author Michael Westergaard
 */
public class SimpleEventObject extends EventObject {
	/**
	 * Constructs an EventObject object.
	 * 
	 * @param e
	 *            the Event this is a view of
	 * @param from
	 *            the view of the originating process
	 * @param to
	 *            the view of the destination process
	 * @throws Exception
	 */
	public SimpleEventObject(final SimpleEvent e, final ProcessObject from, final ProcessObject to) throws Exception {
		super(e, from, new Point(0, 0), new Point((int) (to.getPosition().getX() - from.getPosition().getX()), 0));
	}

	@Override
	protected Point[] arrowTip(final Point... points) {
		final Point[] result = EventObject.copy(4, points);
		result[points.length + 0] = EventObject.createPoint(-12, 5, points, true);
		result[points.length + 1] = EventObject.createPoint(-8, 0, points, true);
		result[points.length + 2] = EventObject.createPoint(-12, -5, points, true);
		result[points.length + 3] = EventObject.createPoint(0, 0, points);
		return result;
	}
}
