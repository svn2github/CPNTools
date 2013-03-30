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

import dk.klafbang.msc.model.CrossingEvent;
import dk.klafbang.msc.model.SimpleEvent;

/**
 * Graphical representation of an {@link SimpleEvent}.
 * 
 * @author Michael Westergaard
 */
public class CrossingEventObject extends EventObject {
	private static Point getEndPoint(final CrossingEvent e, final ProcessObject from, final ProcessObject to) {
		if (to == null) { return new Point(84, -21); }
		final int dy = e.getEndPosition() - e.getPosition();
		final int dx = (int) (to.getPosition().getX() - from.getPosition().getX());
		return new Point(dx, -42 * dy);
	}

	/**
	 * Constructs an EventObject object.
	 * 
	 * @param e
	 *            the Event this is a view of
	 * @param from
	 *            the view of the originating process
	 * @param to
	 * @throws Exception
	 */
	public CrossingEventObject(final CrossingEvent e, final ProcessObject from, final ProcessObject to)
	        throws Exception {
		super(e, from, new Point(0, 0), CrossingEventObject.getEndPoint(e, from, to));
		line.setLineStyle(2);
	}
}
