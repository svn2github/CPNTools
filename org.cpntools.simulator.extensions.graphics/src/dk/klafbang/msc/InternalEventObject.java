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
import org.cpntools.simulator.extensions.graphics.Rectangle;
import org.cpntools.simulator.extensions.graphics.Text;

import dk.klafbang.msc.model.InternalEvent;

/**
 * @author Michael Westergaard
 */
public class InternalEventObject extends Group {
	/**
	 * @param e
	 * @throws Exception
	 */
	public InternalEventObject(final InternalEvent e) throws Exception {
		setPosition(new Point(0, -e.getPosition() * 42 - 42));

		final Rectangle marker = new Rectangle(new java.awt.Rectangle(-3, -3, 6, 6));
		marker.setForeground(e.getForeground());
		marker.setBackground(e.getForeground());
		add(marker);

		final Text text = new Text(10, -3, e.getText());
		text.setForeground(e.getForeground());
		add(text);
	}
}
