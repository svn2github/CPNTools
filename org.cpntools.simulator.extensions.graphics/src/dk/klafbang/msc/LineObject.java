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

import dk.klafbang.msc.model.Line;

/**
 * @author Michael Westergaard
 */
public class LineObject extends Group {
	private final Line model;
	private final Rectangle rectangle;

	/**
	 * @param model
	 * @throws Exception
	 */
	public LineObject(final Line model) throws Exception {
		this.model = model;

		rectangle = new Rectangle(new java.awt.Rectangle(-42, -10,
		        model.getParent().getProcesses().size() * 3 * 42 - 42, 21));
		rectangle.setForeground(model.getForeground());
		rectangle.setBackground(model.getBackground());
		add(rectangle);
		setPosition(new Point(0, -42 - model.getPosition() * 42));
		updateRight();

		final Text text = new Text(-30, -6, model.getText());
		text.setForeground(model.getForeground());
		add(text);
	}

	/**
	 * @throws Exception
	 */
	public void updateRight() throws Exception {
		rectangle.setSize(model.getParent().getProcesses().size() * 3 * 42 - 42, 21);
	}
}
