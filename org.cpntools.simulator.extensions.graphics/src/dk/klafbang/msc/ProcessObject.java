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
import org.cpntools.simulator.extensions.graphics.Rectangle;
import org.cpntools.simulator.extensions.graphics.Text;

import dk.klafbang.msc.model.Process;

/**
 * @author Michael Westergaard
 */
public class ProcessObject extends Group {
	private final Rectangle bottom;
	private Line line;
	Process model;

	/**
	 * @param model
	 * @throws Exception
	 */
	public ProcessObject(final Process model) throws Exception {
		this.model = model;

		final Rectangle header = new Rectangle(new java.awt.Rectangle(-50, -21, 100, 42));
		header.setForeground(model.getForeground());
		header.setBackground(model.getBackground());
		add(header);
		final Text name = new Text(new Point(-42, -6), model.getName());
		name.setForeground(model.getForeground());
		add(name);

		bottom = new Rectangle(new java.awt.Rectangle(-10, -5, 21, 11));
		bottom.setForeground(model.getForeground());
		bottom.setBackground(model.getBackground());
		line = new Line(new Point(0, -21), new Point(0, -36));
		line.setForeground(model.getForeground());
		add(bottom);
		add(line);
		updateBottom();
	}

	/**
	 * @throws Exception
	 */
	public void updateBottom() throws Exception {
		final int y = model.getParent().getEvents().size() * 42 + 42;
		bottom.setPosition(new Point(-10, -5 - y));
		remove(line);
		line = new Line(new Point(0, -21), new Point(0, 6 - y));
		add(line);
	}
}
