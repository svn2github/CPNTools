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
package dk.klafbang.msc.model;

/**
 * @author Michael Westergaard
 */
public class Line extends SimpleEvent {
	private final MSC parent;

	/**
	 * @param parent
	 * @param text
	 */
	public Line(final MSC parent, final String text) {
		super(new Process("tmp"), new Process("tmp"), text); //$NON-NLS-1$ //$NON-NLS-2$
		this.parent = parent;
	}

	/**
	 * @see dk.klafbang.msc.model.SimpleEvent#getFrom()
	 */
	@Override
	public Process getFrom() {
		return null;
	}

	/**
	 * @return
	 */
	public MSC getParent() {
		return parent;
	}

	/**
	 * @see dk.klafbang.msc.model.SimpleEvent#getTo()
	 */
	@Override
	public Process getTo() {
		return null;
	}
}
