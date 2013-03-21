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

import java.awt.Color;

/**
 * @author Michael Westergaard
 */
public class InternalEvent extends SimpleEvent {
	/**
	 * @param p
	 * @param text
	 */
	public InternalEvent(final Process p, final String text) {
		super(p, new Process("tmp"), text); //$NON-NLS-1$
	}

	/**
	 * @param p
	 * @param text
	 * @param foreground
	 * @param background
	 * @param width
	 */
	public InternalEvent(final Process p, final String text, final Color foreground, final Color background,
	        final float width) {
		super(p, new Process("tmp"), text, foreground, background, width); //$NON-NLS-1$
	}

	/**
	 * @see dk.klafbang.msc.model.SimpleEvent#getTo()
	 */
	@Override
	public Process getTo() {
		return null;
	}
}
