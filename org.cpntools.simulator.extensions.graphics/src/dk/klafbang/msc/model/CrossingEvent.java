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
public class CrossingEvent extends SimpleEvent {
	boolean dropped = false;

	int endPosition = 0;

	/**
	 * @param p
	 * @param text
	 */
	public CrossingEvent(final Process p, final String text) {
		super(p, new Process("tmp"), text); //$NON-NLS-1$
		endPosition = -1;
	}

	/**
	 * @param p
	 * @param text
	 * @param foreground
	 * @param background
	 * @param width
	 */
	public CrossingEvent(final Process p, final String text, final Color foreground, final Color background,
	        final float width) {
		super(p, new Process("tmp"), text, foreground, background, width); //$NON-NLS-1$
		endPosition = -1;
	}

	/**
	 * 
	 */
	public void drop() {
		dropped = true;
		final MSCEvent ev = new MSCEvent(from.getParent(), this);
		for (final MSCListener l : from.getParent().listeners) {
			l.eventAdded(ev);
		}
	}

	/**
	 * @return the position where this event ends
	 */
	public int getEndPosition() {
		return endPosition;
	}

	/**
	 * @return whether this event is dropped
	 */
	public boolean isDropped() {
		return dropped;
	}

	/**
	 * @see dk.klafbang.msc.model.SimpleEvent#setPosition(int)
	 */
	@Override
	public void setPosition(final int position) {
		if (endPosition != -1) {
			super.setPosition(position);
		} else {
			endPosition = position;
		}
	}

	/**
	 * @param to
	 */
	public void setTo(final Process to) {
		this.to = to;
		to.addEvent(this);
		if (to.getParent() != null) {
			to.getParent().addEvent(this);
		}
	}

	/**
	 * @param to
	 * @param foreground
	 * @param background
	 * @param width
	 */
	public void setTo(final Process to, final Color foreground, final Color background, final float width) {
		this.foreground = foreground;
		this.background = background;
		this.width = width;
		setTo(to);
	}
}
