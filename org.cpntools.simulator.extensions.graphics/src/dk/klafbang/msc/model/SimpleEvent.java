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
 * Event in an MSC.
 * 
 * @author Michael Westergaard
 */
public class SimpleEvent {
	Color foreground, background;

	Process from, to;

	int position;

	String text;

	float width;

	/**
	 * @param from
	 * @param to
	 * @param text
	 */
	public SimpleEvent(final Process from, final Process to, final String text) {
		this(from, to, text, Color.BLACK, Color.WHITE, 1);
	}

	/**
	 * Constructs an Event object.
	 * 
	 * @param from
	 *            originating process
	 * @param to
	 *            destination process
	 * @param text
	 *            text of the event
	 * @param foreground
	 * @param background
	 * @param width
	 */
	public SimpleEvent(final Process from, final Process to, final String text, final Color foreground,
	        final Color background, final float width) {
		this.from = from;
		this.to = to;
		this.text = text;
		this.foreground = foreground;
		this.background = background;
		this.width = width;
		from.addEvent(this);
		to.addEvent(this);
		if (from.getParent() != null) {
			from.getParent().addEvent(this);
		}
	}

	/**
	 * @return Returns the background.
	 */
	public Color getBackground() {
		return background;
	}

	/**
	 * @return Returns the foreground.
	 */
	public Color getForeground() {
		return foreground;
	}

	/**
	 * Returns the from value.
	 * 
	 * @return originating process
	 */
	public Process getFrom() {
		return from;
	}

	/**
	 * Gets this object's position.
	 * 
	 * @return position of this event
	 */
	public int getPosition() {
		return position;
	}

	/**
	 * Gets this object's text.
	 * 
	 * @return Label of event
	 */
	public String getText() {
		return text;
	}

	/**
	 * Returns the to value.
	 * 
	 * @return destination process
	 */
	public Process getTo() {
		return to;
	}

	/**
	 * @return Returns the width.
	 */
	public float getWidth() {
		return width;
	}

	void setPosition(final int position) {
		this.position = position;
	}
}
