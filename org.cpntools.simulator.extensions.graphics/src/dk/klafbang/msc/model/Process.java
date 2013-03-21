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
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * @author Michael Westergaard
 */
public class Process {
	List<SimpleEvent> events;

	Color foreground, background;

	String name;

	MSC parent;

	float width;

	/**
	 * @param name
	 */
	public Process(final String name) {
		this(name, Color.BLACK, Color.WHITE, 2.0f);
	}

	/**
	 * @param name
	 * @param foreground
	 * @param background
	 * @param width
	 */
	public Process(final String name, final Color foreground, final Color background, final float width) {
		this.name = name;
		this.foreground = foreground;
		this.background = background;
		this.width = width;
		events = new ArrayList<SimpleEvent>();
	}

	/**
	 * @return Returns the background.
	 */
	public Color getBackground() {
		return background;
	}

	/**
	 * @return the events of the process
	 */
	public List<SimpleEvent> getEvents() {
		return Collections.unmodifiableList(events);
	}

	/**
	 * @return Returns the foreground.
	 */
	public Color getForeground() {
		return foreground;
	}

	/**
	 * @return the name of the process
	 */
	public String getName() {
		return name;
	}

	/**
	 * @return the MSC this process belongs to
	 */
	public MSC getParent() {
		return parent;
	}

	/**
	 * @return Returns the width.
	 */
	public float getWidth() {
		return width;
	}

	void addEvent(final SimpleEvent e) {
		events.add(e);
	}

	void setParent(final MSC parent) {
		this.parent = parent;
	}
}
