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
public class MSCEvent {
	private final SimpleEvent event;

	private final int position;

	private final Process process;

	private final MSC source;

	/**
	 * @param source
	 * @param process
	 * @param position
	 */
	public MSCEvent(final MSC source, final Process process, final int position) {
		this.source = source;
		this.process = process;
		this.position = position;
		event = null;
	}

	/**
	 * @param source
	 * @param event
	 */
	public MSCEvent(final MSC source, final SimpleEvent event) {
		this.source = source;
		this.event = event;
		process = null;
		position = 0;
	}

	/**
	 * @return the event associated with the event
	 */
	public SimpleEvent getEvent() {
		return event;
	}

	/**
	 * @return the position of the event that generated the event
	 */
	public int getPosition() {
		return position;
	}

	/**
	 * @return the process associated with the event
	 */
	public Process getProcess() {
		return process;
	}

	/**
	 * @return the source of the event
	 */
	public MSC getSource() {
		return source;
	}
}
