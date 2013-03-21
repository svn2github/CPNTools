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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * @author Michael Westergaard
 */
public class MSC {
	private final String name;

	List<SimpleEvent> events;

	List<MSCListener> listeners;

	List<Process> processes;

	/**
	 * @param name
	 */
	public MSC(final String name) {
		this.name = name;
		processes = new ArrayList<Process>();
		events = new ArrayList<SimpleEvent>();
		listeners = new ArrayList<MSCListener>();
	}

	/**
	 * @param text
	 */
	public void addLine(final String text) {
		addEvent(new Line(this, text));
	}

	/**
	 * @param listener
	 */
	public void addMSCListener(final MSCListener listener) {
		listeners.add(listener);
	}

	/**
	 * @param p
	 */
	public void addProcess(final Process p) {
		processes.add(p);
		p.setParent(this);
		final MSCEvent ev = new MSCEvent(this, p, processes.size() - 1);
		for (final MSCListener l : listeners) {
			l.processAdded(ev);
		}
	}

	/**
	 * @return all events of this MSC
	 */
	public List<SimpleEvent> getEvents() {
		return Collections.unmodifiableList(events);
	}

	/**
	 * @return Returns the name.
	 */
	public String getName() {
		return name;
	}

	/**
	 * @return all process of this MSC
	 */
	public List<Process> getProcesses() {
		return Collections.unmodifiableList(processes);
	}

	void addEvent(final SimpleEvent e) {
		e.from.setParent(this);
		events.add(e);
		e.setPosition(events.size() - 1);
		final MSCEvent ev = new MSCEvent(this, e);
		for (final MSCListener l : listeners) {
			l.eventAdded(ev);
		}
	}
}
