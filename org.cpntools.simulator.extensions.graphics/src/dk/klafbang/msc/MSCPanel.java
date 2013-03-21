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
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.cpntools.simulator.extensions.Channel;
import org.cpntools.simulator.extensions.graphics.Canvas;

import dk.klafbang.msc.model.CrossingEvent;
import dk.klafbang.msc.model.InternalEvent;
import dk.klafbang.msc.model.Line;
import dk.klafbang.msc.model.MSC;
import dk.klafbang.msc.model.MSCEvent;
import dk.klafbang.msc.model.MSCListener;
import dk.klafbang.msc.model.Process;
import dk.klafbang.msc.model.SimpleEvent;

/**
 * @author Michael Westergaard
 */
public class MSCPanel implements MSCListener {
	private final Canvas canvas;

	private final Map<CrossingEvent, EventObject> crossingEvents;

// private ProcessObject lastProcess;

	private final List<LineObject> lines;

// private final MSC model;

	private final Map<Process, ProcessObject> views;

	/**
	 * @param model
	 * @param channel
	 * @throws Exception
	 */
	public MSCPanel(final MSC model, final Channel channel) throws Exception {
// this.model = model;

		canvas = new Canvas(channel, model.getName());
		canvas.setPosition(new Point(84, 420));

		views = new HashMap<Process, ProcessObject>();
		crossingEvents = new HashMap<CrossingEvent, EventObject>();
		lines = new ArrayList<LineObject>();

		int i = 0;
		for (final Process p : model.getProcesses()) {
			addProcess(p, i);
			i++;
		}
		for (final SimpleEvent e : model.getEvents()) {
			addEvent(e);
		}
		model.addMSCListener(this);
	}

	/**
	 * @see dk.klafbang.msc.model.MSCListener#eventAdded(dk.klafbang.msc.model.MSCEvent)
	 */
	@Override
	public synchronized void eventAdded(final MSCEvent e) {
		boolean suspend = false;
		try {
			suspend = canvas.suspend(true);
			addEvent(e.getEvent());
			for (final ProcessObject po : views.values()) {
				po.updateBottom();
			}
		} catch (final Exception e1) { // Ignore
		} finally {
			try {
				canvas.suspend(suspend);
			} catch (final Exception e1) { // Ignore
			}
		}
	}

	/**
	 * @see dk.klafbang.msc.model.MSCListener#processAdded(dk.klafbang.msc.model.MSCEvent)
	 */
	@Override
	public synchronized void processAdded(final MSCEvent e) {
		addProcess(e.getProcess(), e.getPosition());
	}

	synchronized void addEvent(final SimpleEvent e) {
		boolean suspend = false;
		try {
			suspend = canvas.suspend(true);
			if (e instanceof InternalEvent) {
				views.get(e.getFrom()).add(new InternalEventObject((InternalEvent) e));
			} else if (e instanceof CrossingEvent) {
				final CrossingEvent evt = (CrossingEvent) e;
				final EventObject newEO;
				if (evt.isDropped()) {
					newEO = new DroppingEventObject(evt, views.get(e.getFrom()));
				} else {
					newEO = new CrossingEventObject(evt, views.get(e.getFrom()), views.get(e.getTo()));
				}
				final EventObject oldEO = crossingEvents.put(evt, newEO);
				if (oldEO != null) {
					canvas.remove(oldEO);
				}
				canvas.add(newEO);
			} else if (e instanceof Line) {
				final LineObject lo = new LineObject((Line) e);
				lines.add(lo);
				canvas.add(lo);
			} else {
				canvas.add(new SimpleEventObject(e, views.get(e.getFrom()), views.get(e.getTo())));
			}
		} catch (final Exception ex) {
			// Ingore
		} finally {
			try {
				canvas.suspend(suspend);
			} catch (final Exception e1) { // Ignore
			}
		}
	}

	synchronized void addProcess(final Process p, final int position) {
		boolean suspend = false;
		try {
			suspend = canvas.suspend(true);
			final ProcessObject view = /* lastProcess = */new ProcessObject(p);
			view.setPosition(new Point(42 * 3 * position, 0));
			canvas.add(view);
			views.put(p, view);
			for (final LineObject lo : lines) {
				lo.updateRight();
			}
		} catch (final Exception ex) {
			// Ignore
		} finally {
			try {
				canvas.suspend(suspend);
			} catch (final Exception e) { // Ignore
			}
		}
	}
}
