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
package dk.klafbang.msc.model.extension;

import java.awt.Color;
import java.util.HashMap;
import java.util.Map;

import org.cpntools.simulator.extensions.Channel;

import dk.klafbang.msc.MSCPanel;
import dk.klafbang.msc.model.CrossingEvent;
import dk.klafbang.msc.model.InternalEvent;
import dk.klafbang.msc.model.MSC;
import dk.klafbang.msc.model.Process;
import dk.klafbang.msc.model.SimpleEvent;
import dk.klafbang.tools.Pair;

/**
 * This animation objects allows you to draw message-sequence charts easily.
 * 
 * @author Michael Westergaard
 * @since 1.0
 */
public class MSCController {
	int counter = 0;

	Map<String, CrossingEvent> halfEvents;

	String id;

	MSC model;

	Map<String, Process> processes;

	MSCPanel sheet;

	Map<String, Pair<Color, Color>> styles;

	/**
	 * Constructs a MSC object.
	 * 
	 * @param c
	 * @param name
	 *            of the MSC
	 * @throws Exception
	 */
	public MSCController(final Channel c, final String name) throws Exception {
		model = new MSC(name);
		sheet = new MSCPanel(model, c);
		processes = new HashMap<String, Process>();
		halfEvents = new HashMap<String, CrossingEvent>();
		styles = new HashMap<String, Pair<Color, Color>>();
		styles.put("defaultevent", Pair.createPair(Color.BLACK, Color.WHITE)); //$NON-NLS-1$
		styles.put("defaultprocess", Pair.createPair(Color.BLACK, Color.WHITE)); //$NON-NLS-1$
	}

	/**
	 * Add an event from one process to another with the given text.
	 * 
	 * @param from
	 *            the name of the process to create the event from
	 * @param to
	 *            the name of the process to create the event to
	 * @param text
	 *            the text of the event
	 * @throws Exception
	 */
	public void addEvent(final String from, final String to, final String text) throws Exception {
		addEventStyle(from, to, text, null);
	}

	/**
	 * Add an event from one process to another with the given text and style.
	 * 
	 * @param from
	 *            the name of the process to create the event from
	 * @param to
	 *            the name of the process to create the event to
	 * @param text
	 *            the text of the event
	 * @param style
	 *            a style id returned by createStyle
	 * @throws Exception
	 */
	public void addEventStyle(final String from, final String to, final String text, final String style)
	        throws Exception {
		if (from.equals(to)) {
			addInternalEventStyle(from, text, style);
			return;
		}
		if (!(processes.containsKey(from) && processes.containsKey(to))) { throw new Exception(
		        "Source or destination does not exist."); }
		final Process source = processes.get(from);
		final Process destination = processes.get(to);
		final Pair<Color, Color> stylePair = getStyle(style, "defaultevent"); //$NON-NLS-1$
		new SimpleEvent(source, destination, text, stylePair.getFirst(), stylePair.getSecond(), 1.0f);
	}

	/**
	 * Add an internal event to the given process.
	 * 
	 * @param process
	 *            the name of the process to add the event to
	 * @param text
	 *            the text of the internal event
	 * @throws Exception
	 */
	public void addInternalEvent(final String process, final String text) throws Exception {
		addInternalEventStyle(process, text, null);
	}

	/**
	 * Add an internal event to the given process.
	 * 
	 * @param process
	 *            the name of the process to add the event to
	 * @param text
	 *            the text of the internal event
	 * @param style
	 *            a style id returned by createStyle
	 * @throws Exception
	 */
	public void addInternalEventStyle(final String process, final String text, final String style) throws Exception {
		if (!processes.containsKey(process)) { throw new Exception("Process does not exist."); }
		final Process p = processes.get(process);
		final Pair<Color, Color> stylePair = getStyle(style, "defaultevent"); //$NON-NLS-1$
		new InternalEvent(p, text, stylePair.getFirst(), stylePair.getSecond(), 1.0f);
	}

	/**
	 * @param text
	 */
	public void addLine(final String text) {
		model.addLine(text);
	}

	/**
	 * Add a new process to the MSC.
	 * 
	 * @param name
	 *            the name of the new Process
	 */
	public void addProcess(final String name) {
		addProcessStyle(name, null);
	}

	/**
	 * Add a new process to the MSC.
	 * 
	 * @param name
	 *            the name of the new Process
	 * @param style
	 *            a style id returned by createStyle
	 */
	public void addProcessStyle(final String name, final String style) {
		if (processes.containsKey(name)) { return; }
		final Pair<Color, Color> stylePair = getStyle(style, "defaultprocess"); //$NON-NLS-1$
		final Process p = new Process(name, stylePair.getFirst(), stylePair.getSecond(), 1.0f);
		model.addProcess(p);
		processes.put(name, p);
	}

	/**
	 * Create a new style.
	 * 
	 * @param foreground
	 *            the foreground color as a string "0xRRGGBB" where RR, GG, BB are hexadecimal encodings of the color's
	 *            red, green, and blue components. E.g. 0xff0000 would represent a bright red, 0xffff00 a bright yellow,
	 *            and 0x7f7fff a light blue.
	 * @param background
	 *            the background color in the same format as the foreground color.
	 * @return an id representing this style
	 * @throws NumberFormatException
	 */
	public String createStyle(final String foreground, final String background) throws NumberFormatException {
		final Color fgcolor = Color.decode(foreground);
		final Color bgcolor = Color.decode(background);
		final int count = counter++;
		final String identifier = "style" + count; //$NON-NLS-1$
		styles.put(identifier, Pair.createPair(fgcolor, bgcolor));
		return identifier;
	}

	/**
	 * Drop a dangling event.
	 * 
	 * @param identifier
	 *            the id returned by startEvent
	 * @throws Exception
	 */
	public void dropEvent(final String identifier) throws Exception {
		checkDanglingEvent(identifier);
		halfEvents.remove(identifier).drop();
	}

	/**
	 * Attach a dangling event to an end process.
	 * 
	 * @param identifier
	 *            the id returned by startEvent
	 * @param to
	 *            name of the process to end the event on
	 * @throws Exception
	 */
	public void endEvent(final String identifier, final String to) throws Exception {
		checkDanglingEvent(identifier);
		halfEvents.remove(identifier).setTo(processes.get(to));
	}

	/**
	 * Attach a dangling event to an end process and updates its style.
	 * 
	 * @param identifier
	 *            the id returned by startEvent
	 * @param to
	 *            name of the process to end the event on
	 * @param style
	 *            a style id returned by createStyle
	 * @throws Exception
	 */
	public void endEventStyle(final String identifier, final String to, final String style) throws Exception {
		checkDanglingEvent(identifier);
		final Pair<Color, Color> stylePair = getStyle(style, "defaultevent"); //$NON-NLS-1$
		halfEvents.remove(identifier).setTo(processes.get(to), stylePair.getFirst(), stylePair.getSecond(), 1.0f);
	}

	/**
	 * Add an event starting at a process, but which does not end anywhere with the given text.
	 * 
	 * @param from
	 *            name of the process to create the event from
	 * @param text
	 *            the text of the event
	 * @return an id for this event, which can be used to terminate it later
	 * @throws Exception
	 */
	public String startEvent(final String from, final String text) throws Exception {
		return startEventStyle(from, text, null);
	}

	/**
	 * Add an event starting at a process, but which does not end anywhere with the given text and style.
	 * 
	 * @param from
	 *            name of the process to create the event from
	 * @param text
	 *            the text of the event
	 * @param style
	 *            a style id returned by createStyle
	 * @return an id for this event, which can be used to terminate it later
	 * @throws Exception
	 */
	public String startEventStyle(final String from, final String text, final String style) throws Exception {
		if (!processes.containsKey(from)) { throw new Exception("Source or destination does not exist."); }
		final int count = counter++;
		final String identifier = "event" + count; //$NON-NLS-1$
		final Pair<Color, Color> stylePair = getStyle(style, "defaultevent"); //$NON-NLS-1$
		halfEvents.put(identifier,
		        new CrossingEvent(processes.get(from), text, stylePair.getFirst(), stylePair.getSecond(), 1.0f));
		return identifier;
	}

	/**
	 * @param identifier
	 * @throws Exception
	 */
	private void checkDanglingEvent(final String identifier) throws Exception {
		if (!halfEvents.containsKey(identifier)) { throw new Exception("Event does not exist."); }
	}

	private Pair<Color, Color> getStyle(final String style, final String fallback) {
		Pair<Color, Color> stylePair = styles.get(style);
		if (stylePair == null) {
			stylePair = styles.get(fallback);
		}
		return stylePair;
	}
}