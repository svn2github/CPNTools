package dk.klafbang.msc.model.extension;

import java.util.HashMap;
import java.util.Map;

import org.cpntools.simulator.extensions.AbstractExtension;
import org.cpntools.simulator.extensions.Channel;
import org.cpntools.simulator.extensions.NamedRPCHandler;

/**
 * @author michael
 */
public class MSCExtension extends AbstractExtension {
	final Map<String, MSCController> mscs = new HashMap<String, MSCController>();
	int serial = 0;

	/**
	 * @author michael
	 */
	public final class MSCDispatcher implements NamedRPCHandler {
		@SuppressWarnings("hiding")
		private final Channel channel;

		/**
		 * @param channel
		 */
		public MSCDispatcher(final Channel channel) {
			this.channel = channel;
		}

		/**
		 * @param name
		 *            of the MSC
		 * @return
		 * @throws Exception
		 */
		public synchronized String createMSC(final String name) throws Exception {
			final String id = "msc" + serial++;
			final MSCController c = new MSCController(channel, name);
			mscs.put(id, c);
			return id;
		}

		private MSCController c(final String id) throws Exception {
			final MSCController c = mscs.get(id);
			if (c == null) { throw new Exception("Unknown MSC"); }
			return c;
		}

		/**
		 * Add an event from one process to another with the given text.
		 * 
		 * @param id
		 * @param from
		 *            the name of the process to create the event from
		 * @param to
		 *            the name of the process to create the event to
		 * @param text
		 *            the text of the event
		 * @throws Exception
		 */
		public void addEvent(final String id, final String from, final String to, final String text) throws Exception {
			c(id).addEvent(from, to, text);
		}

		/**
		 * Add an event from one process to another with the given text and style.
		 * 
		 * @param id
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
		public void addEventStyle(final String id, final String from, final String to, final String text,
		        final String style) throws Exception {
			c(id).addEventStyle(from, to, text, style);
		}

		/**
		 * Add an internal event to the given process.
		 * 
		 * @param id
		 * @param process
		 *            the name of the process to add the event to
		 * @param text
		 *            the text of the internal event
		 * @throws Exception
		 */
		public void addInternalEvent(final String id, final String process, final String text) throws Exception {
			c(id).addInternalEvent(process, text);
		}

		/**
		 * Add an internal event to the given process.
		 * 
		 * @param id
		 * @param process
		 *            the name of the process to add the event to
		 * @param text
		 *            the text of the internal event
		 * @param style
		 *            a style id returned by createStyle
		 * @throws Exception
		 */
		public void addInternalEventStyle(final String id, final String process, final String text, final String style)
		        throws Exception {
			c(id).addInternalEventStyle(process, text, style);
		}

		/**
		 * @param id
		 * @param text
		 * @throws Exception
		 */
		public void addLine(final String id, final String text) throws Exception {
			c(id).addLine(text);
		}

		/**
		 * Add a new process to the MSC.
		 * 
		 * @param id
		 * @param name
		 *            the name of the new Process
		 * @throws Exception
		 */
		public void addProcess(final String id, final String name) throws Exception {
			c(id).addProcess(name);
		}

		/**
		 * Add a new process to the MSC.
		 * 
		 * @param id
		 * @param name
		 *            the name of the new Process
		 * @param style
		 *            a style id returned by createStyle
		 * @throws Exception
		 */
		public void addProcessStyle(final String id, final String name, final String style) throws Exception {
			c(id).addProcessStyle(name, style);
		}

		/**
		 * Create a new style.
		 * 
		 * @param id
		 * @param foreground
		 *            the foreground color as a string "0xRRGGBB" where RR, GG, BB are hexadecimal encodings of the
		 *            color's red, green, and blue components. E.g. 0xff0000 would represent a bright red, 0xffff00 a
		 *            bright yellow, and 0x7f7fff a light blue.
		 * @param background
		 *            the background color in the same format as the foreground color.
		 * @return an id representing this style
		 * @throws Exception
		 */
		public String createStyle(final String id, final String foreground, final String background) throws Exception {
			return c(id).createStyle(foreground, background);
		}

		/**
		 * Drop a dangling event.
		 * 
		 * @param id
		 * @param identifier
		 *            the id returned by startEvent
		 * @throws Exception
		 */
		public void dropEvent(final String id, final String identifier) throws Exception {
			c(id).dropEvent(identifier);
		}

		/**
		 * Attach a dangling event to an end process.
		 * 
		 * @param id
		 * @param identifier
		 *            the id returned by startEvent
		 * @param to
		 *            name of the process to end the event on
		 * @throws Exception
		 */
		public void endEvent(final String id, final String identifier, final String to) throws Exception {
			c(id).endEvent(identifier, to);
		}

		/**
		 * Attach a dangling event to an end process and updates its style.
		 * 
		 * @param id
		 * @param identifier
		 *            the id returned by startEvent
		 * @param to
		 *            name of the process to end the event on
		 * @param style
		 *            a style id returned by createStyle
		 * @throws Exception
		 */
		public void endEventStyle(final String id, final String identifier, final String to, final String style)
		        throws Exception {
			c(id).endEventStyle(identifier, to, style);
		}

		/**
		 * Add an event starting at a process, but which does not end anywhere with the given text.
		 * 
		 * @param id
		 * @param from
		 *            name of the process to create the event from
		 * @param text
		 *            the text of the event
		 * @return an id for this event, which can be used to terminate it later
		 * @throws Exception
		 */
		public String startEvent(final String id, final String from, final String text) throws Exception {
			return c(id).startEvent(from, text);
		}

		/**
		 * Add an event starting at a process, but which does not end anywhere with the given text and style.
		 * 
		 * @param id
		 * @param from
		 *            name of the process to create the event from
		 * @param text
		 *            the text of the event
		 * @param style
		 *            a style id returned by createStyle
		 * @return an id for this event, which can be used to terminate it later
		 * @throws Exception
		 */
		public String startEventStyle(final String id, final String from, final String text, final String style)
		        throws Exception {
			return c(id).startEventStyle(from, text, style);
		}

		/**
		 * @see org.cpntools.simulator.extensions.NamedRPCHandler#structureName()
		 */
		@Override
		public String structureName() {
			return "MSC";
		}
	}

	/**
	 * 
	 */
	public static final int ID = 10006;

	/**
	 * 
	 */
	public MSCExtension() {
	}

	/**
	 * @see org.cpntools.simulator.extensions.Extension#getIdentifier()
	 */
	@Override
	public int getIdentifier() {
		return MSCExtension.ID;
	}

	/**
	 * @see org.cpntools.simulator.extensions.AbstractExtension#getRPCHandler()
	 */
	@Override
	public Object getRPCHandler() {
		return new MSCDispatcher(channel);
	}

	/**
	 * @see org.cpntools.simulator.extensions.Extension#getName()
	 */
	@Override
	public String getName() {
		return "MSC";
	}

}
