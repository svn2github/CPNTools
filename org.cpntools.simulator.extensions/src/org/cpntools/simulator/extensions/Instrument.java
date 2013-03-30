package org.cpntools.simulator.extensions;

/**
 * @author michael
 */
public class Instrument {
	private final String name, toolTip, toolBox;
	private final int targets;
	private final String key;

	/**
	 * @author michael
	 */
	public static final class ToolBoxes {
		@SuppressWarnings("javadoc")
		// Eclipse cannot detect JavaDoc for these
		public static final String AUXILIARY = "aux", CREATE = "create", DECLARE = "declare", HIERARCHY = "hierarchy",
		        MONITORING = "monitoring", NET = "net", SIMULATION = "simulation", STATE_SPACE = "statespace",
		        STYLE = "style", VIEW = "view", EXTENSIONS = "Extensions";
	}

	/**
	 * @param name
	 */
	public Instrument(final String name) {
		this(ToolBoxes.EXTENSIONS, name);
	}

	/**
	 * @param toolBox
	 * @param name
	 */
	public Instrument(final String toolBox, final String name) {
		this(toolBox, name, Target.NET);
	}

	/**
	 * @param name
	 * @param targets
	 */
	public Instrument(final String name, final Target... targets) {
		this(ToolBoxes.EXTENSIONS, name, targets);
	}

	/**
	 * @param toolBox
	 * @param name
	 * @param targets
	 */
	public Instrument(final String toolBox, final String name, final Target... targets) {
		this(toolBox, name.toLowerCase().replaceAll("[ \t\n\r]*", "_"), name, targets);
	}

	/**
	 * @param toolBox
	 * @param key
	 * @param name
	 */
	public Instrument(final String toolBox, final String key, final String name) {
		this(toolBox, key, name, Target.NET);
	}

	/**
	 * @param toolBox
	 * @param key
	 * @param name
	 * @param targets
	 */
	public Instrument(final String toolBox, final String key, final String name, final Target... targets) {
		this(toolBox, key, name, name, targets);
	}

	/**
	 * @param toolBox
	 * @param key
	 * @param name
	 * @param toolTip
	 */
	public Instrument(final String toolBox, final String key, final String name, final String toolTip) {
		this(toolBox, key, name, toolTip, Target.NET);
	}

	/**
	 * @param name
	 * @param toolTip
	 * @param toolBox
	 * @param key
	 * @param targets
	 */
	public Instrument(final String toolBox, final String key, final String name, final String toolTip,
	        final Target... targets) {
		this.key = key;
		this.name = name;
		this.toolTip = toolTip;
		this.toolBox = toolBox;
		int t = 0;
		for (final Target target : targets) {
			t = target.getValue() | t;
		}
		this.targets = t;
	}

	/**
	 * @author michael
	 */
	public static enum Target {
		/** */
		NET(0x1), /** */
		PAGE(0x2), /** */
		PLACE(0x4), /** */
		TRANSITION(0x8), /** */
		ARC(0x10), /** */
		AUX_BOX(0x10000), /** */
		AUX_ELLIPSE(0x20000), /** */
		AUX_TEXT(0x40000);

		private final int value;

		private Target(final int value) {
			this.value = value;
		}

		/**
		 * @return
		 */
		public int getValue() {
			return value;
		}
	}

	/**
	 * @return
	 */
	public String getName() {
		return name;
	}

	/**
	 * @return
	 */
	public String getToolTip() {
		return toolTip;
	}

	/**
	 * @return
	 */
	public String getToolBox() {
		return toolBox;
	}

	/**
	 * @return
	 */
	public int getTargets() {
		return targets;
	}

	/**
	 * @return
	 */
	public String getKey() {
		return key;
	}
}
