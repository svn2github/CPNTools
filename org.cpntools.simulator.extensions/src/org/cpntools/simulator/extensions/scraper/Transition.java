package org.cpntools.simulator.extensions.scraper;

/**
 * @author michael
 */
public class Transition extends Node {

	/**
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + (channel == null ? 0 : channel.hashCode());
		result = prime * result + (code == null ? 0 : code.hashCode());
		result = prime * result + (controllable ? 1231 : 1237);
		result = prime * result + (guard == null ? 0 : guard.hashCode());
		result = prime * result + (priority == null ? 0 : priority.hashCode());
		result = prime * result + (time == null ? 0 : time.hashCode());
		return result;
	}

	/**
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(final Object obj) {
		if (this == obj) { return true; }
		if (!super.equals(obj)) { return false; }
		if (!(obj instanceof Transition)) { return false; }
		final Transition other = (Transition) obj;
		if (channel == null) {
			if (other.channel != null) { return false; }
		} else if (!channel.equals(other.channel)) { return false; }
		if (code == null) {
			if (other.code != null) { return false; }
		} else if (!code.equals(other.code)) { return false; }
		if (controllable != other.controllable) { return false; }
		if (guard == null) {
			if (other.guard != null) { return false; }
		} else if (!guard.equals(other.guard)) { return false; }
		if (priority == null) {
			if (other.priority != null) { return false; }
		} else if (!priority.equals(other.priority)) { return false; }
		if (time == null) {
			if (other.time != null) { return false; }
		} else if (!time.equals(other.time)) { return false; }
		return true;
	}

	private String guard;
	private String priority;
	private String time;
	private String code;
	private String channel;
	private boolean controllable;

	public Transition(final String id, final String name, final Page page, final String guard, final String priority,
	        final String time, final String code, final String channel, final boolean controllable) {
		super(id, name, page);
		setChannel(channel);
		setControllable(controllable);
		setGuard(guard);
		setPriority(priority);
		setTime(time);
		setCode(code);
	}

	public String getGuard() {
		return guard;
	}

	public boolean setGuard(final String guard) {
		if (guard.equals(this.guard)) { return false; }
		this.guard = guard;
		return true;
	}

	public String getPriority() {
		return priority;
	}

	public boolean setPriority(final String priority) {
		if (priority.equals(this.priority)) { return false; }
		this.priority = priority;
		return true;
	}

	public String getTime() {
		return time;
	}

	public boolean setTime(final String time) {
		if (time.equals(this.time)) { return false; }
		this.time = time;
		return true;
	}

	public String getCode() {
		return code;
	}

	public boolean setCode(final String code) {
		if (code.equals(this.code)) { return false; }
		this.code = code;
		return true;
	}

	public String getChannel() {
		return channel;
	}

	public boolean setChannel(final String channel) {
		if (channel.equals(this.channel)) { return false; }
		this.channel = channel;
		return true;
	}

	public boolean isControllable() {
		return controllable;
	}

	public boolean setControllable(final boolean controllable) {
		if (controllable == this.controllable) { return false; }
		this.controllable = controllable;
		return true;
	}

	/**
	 * @see java.lang.Object#clone()
	 */
	@Override
	public Transition clone() {
		return (Transition) super.clone();
	}
}
