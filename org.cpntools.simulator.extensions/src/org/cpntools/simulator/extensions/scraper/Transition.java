package org.cpntools.simulator.extensions.scraper;

/**
 * @author michael
 */
public class Transition extends Node {

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

}
