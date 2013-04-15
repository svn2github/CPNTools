package org.cpntools.simulator.extensions.scraper;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * @author michael
 */
public class Transition extends Node {

	private String channel;

	private String code;

	private boolean controllable;
	private String guard;
	private final Map<String, Arc> oldArcs = new HashMap<String, Arc>();
	private final Map<String, String> oldPortSock = new HashMap<String, String>();
	private final Map<String, String> portSocket = new HashMap<String, String>();
	private String priority;

	private String subpage;

	private boolean substitution;

	private String time;
	final Set<Place> changedPlaces = new HashSet<Place>();

	final Set<String> changedSockets = new HashSet<String>();

	/**
	 * @param dictionary
	 * @param id
	 * @param name
	 * @param page
	 * @param guard
	 * @param priority
	 * @param time
	 * @param code
	 * @param channel
	 * @param subpage
	 * @param controllable
	 * @param substitution
	 */
	public Transition(final ElementDictionary dictionary, final String id, final String name, final Page page,
	        final String guard, final String priority, final String time, final String code, final String channel,
	        final String subpage, final boolean controllable, final boolean substitution) {
		super(dictionary, id, name, page);
		this.subpage = subpage;
		setChannel(channel);
		setControllable(controllable);
		setGuard(guard);
		setPriority(priority);
		setTime(time);
		setCode(code);
		this.substitution = substitution;
	}

	/**
	 * @see org.cpntools.simulator.extensions.scraper.Node#addArc(org.cpntools.simulator.extensions.scraper.Arc)
	 */
	@Override
	public boolean addArc(final Arc a) {
		final Arc oldArc = oldArcs.remove(a.getId());
		if (oldArc == null || !oldArc.equals(a)) {
			if (oldArc != null) {
				final Place place = oldArc.getPlace();
				removeArc(oldArc);
				place.removeArc(oldArc);
				changedPlaces.add(place);
			}
			super.addArc(a);
			changedPlaces.add(a.getPlace());
			return true;
		}
		return false;
	}

	/**
	 * @param port
	 * @param socket
	 * @return
	 */
	public boolean addPortSocket(final String port, final String socket) {
		final String oldPort = oldPortSock.remove(socket);
		if (oldPort == null || !oldPort.equals(port)) {
			portSocket.put(socket, port);
			changedSockets.add(socket);
			return true;
		}
		return false;
	}

	/**
	 * @see java.lang.Object#clone()
	 */
	@Override
	public Transition clone() {
		return (Transition) super.clone();
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

	/**
	 * @return
	 */
	public Set<Place> finishNewArcs() {
		for (final Arc a : oldArcs.values()) {
			removeArc(a);
			a.getPlace().removeArc(a);
			changedPlaces.add(a.getPlace());
		}
		oldArcs.clear();
		return changedPlaces;
	}

	/**
	 * @return
	 */
	public Set<String> finishNewPortSocket() {
		for (final String socket : oldPortSock.keySet()) {
			portSocket.remove(socket);
			changedSockets.add(socket);
		}
		oldPortSock.clear();
		return changedSockets;
	}

	/**
	 * @return
	 */
	public String getChannel() {
		return channel;
	}

	/**
	 * @return
	 */
	public String getCode() {
		return code;
	}

	/**
	 * @return
	 */
	public String getGuard() {
		return guard;
	}

	/**
	 * @return
	 */
	public String getPriority() {
		return priority;
	}

	/**
	 * @return
	 */
	public String getSubpage() {
		return subpage;
	}

	/**
	 * @return
	 */
	public String getTime() {
		return time;
	}

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
	 * @return
	 */
	public boolean isControllable() {
		return controllable;
	}

	/**
	 * @return
	 */
	public boolean isSubstitution() {
		return substitution;
	}

	/**
	 * 
	 */
	public void prepareNewArcs() {
		oldArcs.clear();
		changedPlaces.clear();
		oldArcs.putAll(inArcs);
		oldArcs.putAll(outArcs); // Not test as they are in both in and out! sneaky!
		oldArcs.putAll(resetArcs);
		oldArcs.putAll(inhibitorArcs);
	}

	/**
	 * 
	 */
	public void prepareNewPortSocket() {
		oldPortSock.clear();
		changedSockets.clear();
		oldPortSock.putAll(portSocket);
	}

	/**
	 * @param channel
	 * @return
	 */
	public boolean setChannel(final String channel) {
		if (channel.equals(this.channel)) { return false; }
		this.channel = channel;
		return true;
	}

	/**
	 * @param code
	 * @return
	 */
	public boolean setCode(final String code) {
		if (code.equals(this.code)) { return false; }
		this.code = code;
		return true;
	}

	/**
	 * @param controllable
	 * @return
	 */
	public boolean setControllable(final boolean controllable) {
		if (controllable == this.controllable) { return false; }
		this.controllable = controllable;
		return true;
	}

	/**
	 * @param guard
	 * @return
	 */
	public boolean setGuard(final String guard) {
		if (guard.equals(this.guard)) { return false; }
		this.guard = guard;
		return true;
	}

	/**
	 * @param priority
	 * @return
	 */
	public boolean setPriority(final String priority) {
		if (priority.equals(this.priority)) { return false; }
		this.priority = priority;
		return true;
	}

	/**
	 * @param subpage
	 * @return
	 */
	public boolean setSubpage(final String subpage) {
		if (subpage.equals(subpage)) { return false; }
		this.subpage = subpage;
		return true;
	}

	/**
	 * @param substitution
	 * @return
	 */
	public boolean setSubstitution(final boolean substitution) {
		if (substitution == this.substitution) { return false; }
		this.substitution = substitution;
		return true;
	}

	/**
	 * @param time
	 * @return
	 */
	public boolean setTime(final String time) {
		if (time.equals(this.time)) { return false; }
		this.time = time;
		return true;
	}
}
