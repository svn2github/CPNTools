package org.cpntools.simulator.extensions.scraper;

/**
 * @author michael
 */
public class Arc extends Element {
	public enum Type {
		INPUT, OUTPUT, BOTHDIR, INHIBITOR, RESET
	};

	private final Transition t;
	private final Place p;
	private final Type type;

	/**
	 * @param id
	 * @param inscription
	 * @param type
	 * @param page
	 * @param t
	 * @param p
	 */
	public Arc(final String id, final String inscription, final Type type, final Page page, final Transition t,
	        final Place p) {
		super(id);
		this.type = type;
		this.page = page;
		this.t = t;
		this.p = p;
		setInscription(inscription);
	}

	private final Page page;
	private String inscription;

	public boolean setInscription(final String inscription) {
		if (inscription.equals(this.inscription)) { return false; }
		this.inscription = inscription;
		return true;
	}

	public Page getPage() {
		return page;
	}

	public String getInscription() {
		return inscription;
	}

	public Type getType() {
		return type;
	}

	public Transition getTransition() {
		return t;
	}

	public Place getPlace() {
		return p;
	}

}
