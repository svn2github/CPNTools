package org.cpntools.simulator.extensions.scraper;

/**
 * @author michael
 */
public class Arc extends Element {
	/**
	 * @author michael
	 */
	public enum Type {
		/**
		 * 
		 */
		BOTHDIR, /**
		 * 
		 */
		INHIBITOR, /**
		 * 
		 */
		INPUT, /**
		 * 
		 */
		OUTPUT, /**
		 * 
		 */
		RESET
	}

	private String inscription;
	private final Place p;
	private final Page page;

	private final Transition t;

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

	/**
	 * @return
	 */
	public String getInscription() {
		return inscription;
	}

	/**
	 * @return
	 */
	public Page getPage() {
		return page;
	}

	/**
	 * @return
	 */
	public Place getPlace() {
		return p;
	}

	/**
	 * @return
	 */
	public Transition getTransition() {
		return t;
	}

	/**
	 * @return
	 */
	public Type getType() {
		return type;
	}

	/**
	 * @param inscription
	 * @return
	 */
	public boolean setInscription(final String inscription) {
		if (inscription.equals(this.inscription)) { return false; }
		this.inscription = inscription;
		return true;
	}

}
