package org.cpntools.simulator.extensions.scraper;

/**
 * @author michael
 */
public class Arc extends Element {
	/**
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + (inscription == null ? 0 : inscription.hashCode());
		result = prime * result + (type == null ? 0 : type.hashCode());
		return result;
	}

	/**
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(final Object obj) {
		if (this == obj) { return true; }
		if (!super.equals(obj)) { return false; }
		if (!(obj instanceof Arc)) { return false; }
		final Arc other = (Arc) obj;
		if (inscription == null) {
			if (other.inscription != null) { return false; }
		} else if (!inscription.equals(other.inscription)) { return false; }
		if (type != other.type) { return false; }
		return true;
	}

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
	 * @param dictionary
	 * @param id
	 * @param inscription
	 * @param type
	 * @param page
	 * @param t
	 * @param p
	 */
	public Arc(final ElementDictionary dictionary, final String id, final String inscription, final Type type,
	        final Page page, final Transition t, final Place p) {
		super(dictionary, id);
		this.type = type;
		this.page = page;
		this.t = t;
		this.p = p;
		setInscription(inscription);
	}

	/**
	 * @return
	 */
	public boolean addToNodes() {
		p.addArc(this);
		return t.addArc(this);
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
