package org.cpntools.simulator.extensions.ppcpnets.java;

import org.cpntools.simulator.extensions.scraper.types.Bool;
import org.cpntools.simulator.extensions.scraper.types.Int;
import org.cpntools.simulator.extensions.scraper.types.Other;
import org.cpntools.simulator.extensions.scraper.types.Type;

/**
 * @author michael
 */
public class Variable implements HasJavaName {

	/**
	 * @param args
	 */
	public static void main(final String... args) {
		System.out.println(Variable.nextName("Britney"));
		System.out.println(Variable.nextName("Britney0"));
		System.out.println(Variable.nextName("Britney1"));
		System.out.println(Variable.nextName("Britney5"));
		System.out.println(Variable.nextName("Britney42"));
		System.out.println(Variable.nextName("Britney59"));
		System.out.println(Variable.nextName("Brit4ney"));
		System.out.println(new Variable("a", "v", new Int(), ""));
		System.out.println(new Variable("a", "vv", new Bool(), ""));
		System.out.println(new Variable("a", "v1", new org.cpntools.simulator.extensions.scraper.types.String(), ""));
		System.out.println(new Variable("a", "v'", new Other("PROD"), ""));
		System.out.println(new Variable("a", "v''", new Other("PROD_TYPE"), ""));
		System.out.println(new Variable("a", "v'''", new Int(), ""));
		System.out.println(new Variable("a", "theName", new Int(), ""));
		System.out.println(new Variable("a", "the_name", new Int(), ""));
		System.out.println(new Variable("a", "The name", new Int(), ""));
		System.out.println(new Variable("a", "The Name", new Int(), ""));
		System.out.println(new Variable("a", "the name", new Int(), ""));
		System.out.println(new Variable("a", "the Name", new Int(), ""));
		System.out.println(new Variable("id123", "The Name", new Int(), ""));
		System.out.println(new Variable("id123", "", new Int(), ""));
		for (final String s : args) {
			System.out.println(new Variable("id123", s, null, "").getJavaName());
		}
	}

	/**
	 * @param theName
	 * @return
	 */
	public static String makeJavaName(final String theName) {
		final StringBuilder result = new StringBuilder();
		boolean first = true;
		for (final String t : theName.split("[_ ]")) {
			if (!"".equals(t.trim())) {
				if (first) {
					result.append(t.substring(0, 1).toLowerCase());
				} else {
					result.append(t.substring(0, 1).toUpperCase());
				}
				first = false;
				result.append(t.substring(1));
			}
		}
		String r = result.toString();
		String s = r;
		do {
			int i = 1;
			while (!r.equals(s)) {
				r = s;
				s = s.replaceAll(i + "'", "" + (i + 1));
				i++;
			}
			s = r.replaceFirst("'", "1");
		} while (!r.equals(s));
		return r;
	}

	/**
	 * @param name
	 * @return
	 */
	public static String nextName(final String name) {
		if (name.matches(".*[0123456789]")) {
			String number = "";
			String rest = name;
			while (rest.matches(".*[0123456789]")) {
				number = rest.substring(rest.length() - 1) + number;
				rest = rest.substring(0, rest.length() - 1);
			}
			return rest + (Integer.parseInt(number) + 1);
		}
		return name + "_1";
	}

	private final String id;

	private String name;

	private final String originalType;

	private final Type type;

	/**
	 * @param id
	 * @param name
	 * @param type
	 * @param originalType
	 */
	public Variable(final String id, final String name, final Type type, final String originalType) {
		this.id = id;
		this.name = name;
		this.type = type;
		this.originalType = originalType;
	}

	/**
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(final Object obj) {
		if (this == obj) { return true; }
		if (obj == null) { return false; }
		if (!(obj instanceof Variable)) { return false; }
		final Variable other = (Variable) obj;
		if (id == null) {
			if (other.id != null) { return false; }
		} else if (!id.equals(other.id)) { return false; }
		if (name == null) {
			if (other.name != null) { return false; }
		} else if (!name.equals(other.name)) { return false; }
		return true;
	}

	/**
	 * @return
	 */
	public String getId() {
		return id;
	}

	/**
	 * @return
	 */
	@Override
	public String getJavaName() {
		return Variable.makeJavaName("".equals(name) ? getId() : name);
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
	public String getOriginalType() {
		return originalType;
	}

	/**
	 * @return
	 */
	public Type getType() {
		return type;
	}

	/**
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + (id == null ? 0 : id.hashCode());
		result = prime * result + (name == null ? 0 : name.hashCode());
		return result;
	}

	/**
	 * @param name
	 */
	@Override
	public void setName(final String name) {
		this.name = name;
	}

	/**
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		if (type != null) { return type.getJavaName() + " " + getJavaName(); }
		return getJavaName();
	}

}
