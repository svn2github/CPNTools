package org.cpntools.simulator.extensions.scraper;

public class Node extends HasName {

	private final Page page;

	public Node(final String id, final String name, final Page page) {
		super(id, name);
		this.page = page;
	}

	public Page getPage() {
		return page;
	}

}
