package org.cpntools.simulator.extensions.ppcpnets.java;

/**
 * @author michael
 */
public class AcquireLock extends ASTNode {

	private final Lock lock;

	/**
	 * @param next
	 * @param lock
	 */
	public AcquireLock(final ASTNode next, final Lock lock) {
		super(next);
		this.lock = lock;
	}

	/**
	 * @return
	 */
	public Lock getLock() {
		return lock;
	}

}
