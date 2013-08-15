package org.cpntools.simulator.extensions;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Observable;

import org.cpntools.accesscpn.engine.protocol.Packet;
import org.cpntools.simulator.extensions.scraper.Element;

/**
 * @author michael
 */
public abstract class AbstractExtension extends Observable implements Extension {
	private final List<Instrument> instruments = new ArrayList<Instrument>();
	private final List<Command> lazysubscriptions = new ArrayList<Command>();
	private final List<Option<?>> options = new ArrayList<Option<?>>();
	private final List<Command> subscriptions = new ArrayList<Command>();
	private final List<Instrument> u_instruments = Collections.unmodifiableList(instruments);
	private final List<Option<?>> u_options = Collections.unmodifiableList(options);
	private final List<Command> u_subscriptions = Collections.unmodifiableList(subscriptions);
	protected Map<Option<Boolean>, Boolean> booleanOptions = new HashMap<Option<Boolean>, Boolean>();
	protected Channel channel;
	protected Map<Option<Integer>, Integer> integerOptions = new HashMap<Option<Integer>, Integer>();
	protected boolean lazyDone = false;

	/**
	 * used for notification when lazy subscriptions are made
	 */
	public static final Object LAZY_SUBSCRIPTIONS_DONE = new Object();

	protected Map<Option<String>, String> stringOptions = new HashMap<Option<String>, String>();

	/**
	 * @see org.cpntools.simulator.extensions.Extension#getInstruments()
	 */
	@Override
	public List<Instrument> getInstruments() {
		return u_instruments;
	}

	/**
	 * @see org.cpntools.simulator.extensions.Extension#getOptions()
	 */
	@Override
	public List<Option<?>> getOptions() {
		return u_options;
	}

	/**
	 * @see org.cpntools.simulator.extensions.Extension#getRPCHandler()
	 */
	@Override
	public Object getRPCHandler() {
		return null;
	}

	/**
	 * @see org.cpntools.simulator.extensions.Extension#getSubscriptions()
	 */
	@Override
	public List<Command> getSubscriptions() {
		return u_subscriptions;
	}

	/**
	 * @see org.cpntools.simulator.extensions.Extension#handle(org.cpntools.accesscpn.engine.protocol.Packet)
	 */
	@Override
	public Packet handle(final Packet p) {
		return null;
	}

	/**
	 * @see org.cpntools.simulator.extensions.Extension#handle(org.cpntools.accesscpn.engine.protocol.Packet,
	 *      org.cpntools.accesscpn.engine.protocol.Packet)
	 */
	@Override
	public Packet handle(final Packet p, final Packet response) {
		return null;
	}

	/**
	 * @see org.cpntools.simulator.extensions.Extension#inject()
	 */
	@Override
	public String inject() {
		return null;
	}

	/**
	 * @param i
	 * @param e
	 */
	@Override
	public void invokeInstrument(final Instrument i, final Element e) {
		setChanged();
		notifyObservers(new Invocation(i, e));
	}

	/**
	 * @see org.cpntools.simulator.extensions.Extension#prefilter(org.cpntools.accesscpn.engine.protocol.Packet)
	 */
	@Override
	public Packet prefilter(final Packet p) {
		return null;
	}

	/**
	 * @see org.cpntools.simulator.extensions.Extension#setOption(org.cpntools.simulator.extensions.Option,
	 *      java.lang.Object)
	 */
	@Override
	public <T> void setOption(final Option<T> option, final T value) {
		setOptionInternal(option, value);
		setChanged();
		notifyObservers(option);
	}

	/**
	 * @see org.cpntools.simulator.extensions.Extension#start(org.cpntools.simulator.extensions.Channel)
	 */
	@Override
	public Extension start(final Channel c) {
		try {
			final AbstractExtension instance = getClass().newInstance();
			instance.setChannel(c);
			return instance;
		} catch (final InstantiationException e) {
			// IGnore
		} catch (final IllegalAccessException e) {
			// Ignore
		}
		return null;
	}

	/**
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return getName();
	}

	@SuppressWarnings("unchecked")
	private <T> void setOptionInternal(final Option<T> option, final T value) {
		if (option.getType() == Integer.class) {
			int intValue = 0;
			if (value != null) {
				intValue = (Integer) value;
			}
			integerOptions.put((Option<Integer>) option, intValue);
		} else if (option.getType() == Boolean.class) {
			boolean booleanValue = false;
			if (value != null) {
				booleanValue = (Boolean) value;
			}
			booleanOptions.put((Option<Boolean>) option, booleanValue);
		} else if (option.getType() == String.class) {
			String stringValue = "";
			if (value != null) {
				stringValue = (String) value;
			}
			stringOptions.put((Option<String>) option, stringValue);
		} else {
			throw new IllegalArgumentException("Unknown option type");
		}
	}

	protected void addInstrument(final Instrument... is) {
		for (final Instrument i : is) {
			instruments.add(i);
		}
	}

	protected void addLazySubscription(final Command c) {
		lazysubscriptions.add(c);
	}

	protected void addLazySubscription(final Command... cs) {
		for (final Command c : cs) {
			addLazySubscription(c);
		}
	}

	@SuppressWarnings("hiding")
	protected void addOption(final Option<?>... options) {
		for (final Option<?> option : options) {
			addOption(option);
		}
	}

	protected <T> void addOption(final Option<T> option) {
		addOption(option, null);
	}

	protected <T> void addOption(final Option<T> option, final T defaultValue) {
		setOptionInternal(option, defaultValue);
		options.add(option);
	}

	protected void addSubscription(final Command... cs) {
		for (final Command c : cs) {
			subscriptions.add(c);
		}
	}

	protected boolean getBoolean(final Option<Boolean> option) {
		try {
			return getOption(option);
		} catch (final NullPointerException _) {
			return false;
		}
	}

	protected int getInt(final Option<Integer> option) {
		try {
			return getOption(option);
		} catch (final NullPointerException _) {
			return 0;
		}
	}

	@SuppressWarnings("unchecked")
	protected <T> T getOption(final Option<T> option) {
		if (option.getType() == Integer.class) { return (T) integerOptions.get(option); }
		if (option.getType() == Boolean.class) { return (T) booleanOptions.get(option); }
		if (option.getType() == String.class) { return (T) stringOptions.get(option); }
		return null;
	}

	protected String getString(final Option<String> option) {
		return getOption(option);
	}

	protected void makeLazySubscriptions() {
		if (lazyDone) { return; }
		lazyDone = true;
		try {
			for (final Command c : lazysubscriptions) {
				channel.subscribe(c, this);
			}
			setChanged();
			notifyObservers(lazysubscriptions);
		} catch (final Exception _) {
			// If called before channel is set or before subscriptions is created buy channel
			subscriptions.addAll(lazysubscriptions);
		}
		lazysubscriptions.clear();
	}

	protected void setChannel(final Channel c) {
		channel = c;
	}
}
