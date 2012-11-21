package org.cpntools.simulator.extensions;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Observable;

import org.cpntools.accesscpn.engine.protocol.Packet;

/**
 * @author michael
 */
public abstract class AbstractExtension extends Observable implements Extension {
	protected Map<Option<String>, String> stringOptions = new HashMap<Option<String>, String>();
	protected Map<Option<Integer>, Integer> integerOptions = new HashMap<Option<Integer>, Integer>();
	protected Map<Option<Boolean>, Boolean> booleanOptions = new HashMap<Option<Boolean>, Boolean>();
	private final List<Option<?>> options = new ArrayList<Option<?>>();
	private final List<Option<?>> u_options = Collections.unmodifiableList(options);
	private final List<Command> subscriptions = new ArrayList<Command>();
	private final List<Command> u_subscriptions = Collections.unmodifiableList(subscriptions);
	protected Channel channel;

	@Override
	public List<Option<?>> getOptions() {
		return u_options;
	}

	@Override
	public Object getRPCHandler() {
		return null;
	}

	@Override
	public List<Command> getSubscriptions() {
		return u_subscriptions;
	}

	@Override
	public Packet handle(final Packet p, final Packet response) {
		return null;
	}

	@Override
	public String inject() {
		return null;
	}

	@Override
	public <T> void setOption(final Option<T> option, final T value) {
		setOptionInternal(option, value);
		setChanged();
		notifyObservers(option);
	}

	@SuppressWarnings("unchecked")
	protected <T> T getOption(final Option<T> option) {
		if (option.getType() == Integer.class) { return (T) integerOptions.get(option); }
		if (option.getType() == Boolean.class) { return (T) booleanOptions.get(option); }
		if (option.getType() == String.class) { return (T) stringOptions.get(option); }
		return null;
	}

	protected int getInt(final Option<Integer> option) {
		try {
			return getOption(option);
		} catch (final NullPointerException _) {
			return 0;
		}
	}

	protected boolean getBoolean(final Option<Boolean> option) {
		try {
			return getOption(option);
		} catch (final NullPointerException _) {
			return false;
		}
	}

	protected String getString(final Option<String> option) {
		return getOption(option);
	}

	@Override
	public Extension start(final Channel c) {
		try {
			final AbstractExtension instance = getClass().newInstance();
			instance.setChannel(c);
			return instance;
		} catch (final InstantiationException e) {
		} catch (final IllegalAccessException e) {
		}
		return null;
	}

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

	protected void addSubscription(final Command c) {
		subscriptions.add(c);
	}

	protected void addSubscription(final Command... cs) {
		for (final Command c : cs) {
			addSubscription(c);
		}
	}

	protected void setChannel(final Channel c) {
		channel = c;
	}

}
