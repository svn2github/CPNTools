package org.cpntools.simulator.extensions.server;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.cpntools.accesscpn.engine.protocol.Packet;
import org.cpntools.simulator.extensions.Extension;

public class Server implements Runnable, Iterable<Extension> {
	/**
	 * 
	 */
	public static final int DEFAULT_PORT = 1998;
	protected final ServerSocket socket;
	private final List<Extension> extensions;

	public Server(final int port, final Extension... extensions) throws IOException {
		this.extensions = new ArrayList<Extension>(extensions.length);
		for (final Extension e : extensions) {
			this.extensions.add(e);
		}
		socket = new ServerSocket(port);
	}

	/**
	 * @param port
	 * @throws IOException
	 */
	public Server(final int port, final List<Extension> extensions) throws IOException {
		this.extensions = extensions;
		socket = new ServerSocket(port);
	}

	/**
	 * @see java.lang.Runnable#run()
	 */
	@Override
	public void run() {
		while (true) {
			try {
				try {
					final Socket connection = socket.accept();
					final Packet p = new Packet();
					final DataInputStream in = new DataInputStream(connection.getInputStream());
					final DataOutputStream out = new DataOutputStream(connection.getOutputStream());
					p.receive(in);
					if (p.getOpcode() != 1 && p.getOpcode() != 2) {
						new Packet(3, "").send(out); //$NON-NLS-1$
						connection.close();
						throw new Exception("Wrong login");
					}
					new Packet(1, "").send(out); //$NON-NLS-1$
					new Handler(in, out, extensions, "for port " + connection.getRemoteSocketAddress());
				} catch (final SocketException se) {
					se.printStackTrace();
				}
			} catch (final Exception _) {

			}
		}
	}

	/**
	 * @throws IOException
	 */
	public void stop() throws IOException {
		socket.close();
	}

	/**
	 * @see java.lang.Iterable#iterator()
	 */
	@Override
	public Iterator<Extension> iterator() {
		return extensions.iterator();
	}
}
