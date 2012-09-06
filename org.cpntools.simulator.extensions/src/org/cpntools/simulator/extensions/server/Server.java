package org.cpntools.simulator.extensions.server;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketException;

import org.cpntools.accesscpn.engine.protocol.Packet;

public class Server implements Runnable {
	/**
	 * 
	 */
	public static final int DEFAULT_PORT = 1985;
	protected final ServerSocket socket;

	/**
	 * @param port
	 * @throws IOException
	 */
	public Server(final int port) throws IOException {
		socket = new ServerSocket(port);
	}

	/**
	 * @throws IOException
	 */
	public void stop() throws IOException {
		socket.close();
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
					new Thread(new Handler(in, out), "Handler for port " + connection.getLocalPort()).start();
				} catch (final SocketException se) {
					break;
				}
			} catch (final Exception _) {

			}
		}
	}
}
