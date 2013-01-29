package org.cpntools.simulator.extensions.export;

import java.io.CharArrayReader;
import java.io.CharArrayWriter;

import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import org.cpntools.accesscpn.engine.protocol.Packet;
import org.cpntools.simulator.extensions.AbstractExtension;

/**
 * @author michael
 * @param <T>
 */
public class ExportExtension extends AbstractExtension {
	public static final int ID = 10003;

	public ExportExtension() {
	}

	/**
	 * @see org.cpntools.simulator.extensions.Extension#getIdentifier()
	 */
	@Override
	public int getIdentifier() {
		return ID;
	}

	/**
	 * @see org.cpntools.simulator.extensions.Extension#getName()
	 */
	@Override
	public String getName() {
		return "Export Helper";
	}

	@Override
	public Packet handle(final Packet p) {
		try {
			p.reset();
			p.getInteger(); // 10000 external
			p.getInteger(); // 10003 export
			final int command = p.getInteger();
			final Packet result = new Packet(7, 1);
			switch (command) {
			case 1:
				result.addString(cpnToPNML(p.getString()));
				break;
			default:
				throw new Exception("Unknown command for export (" + command + ")");
			}
			return result;
		} catch (final Exception e) {
			e.printStackTrace();
			final Packet result = new Packet(7, -1);
			result.addString(e.getMessage());
			return result;
		}
	}

	private String cpnToPNML(final String string) throws TransformerException {
		final TransformerFactory factory = TransformerFactory.newInstance();
		final Source xslt = new StreamSource(ExportExtension.class.getResourceAsStream("/cpn2pnml.xsl"));
		final Transformer transformer = factory.newTransformer(xslt);

		final Source text = new StreamSource(new CharArrayReader(string.toCharArray()));
		final CharArrayWriter result = new CharArrayWriter();
		transformer.transform(text, new StreamResult(result));
		return new String(result.toCharArray());
	}

}
