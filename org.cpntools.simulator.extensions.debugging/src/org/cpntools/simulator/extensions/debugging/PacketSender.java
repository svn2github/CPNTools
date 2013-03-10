package org.cpntools.simulator.extensions.debugging;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;

import javax.swing.BoxLayout;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;
import javax.swing.ScrollPaneConstants;

import org.cpntools.accesscpn.engine.protocol.Packet;

/**
 * @author michael
 */
public class PacketSender extends DebuggingPanel {

	/**
	 * @author michael
	 */
	public class NamedInteger {

		private final String name;
		private final int value;

		/**
		 * @param name
		 * @param value
		 */
		public NamedInteger(final String name, final int value) {
			this.name = name;
			this.value = value;
		}

		/**
		 * @return
		 */
		public int getValue() {
			return value;
		}

		/**
		 * @see java.lang.Object#toString()
		 */
		@Override
		public String toString() {
			return name;
		}
	}

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * 
	 */
	public PacketSender() {
		final JPanel bis = new JPanel();
		bis.setLayout(new BoxLayout(bis, BoxLayout.Y_AXIS));

		final JPanel b = new JPanel(new BorderLayout());
		final DefaultListModel bmodel = new DefaultListModel();
		final JList bs = new JList(bmodel);
		bs.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
		b.add(new JScrollPane(bs, ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS,
		        ScrollPaneConstants.HORIZONTAL_SCROLLBAR_ALWAYS));
		JPanel value = new JPanel(new FlowLayout(FlowLayout.CENTER));
		final JButton btrue = new JButton("True");
		btrue.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(final ActionEvent arg0) {
				bmodel.addElement(true);
			}
		});
		value.add(btrue);
		final JButton bfalse = new JButton("False");
		bfalse.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(final ActionEvent arg0) {
				bmodel.addElement(false);
			}
		});
		value.add(bfalse);
		final JButton bremove = new JButton("Remove");
		bremove.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(final ActionEvent arg0) {
				final int[] selectedIndices = bs.getSelectedIndices();
				for (int i = selectedIndices.length - 1; i >= 0; i--) {
					bmodel.removeElementAt(selectedIndices[i]);
				}
			}
		});
		value.add(bremove);
		final JButton bclear = new JButton("Clear");
		bclear.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(final ActionEvent e) {
				bmodel.clear();
			}
		});
		value.add(bclear);
		b.add(value, BorderLayout.SOUTH);
		bis.add(b);

		final JPanel i = new JPanel(new BorderLayout());
		final DefaultListModel imodel = new DefaultListModel();
		final JList is = new JList(imodel);
		is.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
		i.add(new JScrollPane(is, ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS,
		        ScrollPaneConstants.HORIZONTAL_SCROLLBAR_ALWAYS));
		value = new JPanel(new FlowLayout(FlowLayout.CENTER));
		final JTextField iinput = new JTextField();
		iinput.setText("0");
		iinput.setPreferredSize(new Dimension(200, 20));
		value.add(iinput);
		final JButton iadd = new JButton("Add");
		iadd.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(final ActionEvent arg0) {
				try {
					@SuppressWarnings("hiding")
					final int value = Integer.parseInt(iinput.getText());
					imodel.addElement(value);
					iinput.selectAll();
					iinput.requestFocus();
				} catch (final NumberFormatException _) {
					iinput.setText("0");
				}
			}
		});
		value.add(iadd);
		final JButton iremove = new JButton("Remove");
		value.add(iremove);
		iremove.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(final ActionEvent arg0) {
				final int[] selectedIndices = is.getSelectedIndices();
				for (@SuppressWarnings("hiding")
				int i = selectedIndices.length - 1; i >= 0; i--) {
					imodel.removeElementAt(selectedIndices[i]);
				}
			}
		});
		final JButton iclear = new JButton("Clear");
		iclear.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(final ActionEvent e) {
				imodel.clear();
				iinput.setText("0");
				iinput.selectAll();
				iinput.requestFocus();
			}
		});
		value.add(iclear);
		i.add(value, BorderLayout.SOUTH);
		bis.add(i);

		final JPanel s = new JPanel(new BorderLayout());
		final DefaultListModel smodel = new DefaultListModel();
		final JList ss = new JList(smodel);
		ss.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
		s.add(new JScrollPane(ss, ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS,
		        ScrollPaneConstants.HORIZONTAL_SCROLLBAR_ALWAYS));
		value = new JPanel(new FlowLayout(FlowLayout.CENTER));
		final JTextField sinput = new JTextField();
		sinput.setPreferredSize(new Dimension(200, 20));
		value.add(sinput);
		final JButton sadd = new JButton("Add");
		sadd.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(final ActionEvent arg0) {
				smodel.addElement(sinput.getText());
				sinput.setText("");
				sinput.requestFocus();
			}
		});
		value.add(sadd);
		final JButton sremove = new JButton("Remove");
		sremove.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(final ActionEvent arg0) {
				final int[] selectedIndices = ss.getSelectedIndices();
				for (@SuppressWarnings("hiding")
				int i = selectedIndices.length - 1; i >= 0; i--) {
					smodel.removeElementAt(selectedIndices[i]);
				}
			}
		});
		value.add(sremove);
		final JButton sclear = new JButton("Clear");
		sclear.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(final ActionEvent e) {
				smodel.clear();
				sinput.setText("");
				sinput.requestFocus();
			}
		});
		value.add(sclear);
		s.add(value, BorderLayout.SOUTH);
		bis.add(s);

		add(bis);

		final JPanel type = new JPanel(new FlowLayout());
		type.add(new JLabel("Packet type"));
		final JComboBox opcode = new JComboBox(new Object[] { new NamedInteger("Reponse", 7),
		        new NamedInteger("GUI Message", 3), new NamedInteger("Command", 9) });
		opcode.setEditable(true);
		type.add(opcode);
		final JTextField command = new JTextField();
		command.setText("0");
		command.setPreferredSize(new Dimension(200, 20));
		type.add(new JLabel("     Command"));
		type.add(command);
		add(type, BorderLayout.NORTH);

		final JPanel buttons = new JPanel(new FlowLayout(FlowLayout.CENTER));
		final JButton send = new JButton("Send");
		send.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(final ActionEvent e) {
				try {
					@SuppressWarnings("hiding")
					int type = 7;
					final Object tobject = opcode.getSelectedItem();
					if (tobject instanceof NamedInteger) {
						type = ((NamedInteger) tobject).getValue();
					} else if (tobject instanceof Integer) {
						type = (Integer) tobject;
					} else if (tobject instanceof String) {
						type = Integer.parseInt(tobject.toString());
					}

					final int cmd = Integer.parseInt(command.getText());

					final Packet p = new Packet(type, cmd);
					for (@SuppressWarnings("hiding")
					int i = 0; i < bmodel.size(); i++) {
						p.addBoolean((Boolean) bmodel.getElementAt(i));
					}
					for (@SuppressWarnings("hiding")
					int i = 0; i < imodel.size(); i++) {
						p.addInteger((Integer) imodel.getElementAt(i));
					}
					for (@SuppressWarnings("hiding")
					int i = 0; i < smodel.size(); i++) {
						p.addString((String) smodel.getElementAt(i));
					}

					try {
						orphanage.handle(channel.send(p));
					} catch (final IOException e1) {
						// TODO Auto-generated catch block
						e1.printStackTrace();
					}

				} catch (final NumberFormatException _) {
					opcode.setSelectedIndex(0);
					command.setText("0");
				}
			}
		});
		buttons.add(send);

		final JButton clear = new JButton("Clear");
		clear.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(final ActionEvent e) {
				bmodel.clear();
				imodel.clear();
				iinput.setText("0");
				smodel.clear();
				sinput.setText("");
			}
		});
		buttons.add(clear);
		add(buttons, BorderLayout.SOUTH);
	}

	/**
	 * @see org.cpntools.simulator.extensions.debugging.DebuggingPanel#getName()
	 */
	@Override
	public String getName() {
		return "Send Packets";
	}

}
