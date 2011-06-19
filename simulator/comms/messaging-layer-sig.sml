(*************************** 
* messaging-layer-sig.sml *
***************************
* 
* Created: July 15th 2001 by Guy Edward Gallasch
* 
* Description: Signature of functions used to packetise data to send, and 
* depacketise received data.  Independent of the type of data sent 
* or received.
* 
*)


signature MESSAGING_LAYER =
sig

    (* The communication channel, from the CommsLayer implementation. *)
    type channel

    (* Exception to be raised if a received header is invalid. *)
    exception InvalidDataExn of string

    (* Packetise data and send to external process *)
    val send : channel * Word8Vector.vector -> unit

    (* Receive data from external process and depacketise *)
    val receive : channel -> Word8Vector.vector

    (* Wait for and receive data from any input channel *)
(*  val select : channel list -> (channel * Word8Vector.vector) list*)

    (* End of signature. *)
end

