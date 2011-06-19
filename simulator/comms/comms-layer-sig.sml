(***********************
 * comms-layer-sig.sml *
 ***********************
 *
 * Based on the following file:
 *
 *************************
 * sock-util-sig.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *************************
 * 
 * Created/Modified: January 2001 by Guy Edward Gallasch
 *
 * Description: Various utility functions for programming with sockets.
 *
 * Current version: 1.2 
 *
 * ChangeLog:
 *
 *   Version 1.1 - 15th July 2001
 *    - removed resolveAddr, connectINetStrm and bindINetStrm from the 
 *      signature
 *    - renamed communication_device to channel
 *    - removed TCP/IP related datatypes from the signature, replaced with channel 
 *    - renamed file from new-sock-util-sig.sml to comms-layer-sig.sml
 *    - renamed signature from NEW_SOCK_UTIL to COMMS_LAYER
 *
 *)

signature COMMS_LAYER =
  sig

    (* The communication channel, sockets in this implementation. *)
    type channel

    (* Exception to be raised in the case that the address is invalid. *)
    exception BadAddr of string

    (* Acting as a client, connect to an external process. *)
    val connect : string * int -> channel

    (* Acting as a server, allow an external client to connect. *)
    val accept : string * int -> channel

    (* Send a vector of bytes to an open socket. *)
    val send : channel * Word8Vector.vector -> unit

    (* Receive a vector of bytes from an open socket. *)
    val receive : channel * int -> Word8Vector.vector

    (* Receive a vector of up to bytes from an open socket. *)
    val receive' : channel * int -> Word8Vector.vector

    (* Check if a receive on a channel will block *)
    val canreceive : channel -> bool;

    (* Close the socket to the external process. *)
    val disconnect : channel -> unit

  (* End of signature. *)
  end;
