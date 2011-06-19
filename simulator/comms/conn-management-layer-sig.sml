(*********************************
 * conn-management-layer-sig.sml *
 *********************************
 *
 * Created: January 2001 by Guy Edward Gallasch
 * 
 * Description: Signature of functions used to establish communications
 *   and to communicate between SML and external processes.  Independent
 *   of the underlying communication mechanism.
 *
 *)

signature CONN_MANAGEMENT_LAYER =
  sig
  
    (* Unique connection identifier. *)
    type Connection 

    (* Exception to be raised if a connection cannot be found. *)
    exception ElementMissingExn of string

    (* Exception to be raised if a connection name is used twice. *)
    exception DupConnNameExn of string

    (* Open a new connection and store its details. *)
    val openConnection : Connection * string * int -> unit

    (* Wait for an external process to connect and store its details. *)
    val acceptConnection : Connection * int -> unit

    (* Send data to an external process. *)
    val send : Connection * 'a * ('a -> Word8Vector.vector) -> unit

    val canreceive : Connection -> bool;

    (* Receive data from an external process. *)
    val receive : Connection * (Word8Vector.vector -> 'a) -> 'a

    (* Look up a connection name and close the connection. *) 
    val closeConnection : Connection -> unit 

  (* End of signature. *)
  end

