(***************************** 
 * conn-management-layer.sml *
 *****************************
 * 
 * Created: January 2001 by Guy Edward Gallasch
 * 
 * Description: Functions used to establish communications and to
 *   communicate between SML and external processes.  Independent of the
 *   underlying communication mechanism.
 * 
 *)


structure ConnManagementLayer : CONN_MANAGEMENT_LAYER =
  struct

    (* Unique connection identifier, a string. *)
    type Connection = string;

    (* The information to identify connections. *)
    type ext_conn_id = 
    {
      name    : Connection, 
      channel : CommsLayer.channel
    };

    (* List of connections, initialised to the empty list. *)
    val connections : ext_conn_id list ref = ref ([])

    (* Exception to be raised if a connection cannot be found. *)
    exception ElementMissingExn of string;

    (* Exception to be raised if a connection name is used twice. *)
    exception DupConnNameExn of string;


   (*******************
    * FUNCTION search *
    *******************
    * Description: Search for a connection with the specified name. 
    *
    * A "private" function, hidden from the user.
    *
    * Precondition: A need to search for a connection has arisen.
    * Postcondition: If found, a connection has been returned.  If not
    *   found, NONE is returned.
    * 
    * Input: (string) The name of a connection to search for and return.
    * Output: (ext_conn_id option) If found, information about the
    *   located connection.  If not found, NONE. 
    *)
    
    fun search(conn_name) = 
      let 

        (* Local function to search the list recursively. *)
        fun recurse([]) =

           (* If not found, return NONE. *)
           NONE

          | recurse((conn:ext_conn_id as {name = name, ...})::conns) = 

          (* If found, then return the connection .... *)
          if ((#name conn) = conn_name)
          then
            SOME(conn)

          (* .... else search the rest of the list. *)
          else
            recurse(conns)
      in

        (* Begin searching the entire list. *)
        recurse(!connections)
      end


   (***************************
    * FUNCTION openConnection *
    ***************************
    * Description: Creates and opens a new connection and stores
    *   information about it.
    * 
    * Precondition: A need to create a new connection to an external
    *   process has arisen.
    * Postcondition: A new connection has been created and/or opened. 
    *
    * Input: (string) The identifier to be associated with the new connection.
    *        (string) The host name of the external process.
    *        (int) The port number of the external process.
    * Output: (unit)
    *)

    fun openConnection (conn_name, host, port) =

      (* Search for a duplicate name. *)
      case search(conn_name) of 

      (* If the name is not already in use then .... *)
      NONE =>
        let

          (* .... open a client external communications channel .... *)
          val new_channel = CommsLayer.connect(host, port);
  
          (* .... compile information about this new connection .... *) 
          val conn:ext_conn_id = {name = conn_name, channel = new_channel} 
        in

          (* .... and store information about the new connection. *)
          connections := conn::(!connections)
        end

      (* If the name is already in use then raise an exception. *)
      | SOME conn =>
        raise DupConnNameExn 
          ("Failure opening connection: name already in use.")


   (*****************************
    * FUNCTION acceptConnection *
    *****************************
    * Description: Waits for an external process to connect to this one,
    *   then stores information about the connection. 
    * 
    * Precondition: A connection attempt by a client process is expected.
    * Postcondition: A new connection has been created. 
    *
    * Input: (string) The name to be associated with the new connection.
    *        (int) The port number to listen to.
    * Output: (unit)
    *)

    fun acceptConnection (conn_name, port) =

      (* Search for a duplicate name. *)
      case search(conn_name) of 

      (* If the name is not already in use then .... *)
      NONE =>
        let

          (* .... allow a client process to connect to this one .... *)
          val new_channel = CommsLayer.accept("localhost",port);
  
          (* .... compile information about this new connection .... *) 
          val conn:ext_conn_id = {name = conn_name, channel = new_channel} 

        in

          (* .... and store information about the new connection. *)
          connections := conn::(!connections)
        end

      (* If the name is already used then raise an exception. *)
      | SOME conn =>
        raise DupConnNameExn 
          ("Failure opening connection: name already in use.")


   (*****************
    * FUNCTION send *
    *****************
    * Description: Send data to an external process.  The data is 
    *   transformed into a vector of bytes using the provided encoding 
    *   function.  The vector of bytes is passed to the send function in 
    *   the MessagingLayer structure in order to be segmented into packets 
    *   for transmission to the external process. 
    *
    * Precondition: A connection has been opened to an external process.
    * Postcondition: Data has been sent to this external process.
    * 
    * Input: (string) The name of the connection to send the data to.
    *        (to_send) The data being sent.
    *        ('a -> Word8Vector.vector) An encoding function to transform
    *   the data being sent into a vector of bytes.
    * Output: (unit)
    *)

    fun send (conn_name, to_send, encodeFunction) =

      (* Search for the connection. *)
      case search(conn_name) of

      (* If the connection was found .... *)
      SOME conn =>
        let

          (* Convert the data to a stream of bytes. *)
          val byte_stream = encodeFunction to_send 
        in

          (* Send the vector of bytes. *)
          MessagingLayer.send((#channel conn), byte_stream)
        end

      (* If the connection was not found, raise exception. *)
      | NONE =>
        raise ElementMissingExn("Failure sending data: Connection not found.")


   (********************
    * FUNCTION receive *
    ********************
    * Description: Receive data from an external process.  A vector 
    *   of bytes is received from the Messaging layer after the  
    *   packetisation has been removed.  The provided decoding function 
    *   restores the vector of bytes to the specified data type. 
    * 
    * Precondition: A connection has been opened to an external process.
    * Postcondition: Data has been received from an external process.
    *
    * Input: (string) The name of the connection to receive data from.
    *        (Word8Vector.vector -> 'a) A decode function to transform the
    *   stream of bytes into data. 
    * Output: ('a) The data received.
    *)

    fun	canreceive (conn_name) = 
      case search(conn_name) of 
           SOME conn => CommsLayer.canreceive (#channel conn)
         | NONE => 
	    raise ElementMissingExn("Failure receiving data: Connection not found.");
        
    fun receive (conn_name, decodeFunction) =

      (* Search for the connection. *)
      case search(conn_name) of

      (* If the connection was found .... *)
      SOME conn =>

        (* Receive a vector of bytes and decode *)
        decodeFunction (MessagingLayer.receive(#channel conn))
 
      (* If the connection was not found, raise exception. *)
      | NONE =>
        raise ElementMissingExn("Failure receiving data: Connection not found.")


   (****************************
    * FUNCTION closeConnection *
    ****************************
    * Description: Look up a connection name and close the connection to
    *   the external process.
    * 
    * Precondition: Communication with an external process has ended.
    * Postcondition: A connection has been closed and information about it
    *   has been removed.
    * Input: (string) The name of the connection to be closed.
    * Output: (unit)
    *)

     fun closeConnection (conn_name) = 

      (* Search for the connection. *)
      case search(conn_name) of

      (* If the connection was found .... *)
      SOME conn =>
        let 

          (* Local function to remove the information about this connection. *)
          fun remove([]) =

            (* If we have no more connections to search, raise exception. *)
            raise ElementMissingExn("Failure closing connection: Connection not found.")
            
            | remove((conn:ext_conn_id as {name = name, ...})::conns) = 

            (* If this is the connection to remove .... *)
            if (name = conn_name)
            then

              (* .... then return the remaining unsearched connections .... *)
              conns
            else

              (* .... else return this connection and search the rest. *)
              conn::remove(conns)
        in

          (* Close the external connection. *)
          CommsLayer.disconnect(#channel conn);

          (* Remove the stored information about this socket. *)
          connections:=remove(!connections)
        end

      (* If the connection was not found, raise exception. *)
      | NONE =>
        raise ElementMissingExn("Failure closing connection: Connection not found.")

  (* End of structure. *)
  end
