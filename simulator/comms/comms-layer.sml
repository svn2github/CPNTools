(*******************
 * comms-layer.sml *
 *******************
 *
 * Based on the following two files:
 *
 *********************
 * 1. sock-util.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *********************
 * 2. puretcpserver.sml
 *
 * Authors: Stephane Nimsgern and Francois Vernet
 * Date: May 28th, 2000
 *********************
 *
 * Created/Modified: January 2001 by Guy Edward Gallasch
 *
 * Description: Various utility functions for programming with TCP sockets.  
 *
 * Current version: 1.2
 *
 * ChangeLog:
 * 
 *   Version 1.1 - 15th July 2001:
 *    - renamed communication_device to channel
 *    - renamed functions to better reflect their purpose
 *    - renamed file from new-sock-util.sml to comms-layer.sml
 *    - renamed structure from NewSockUtil to CommsLayer
 *    - renamed signature from NEW_SOCK_UTIL to COMMS_LAYER
 *
 *   Version 1.01 - 19th February 2001: 
 *    - Modified serverConnect to close the server socket.
 *
 *)

structure CommsLayer : COMMS_LAYER =
  struct

    (* A host can be identified by name or address *)
    datatype hostname = HostName of string | HostAddr of NetHostDB.in_addr

    (* A port can be identified by number, or by the name of a service *)
    datatype port = PortNumber of int | ServName of string

    (* A server socket to listen for incoming connection requests. *)
    type 'a stream_sock_passive = ('a, Socket.passive Socket.stream) Socket.sock

    (* A socket for process communication. *)
    type 'a stream_sock_active = ('a, Socket.active Socket.stream) Socket.sock

    (* The communication channel, sockets in this implementation. *)
    type channel = INetSock.inet stream_sock_active; 
       
    (* Exception to be raised in the case that the address is invalid *)
    exception BadAddr of string

   (************************
    * FUNCTION resolveAddr *
    ************************
    * Description:  Given a hostname and port, resolve them in the host
    *   and service database.  If either the host or service name is not
    *   found, then BadAddr is raised. 
    *
    * Precondition: There is a need to resolve the address of an external
    *   process.
    * Postcondition: If the host and port are valid, the address is
    *   resolved.
    * 
    * Input: (hostname:string) The name of the host to be resolved.
    *        (portcom:int) The port number (service) to resolve.
    * Output: (host:string) The host name.
    *         (addr:NetHostDB.in_addr) The host address.
    *         (port:int option) The port number (optional).
    *)

    fun resolveAddr {hostname, portcom} = 
      let
 
        (* Identify host name and port. *) 
        val (host,port) = (HostName hostname, SOME(PortNumber portcom));

        (* Function to raise the bad address exception. *)
        fun err (a, b) = 
          raise BadAddr(String.concat[a, " \"", b, "\" not found"])

        (* Resolve the host and port. *)
        val (name, addr) =      
        (
          case host of 

          (* If host is given as a name, then find the address. *) 
          HostName s => 
          (
            case NetHostDB.getByName s of 

            (* If host name not found, signal error/raise exception. *)
            NONE => err ("hostname", s)

            (* If host name found, then return the address. *)
            | (SOME entry) => (s, NetHostDB.addr entry)
          )

          (* If host is given as an address then find the name. *)
          | HostAddr addr => 
          (
            case NetHostDB.getByAddr addr of 

              (* If host address invalid, signal error/raise exception. *)
              NONE => err ("host address", NetHostDB.toString addr)

              (* If host address found, return the host name. *)
              | (SOME entry) => (NetHostDB.name entry, addr)
          )
        )

        val port = 
        (
          case port of 

          (* If port is given as a number, no need to look it up. *)
          (SOME(PortNumber n)) => SOME n

          (* If port is given as a service, then find the port number. *)
          | (SOME(ServName s)) => 
          (
            case NetServDB.getByName(s, NONE) of 

            (* If service valid, then return the port number. *)
            (SOME entry) => SOME(NetServDB.port entry)

            (* If service invalid, then signal error/raise exception. *)
            | NONE => err("service", s)
          )

          (* If port not given at all, don't look anything up. *)
          | NONE => NONE
        )
      in

        (* Return host name, address, port number. *) 
        {host = name, addr = addr, port = port}
      end;


   (****************************
    * FUNCTION connectINetStrm *
    ****************************
    * Description: Establish a client-side connection to an INET domain 
    *   stream socket.
    *
    * Precondition: A connection to the specified host and port is required.
    * Postcondition: A socket has been opened to the specified host and port.
    *
    * Input: (addr:NetHostDB.in_addr) The address of the host.
    *        (port:int) The port number of the host process.
    * Output: (INetSock.inet stream_sock_active) The opened socket.
    *)

    fun connectINetStrm {addr, port} = 
      let

        (* Create a TCP socket. *)
        val sock = INetSock.TCP.socket (): INetSock.inet stream_sock_active
      in

        (* Connect the TCP socket. *)
        Socket.connect (sock, INetSock.toAddr(addr, port));

        (* Return the opened socket. *)
        sock
      end



   (********************
    * FUNCTION connect *
    ********************
    * Description: Create and open a TCP socket to the specified host name 
    *   and port number.
    *
    * Precondition: A connection to the specified server is to be created.
    * Postcondition: An connection to the server has been created.
    *
    * Input: (string) The host name of the server.
    *        (int) The port number listened to by the server.
    * Output: (InetSock.inet stream_sock_active) The opened TCP socket.
    *)

    fun connect (host, portcom) = 
      let

        (* Resolve the address and port of the external host. *)
        val {addr,host,port} = resolveAddr{hostname=host,portcom=portcom};

        (* Create and open a TCP socket to the external host. *)
        val socka = connectINetStrm{addr=addr, port=portcom};
      in

        (* Return the opened socket. *)
        socka
      end

   (*************************
    * FUNCTION bindINetStrm *
    *************************
    * Description: Establish a server-side socket and listen for incoming
    *   connections.
    *
    * Precondition: The need to listen for incoming sockets has arisen.
    * Postcondition: A server socket has been opened to listen for
    *   incoming connections on the specified port.
    * 
    * Input: (addr:NetHostDB.in_addr) The address of the host.
    *        (port:int) The port number to listen to.
    * Output: (INetSock.inet stream_sock_passive) The passive server
    *           socket
    *)

    fun bindINetStrm {addr, port} = 
      let
        (* Create a server socket. *)
        val sock = INetSock.TCP.socket () : INetSock.inet stream_sock_passive
      in

        (* Connect the server socket. *)
        Socket.bind (sock, INetSock.toAddr(addr, port));


        (* Set the queue limit to 5 for incoming connection requests. *)
        (Socket.listen (sock, 5));


        (* Return the opened socket. *)
        sock
      end



   (*******************
    * FUNCTION accept *
    *******************
    * Description: Listen for an incoming connection, and, when detected,  
    *   create a TCP socket for this connection.
    *
    * Precondition: A connection attempt by an external client is expected.
    * Postcondition: A TCP connection has been created.
    *
    * Input: (string) The host name of the server (this process). [no longer used]
    *        (int) The port number to lsiten to for connection requests.
    * Output: (InetSock.inet stream_sock_active) The opened TCP socket.
    *)

    fun accept (host, portcom) = 
      let
        val sock = INetSock.TCP.socket ()
        val _ = Socket.bind (sock, INetSock.any portcom)
        val _ = Socket.listen (sock, 5)
        (* Local function to accept a connection request. *)
        fun acceptRequest(sock) = 
          let 

            (* Accept the incoming connection. *)
            val (sockb,addr_of_client) = Socket.accept (sock)
          in

            (* Close the server socket and return the opened TCP socket. *)
            Socket.close(sock); 
            sockb
          end
      in

        (* Listen for and accept a connection request from a client. *)
        acceptRequest(sock)
      end


   (*****************
    * FUNCTION send *
    *****************
    * Description: Send the complete contents of a vector of bytes. 
    *
    * Precondition: A socket has been opened and information is to be
    *   sent to it.
    * Postcondition: A vector of bytes has been written to the socket.
    *
    * Input: ('a stream_sock_active) The stream to write to.
    *        (Word8Vector.vector) A vector of bytes.
    * Output: (unit)
    *)

    fun send (sock, vec) = 
      let

        (* The length of the vector (the number of bytes to send). *)
        val len = Word8Vector.length vec

        (* Local function to send bytes to the socket. *)
        fun sendBytes i = 
          Socket.sendVec (sock, Word8VectorSlice.slice (vec, i, NONE))

        (* Local function to send the vector of bytes to the socket. *)
        fun put i = 

          (* If not all has been sent then send some more .... *)
          if (i < len)
          then put(i + sendBytes i)

          (* .... else return unit. *)
          else ()
      in

        (* Start at the beginning of the vector. *)
        put 0
      end

   (* check if input is available on a given socket *)
      fun canreceive (socket) = 
          Socket.Ctl.getNREAD socket > 0

   (********************
    * FUNCTION receive *
    ********************
    * Description: Read exactly n bytes from a stream socket. 
    *
    * Precondition: A socket has been opened.
    * Postcondition: Exactly n bytes has been read from a socket.
    *
    * Input: ('a stream_sock_active) The stream to read from.
    *        (int) The number of bytes to read from the stream.
    * Output: (Word8Vector.vector) A vector of bytes.
    *)

    fun receive (sock, n) = 
      let
        fun get (0, data) = 

          (* All bytes read, so concatenate them into a vector. *)
          Word8Vector.concat(rev data)

	  | get (n, data) = 
          let

            (* Read n bytes from the socket. *)
            val v = Socket.recvVec (sock, n)
          in

            (* If no bytes were read, raise an exception .... *)
            if (Word8Vector.length v = 0)
            then raise OS.SysErr("closed socket", NONE)

            (* .... else read any remaining bytes. *)
            else get (n - Word8Vector.length v, v::data)
          end
      in

        (* If the number of bytes to read is negative, raise exception .... *)
        if (n < 0)
        then raise Size 

        (* .... else get the n bytes. *)
        else get (n, [])
      end

      fun receive' (sock, n) =
          Socket.recvVec (sock, n)


   (***********************
    * FUNCTION disconnect *
    ***********************
    * Description: Close a previously opened TCP socket.
    *
    * Precondition: An external connection is to be closed.
    * Postcondition: The TCP socket used in the external connection has
    *   been closed.
    * 
    * Input: (InetSock.inet stream_sock_active) An open client TCP socket.
    * Output: (unit)
    *)

    fun disconnect (socket) = 

      (* Close the socket. *)
      Socket.close(socket)

  (* End of structure. *)
  end 
