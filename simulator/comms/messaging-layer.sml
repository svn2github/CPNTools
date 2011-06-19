(*********************** 
 * messaging-layer.sml *
 ***********************
 * 
 * Created: July 15th 2001 by Guy Edward Gallasch
 * 
 * Description: Functions used to packetise data into messages to send, and 
 * depacketise received data messages.  Independent of the type of data sent 
 * or received.
 * 
 *)


structure MessagingLayer : MESSAGING_LAYER =
  struct

    (* The communication channel, from the CommsLayer implementation. *)
    type channel = CommsLayer.channel;

    (* Exception to be raised if a received header is invalid. *)
    exception InvalidDataExn of string


   (*****************
    * FUNCTION send *
    *****************
    * Description: Given a vector of bytes and a communication channel, 
    *   segment the vector of bytes into 128 byte packets, 1 byte header 
    *   and 127 bytes payload, and pass it to the send function in the 
    *   CommsLayer layer.  The header indicates the length of the packet 
    *   and if more packets to are to follow.
    *
    * Precondition: A connection has been opened to an external process.
    * Postcondition: Data has been sent to this external process.
    * 
    * Input: (CommsLayer.channel) The communication channel to send the data to.
    *        (to_send) The data being sent represented as a vector of  bytes.
    * Output: (unit)
    *)

    fun send (conn, to_send) =

      let

        (* Local function to recursively segment the data. *)
        fun segmentAndSend (byte_stream) = 
          let

            (* Number of bytes left to segment. *)
            val length = Word8Vector.length(byte_stream)
          in

            (* If more than 127 still to segment .... *)
            if (length > 127) 
            then
            (
              let 

                (* .... then generate appropriate header .... *)
                val header = Word8VectorSlice.full
                (Word8Vector.fromList(Word8.fromInt(255)::[]))

                (* .... get the next 127 bytes of data .... *)
                val payload = Word8VectorSlice.slice (byte_stream,0,SOME(127))

                (* .... create a packet .... *)
                val packet = Word8VectorSlice.concat(header::(payload::[]));
              in

                (* .... and send the packet to the external process. *)
                CommsLayer.send(conn, packet);

                (* Segment the remaining data. *)
                segmentAndSend(Word8VectorSlice.vector(Word8VectorSlice.slice(byte_stream,127,NONE)))
              end
            )

            (* If 127 or less bytes of data left to segment .... *)
            else
            (
              let 

                (* .... then generate the header .... *) 
                val header = Word8Vector.fromList(Word8.fromInt(length)::[]);

                (* .... create a packet .... *)
                val packet = Word8Vector.concat(header::(byte_stream::[]));
              in

                (* ... and send it to the external process. *)
                CommsLayer.send(conn, packet)

              end
            )
          end     
      in 

        (* Send the vector of bytes. *)
        segmentAndSend(to_send)
      end


   (********************
    * FUNCTION receive *
    ********************
    * Description: Receive data from an external process.  Headers are
    *   read, interpreted and stripped from the payload data accordingly.  
    * 
    * Precondition: A connection has been opened to an external process.
    * Postcondition: Data has been received from an external process.
    *
    * Input: (CommsLayer.channel) The communication channel to receive data from.
    * Output: (Word8Vector.vector) The data received as a vector of bytes 
    *   without headers.
    *)

    fun receive (conn) =

      let

        (* A place to store the payload data as headers are stripped. *)
        val byte_stream:Word8Vector.vector list ref = ref ([]);

        (* Local function to reassemble the payload data. *)
        fun stripHeaders() =
          let

            (* Storage for the header (interpreted as an unsigned integer). *)
            val header = Word8.toInt(Word8Vector.sub(CommsLayer.receive(conn,1),0))
          in

            (* If more packets to follow in the sequence .... *)
            if (header > 127) 
            then
            (

              (* .... then extract payload and get the next packet. *)
              byte_stream:= List.@ ((!byte_stream), (CommsLayer.receive(conn,127)::[])); 
              stripHeaders()
            )

            (* If this is the last packet in the sequence .... *)
            else if (header >= 0)
            then
            (

              (* .... then extract the payload and apply the decode function. *)
              byte_stream:= List.@ ((!byte_stream), (CommsLayer.receive(conn,header)::[]));
              Word8Vector.concat(!byte_stream)
            )

            (* If the header is negative (i.e. invalid), raise exception. *)
            else 
              raise InvalidDataExn 
                ("Failure receiving data: Cannot receive a negative number of bytes.")            
          end

      in

        (* Begin the process. *)
        stripHeaders()
      end

(*      fun select (conns) =
      let
          fun to_poll con = (OS.IO.pollIn(Socket.pollDesc con), con)

          val with_info = List.map to_poll conns

          val info_list = OS.IO.poll(List.map #1 with_info, NONE)

          val desc_list = List.map OS.IO.infoToPollDesc info_list

          val ready = List.filter (fn (desc, con) => (List.exists (fn info =>
      info = desc) desc_list)) with_info

          val result = List.map (fn (desc, con) => (con, receive con)) ready
      in
          result
      end*)


  (* End of structure. *)
  end

