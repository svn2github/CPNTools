(************************************************************************)
(* CPN Tools Simulator (Simulator/CPN)                                  *)
(* Copyright 2012 AIS Group, Eindhoven University of Technology         *)
(* All rights reserved.                                                 *)
(*                                                                      *)
(* This file is part of the CPN Tools Simulator (Simulator/CPN).        *)
(*                                                                      *)
(* You can choose among two licenses for this code, either the GNU      *)
(* General Public License version 2 or the below 4-clause BSD License.  *)
(*                                                                      *)
(************************************************************************)
(* GNU General Public License for CPN Tools Simulator (Simulator/CPN)   *)
(*                                                                      *)
(* CPN Tools is free software: you can redistribute it and/or modify    *)
(* it under the terms of the GNU General Public License as published by *)
(* the Free Software Foundation, either version 2 of the License, or    *)
(* (at your option) any later version.                                  *)
(*                                                                      *)
(* CPN Tools is distributed in the hope that it will be useful,         *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of       *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        *)
(* GNU General Public License for more details.                         *)
(*                                                                      *)
(* You should have received a copy of the GNU General Public License    *)
(* along with CPN Tools.  If not, see <http://www.gnu.org/licenses/>.   *)
(************************************************************************)
(* 4-clause BSD License for CPN Tools Simulator (Simulator/CPN)         *)
(*                                                                      *)
(* Redistribution and use in source and binary forms, with or without   *)
(* modification, are permitted provided that the following conditions   *)
(* are met:                                                             *)
(*                                                                      *)
(* 1. Redistributions of source code must retain the above copyright    *)
(* notice, this list of conditions and the following disclaimer.        *)
(* 2. Redistributions in binary form must reproduce the above copyright *)
(* notice, this list of conditions and the following disclaimer in the  *)
(* documentation and/or other materials provided with the distribution. *)
(* 3. All advertising materials mentioning features or use of this      *)
(* software must display the following acknowledgement: “This product   *)
(* includes software developed by the AIS Group, Eindhoven University   *)
(* of Technology and the CPN Group, Aarhus University.”                 *)
(* 4. Neither the name of the AIS Group, Eindhoven University of        *)
(* Technology, the name of Eindhoven University of Technology, nor the  *)
(* names of its contributors may be used to endorse or promote products *)
(* derived from this software without specific prior written permission.*)
(*                                                                      *)
(* THIS SOFTWARE IS PROVIDED BY THE AIS GROUP, EINDHOVEN UNIVERSITY OF  *)
(* TECHNOLOGY AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,   *)
(* BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND    *)
(* FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL   *)
(* THE AIS GROUP, EINDHOVEN UNIVERSITY OF TECHNOLOGY BE LIABLE FOR ANY  *)
(* DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL   *)
(* DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE    *)
(* GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS        *)
(* INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER *)
(* IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR      *)
(* OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN  *)
(*IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                         *)
(************************************************************************)
(* File: extension.sml
 *
 * Simulator extensions
 *)

import "extension.sig";

functor Extension(structure Stream: STREAM structure Err : GRAMERROR) : EXTENSION = struct
    local
        type socket = (INetSock.inet, Socket.active Socket.stream) Socket.sock

        structure BIS = BIS(structure Stream = Stream structure Err = Err)
        open BIS

        val host = ref (NetHostDB.addr (Option.valOf (NetHostDB.getByName "127.0.0.1")))
        val port = ref 1985
        val unreachable = ref false
        val socket = ref (NONE : socket option)
        val input = ref (NONE : Stream.instream option)
        val output = ref (NONE : Stream.outstream option)
        val subscriptions = ref ([] : (int * int list ref) list)

        fun disconnect () =
        let
            val _ = 
                if (!socket <> NONE)
                then Socket.close (Option.valOf (!socket))
                else ()
            val _ = socket := NONE
        in
            ()
        end

        exception Unconnected
        exception NetworkError of string

        fun handshake socket =
        (let
            val iodesc = Socket.ioDesc socket
            val ii = Stream.makeIn iodesc
            val oo = Stream.makeOut iodesc
            val _ = input := (SOME ii)
            val _ = output := (SOME oo)

            val _ = Stream.putInteger oo 1
            val _ = Stream.putInteger oo 0
            val _ = Stream.flush oo

            val opcode = Stream.getInteger ii
            val length = Stream.getInteger ii
        in
            if (opcode <> 1) orelse (length <> 0)
            then raise NetworkError "Invalid handshake from extension server"
            else ()
        end
        handle exn =>
        (input := NONE; output := NONE; raise exn))

        fun attempt 0 =
        let
            val _ = unreachable := true
        in
            ()
        end
          | attempt n =
          let
              val sock = INetSock.TCP.socket (): socket
              fun setup sock = 
              let
                  val _ = Socket.Ctl.setKEEPALIVE (sock, true) handle _ =>
                  ()(*Err.log "Failed setting KEEPALIVE")*)
                  val _ = Socket.Ctl.setDEBUG (sock, true) handle _ =>
                  ()(*Err.log "Failed setting DEBUG")*)
(*                  val _ = Socket.Ctl.setLINGER (sock, NONE) handle _ => 
                  (Err.log "Failed setting LINGER")
                  val _ = Socket.Ctl.setSNDBUF (sock, 0) handle _ =>
                  (Err.log "Failed setting SNDBUF")
                  val _ = Socket.Ctl.setRCVBUF (sock, 0) handle _ =>
                  (Err.log "Failed setting RCVBUF")*)
                  val _ = INetSock.TCP.setNODELAY(sock, true) handle _ =>
                  ()(*Err.log "Failed setting NODELAY")*)
              in
                  ()
              end
          in
             if (!socket = NONE)
             then 
                 (
                  (Socket.connect (sock, INetSock.toAddr (!host, !port));
                  setup sock;
                  handshake sock;
                  socket := (SOME sock))
                  handle exn => (Socket.close sock; raise exn))
             else ()
          end
          handle _ =>
          let
              val _ = OS.Process.sleep (Time.fromSeconds 1)
          in
              attempt (n - 1)
          end

        fun connect () =
            if (!socket = NONE)
            then
                if (!unreachable)
                then ()
                else 
                    let
                        val _ = attempt 5
                    in
                        ()
                    end
            else ()
    in
        structure Stream = Stream
        type BIS = (bool list) * (int list) * (string list)

        exception NoExtensionServer

        fun configure (newhost, newport) =
        (let
            val newhost' = NetHostDB.getByName newhost
            val _ = if Option.isSome newhost'
                    then ()
                    else (unreachable := true;
                          raise NetworkError "Host not found")
            val _ = host := (NetHostDB.addr (Option.valOf newhost'))
            val _ = port := newport
            val _ = disconnect ()
            val _ = unreachable := false
        in
            ()
        end
        handle exn => (unreachable := true; raise exn))

        fun getStream () =
            case (!input)
              of NONE => []
               | (SOME (str)) => [str]

        fun getStreams () = (Option.valOf (!input), Option.valOf (!output))

        fun getSubscription cmd =
            List.find (fn (c, v) => c = cmd) (!subscriptions)

        fun addSubscription cmd subcmd =
            case (getSubscription cmd)
               of NONE =>
                   if subcmd < 0
                   then subscriptions := (cmd, ref [])::(!subscriptions)
                   else subscriptions := (cmd, ref [subcmd])::(!subscriptions)
               | SOME (_, v) =>
                       if !v = []
                       then ()
                       else if subcmd < 0
                            then v := []
                            else if (List.exists (fn c => c = subcmd) (!v))
                                 then ()
                                 else v := subcmd::(!v)

        fun testSubscriptions () = !subscriptions

        fun addSubscriptions [] = ()
          | addSubscriptions (cmd::subcmd::rest) =
            (addSubscription cmd subcmd; addSubscriptions rest)

        fun dispatchLocally (b, 2::count::i, s) =
            if List.length i = 2 * count
            then (addSubscriptions i; ([], [1], []))
            else ([], [~1], [])
          | dispatchLocally _ = ([], [~1], ["Unknown command"])

        exception Local of bool list * int list * string list
        fun forward received param opcode (b, i, s) =
        let
            val _ =
                if (opcode = 9) andalso (List.length i >= 2) andalso
                   (List.hd i = 10000) andalso (List.hd (List.tl i) < 100)
                then raise Local (dispatchLocally (b, List.tl i, s))
                else connect ()
        in
            if (!socket = NONE)
            then raise NoExtensionServer
            else
                let
                    val _ = send_result opcode (Option.valOf (!output)) (b, i, s)
                    val result = received param (Option.valOf (!input))
                in
                    result
                end
        end handle (Local bis) => bis

        fun forward' received param (b, i, s) (b', i', s') =
            forward received param 12
            (List.@ (b, b'),
             (List.length b)::(List.length i)::(List.length s)::
             (List.length b')::(List.length i')::(List.length s')::
             (List.@ (i, i')),
             List.@ (s, s'))

        fun watched received param ((b, [], s), response) = response
          | watched received param ((b, [cmd], s), response) =
          (case (getSubscription cmd)
            of NONE => response
             | SOME (_, v) => 
                     if (!v) = []
                     then forward' received param (b, [cmd], s) response
                     else response)
          | watched received param ((b, cmd::subcmd::i, s), response) =
            (case (getSubscription cmd)
            of NONE => response
             | SOME (_, v) => 
                     if (!v) = []
                     then forward' received param (b, cmd::subcmd::i, s) response
                     else if (List.exists (fn c => c = subcmd) (!v))
                          then forward' received param (b, cmd::subcmd::i, s) response
                          else response)
    end
end
