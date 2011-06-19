(*******************************
 * encode-decode-functions.sml *
 *******************************
 *
 * Created/Modified: January 2001 by Guy Edward Gallasch
 *
 * Description: Functions to encode and decode common data types to and 
 *              from sequences of bytes.
 *
 *)


(* Exception to be raised in the case that the received data is 
   not of the form expected. *)
exception InvalidDataExn of string

structure EncodeDecodeFunctions =
struct
(* Encoding and decoding functions for strings and integers. *)
fun stringEncode (to_send) = 
  Byte.stringToBytes(to_send);

fun stringDecode (received) = 
  Byte.bytesToString(received);


fun integerEncode (to_send) = 
  Byte.stringToBytes(Int.toString(to_send))

fun integerDecode (received) = 
  case Int.fromString(Byte.bytesToString(received)) of
  SOME received =>
  (  print(Int.toString(received));
    received
  )
  | NONE => 
    raise InvalidDataExn("Failure receiving data: Integer expected");

end
