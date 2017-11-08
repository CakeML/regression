(*
  Acquire the lock that is used by the server to run a command without
  interfering with the server
*)
use "regressionLib.sml";
open regressionLib
open Posix.Process
fun main () =
  let
    val args = CommandLine.arguments ()
    val () = assert (not (List.null args)) ["usage: ./flock cmd args ..."]
    val fd = acquire_lock ()
  in
    case fork() of
      NONE => execp (List.hd args, args)
    | SOME pid =>
      let
        val (pid',status) = wait ()
        val () = assert (pid=pid') ["wrong child"]
        val () = Posix.IO.close fd
      in
        case status of
          W_EXITED => ()
        | W_EXITSTATUS w => exit w
        | _ => exit (Word8.fromInt 126)
      end
  end
