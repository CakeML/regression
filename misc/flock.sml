(*
  Maintenance tool: acquire the lock that is used by the server to run a
  command without interfering with the server.
*)
use "lib/serverLib.sml";
open serverLib
open Posix.Process

fun assert b s =
  if b then () else
  (TextIO.output(TextIO.stdErr,s);
   OS.Process.exit OS.Process.failure)

fun main () =
  let
    val args = CommandLine.arguments ()
    val () = assert (not (List.null args)) "usage: ./flock cmd args ...\n"
    val fd = acquire_lock ()
  in
    case fork() of
      NONE => execp (List.hd args, args)
    | SOME pid =>
      let
        val (pid',status) = wait ()
        val () = assert (pid=pid') "wrong child\n"
        val () = Posix.IO.close fd
      in
        case status of
          W_EXITED => ()
        | W_EXITSTATUS w => exit w
        | _ => exit (Word8.fromInt 126)
      end
  end
