(*
  Outdated maintenance tool: acquire the lock that used to be used by the
  server, hoping to run a command without interfering with the server.

  With the new threaded server, this tool no longer makes sense.
  The lock is internal to the server process, so cannot be acquired externally.
*)
use "serverLib.sml";
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
    val () = acquire_lock ()
  in
    case fork() of
      NONE => execp (List.hd args, args)
    | SOME pid =>
      let
        val (pid',status) = wait ()
        val () = assert (pid=pid') "wrong child\n"
        val () = release_lock ()
      in
        case status of
          W_EXITED => ()
        | W_EXITSTATUS w => exit w
        | _ => exit (Word8.fromInt 126)
      end
  end
