use "serverLib.sml";

open serverLib

fun main () =
  let
    val _ = case Posix.Process.fork () of SOME _ => OS.Process.exit OS.Process.success | NONE => Posix.ProcEnv.setsid ()
    val () =
     (case CommandLine.arguments() of
        [time,outfile] =>
          (OS.Process.sleep (Time.fromSeconds (Option.getOpt (LargeInt.fromString time, 0)));
           TextIO.setOutstream(TextIO.stdOut, TextIO.getOutstream (TextIO.openAppend outfile)))
      | _ => ())
    val () = TextIO.output(TextIO.stdOut, "hello from refresh\n")
    val snapshots = get_current_snapshots ()
    val fd = acquire_lock ()
    val () = clear_list "waiting"
    (* TODO: stop timed out jobs *)
    val running_ids = running()
    val stopped_ids = stopped()
    val snapshots = filter_out (same_head "running") running_ids snapshots
    val snapshots = filter_out (same_snapshot "stopped") stopped_ids snapshots
    val avoid_ids = running_ids @ stopped_ids @ aborted()
    val () = if List.null snapshots then ()
             else ignore (List.foldl (add_waiting avoid_ids) 1 snapshots)
  in Posix.IO.close fd end
