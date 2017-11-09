(*
Implements the server-side regression-test API as a CGI program.

The API is for workers to view and manipulate the job queues.
It also provides a hook for refreshing the queues:
  If there are new jobs on GitHub, they will be added as waiting.
  If there are stale jobs, they will be removed.

each job is on exactly one list: waiting, running, stopped
if a job changes list, it can only move to the right

Refreshing the queues:

  Behaviours:

    1. add a job to the waiting list:
      - create a new job id
      - the commits for the job satisfy the following:
        - they are the current commits (on GitHub) for a particular target
        - there are no other jobs with the same commits

    2. remove a job from the waiting list:
      - if it does not have the current commits (on GitHub) for any target, or
      - if there are other running or waiting jobs for the same commits
          (this should never happen -- not checking it currently)
          (we don't count duplicate stopped jobs since
           they might have been retried)
      - the removed job's id number could be re-used (but is that worthwhile?)
      - the race with workers trying to obtain this job is handled by the global
        lock on queues. either the job is claimed before it can be removed, or
        removed before it can be claimed.

    3. move a job from running to stopped:
      - if the time since it started is too long
      - adds a note, "timed out", to the output
      - does the stop API actions

  Targets:

    CakeML:
      - branch "master"
      - each open, mergeable pull request
        in this case, there are two commits:
          - the pull request commit
          - the master commit to merge into
        but they move together (as captured by the state of the PR on GitHub)
    HOL:
      - branch "master"
*)
use "serverLib.sml";

open serverLib

fun job id =
  let
    val f = Int.toString id
    val q = queue_of_job f
  in
    OS.Path.concat(q,f)
  end

fun claim id name =
  let
    val f = Int.toString id
    val old = OS.Path.concat("waiting",f)
    val new = OS.Path.concat("running",f)
    val () =
      if OS.FileSys.access(old,[OS.FileSys.A_READ]) then
        if OS.FileSys.access(new,[OS.FileSys.A_READ]) then
          cgi_die ["job ",f, " is both waiting and running"]
        else OS.FileSys.rename{old = old, new = new}
      else cgi_die ["job ",f," is not waiting to be claimed"]
    val out = TextIO.openAppend new
  in
    print_claimed out (name,Date.fromTimeUniv(Time.now())) before TextIO.closeOut out
  end

fun append id line =
  let
    val f = Int.toString id
    val p = OS.Path.concat("running",f)
    val out = TextIO.openAppend p handle e as IO.Io _ => (cgi_die ["job ",f," is not running: cannot append"]; raise e)
  in
    print_log_entry out (Date.fromTimeUniv(Time.now()),line) before TextIO.closeOut out
  end

fun log id data =
  let
    val f = Int.toString id
    val p = OS.Path.concat("running",f)
    val out = TextIO.openAppend p handle e as IO.Io _ => (cgi_die ["job ",f," is not running: cannot log"]; raise e)
  in
    TextIO.output(out,data) before TextIO.closeOut out
  end

fun stop id =
  let
    val f = Int.toString id
    val old = OS.Path.concat("running",f)
    val new = OS.Path.concat("stopped",f)
    val () =
      if OS.FileSys.access(old,[OS.FileSys.A_READ]) then
        if OS.FileSys.access(new,[OS.FileSys.A_READ]) then
          cgi_die ["job ",f, " is both running and stopped"]
        else OS.FileSys.rename{old = old, new = new}
      else cgi_die ["job ",f," is not running: cannot stop"]
  in
    () (* TODO: send email *)
  end

fun retry id =
  let
    val f = Int.toString id
    val old = OS.Path.concat("stopped",f)
    val () = cgi_assert (OS.FileSys.access(old,[OS.FileSys.A_READ])) ["job ",f," is not stopped: cannot retry"]
    val id = first_unused_id (waiting()@running()@stopped()) 1
    val new = OS.Path.concat("waiting",Int.toString id)
    val inp = TextIO.openIn old
    val out = TextIO.openOut new
    fun loop last =
      case TextIO.inputLine inp of NONE => cgi_die ["stopped job ",f," has invalid file format"]
      | SOME line => (TextIO.output(out,line);
                      if last then () else
                      loop (String.isPrefix "HOL: " line))
    val () = loop false
    val () = TextIO.closeOut out
    val () = TextIO.closeIn inp
  in id end

fun refresh () =
  let
    val snapshots = get_current_snapshots ()
    val () = clear_list "waiting"
    (* TODO: stop timed out jobs *)
    val running_ids = running()
    val stopped_ids = stopped()
    val snapshots = filter_out "running" running_ids snapshots
    val snapshots = filter_out "stopped" stopped_ids snapshots
    val avoid_ids = running_ids @ stopped_ids
    val () = if List.null snapshots then ()
             else ignore (List.foldl (add_waiting avoid_ids) 1 snapshots)
  in () end

datatype request_api = Get of api | Post of id * string

fun get_api () =
  case (OS.Process.getEnv "PATH_INFO",
        OS.Process.getEnv "REQUEST_METHOD") of
    (SOME path_info, SOME "GET")
      => Option.map Get (api_from_string path_info (OS.Process.getEnv "QUERY_STRING"))
  | (SOME path_info, SOME "POST")
      => (case String.tokens (equal #"/") path_info of
            ["log",n] =>
              (Option.mapPartial
                (fn len =>
                  Option.compose
                    ((fn id => Post(id,TextIO.inputN(TextIO.stdIn,len))),
                     id_from_string) n)
                (Option.composePartial(Int.fromString,OS.Process.getEnv) "CONTENT_LENGTH"))
          | _ => NONE)
  | _ => NONE

local
  fun id_list ids = String.concatWith " " (List.map Int.toString ids)
in
  fun dispatch api =
    text_response (
      case api of
        Waiting => id_list (waiting())
      | Running => id_list (running())
      | Stopped => id_list (stopped())
      | Refresh => (refresh (); refresh_response)
      | Job id => file_to_string (job id)
      | Claim(id,name) => (claim id name; claim_response)
      | Append(id,line) => (append id line; append_response)
      | Stop id => (stop id; stop_response)
      | Retry id => String.concat["retried as job ",Int.toString(retry id),"\n"]
    ) handle e => cgi_die [exnMessage e]
end

fun dispatch_log id data =
  text_response (log id data; log_response)
  handle e => cgi_die [exnMessage e]

fun dispatch_req (Get api) = dispatch api
  | dispatch_req (Post (id,data)) = dispatch_log id data

fun main () =
  let
    val () = ensure_queue_dirs ()
  in
    case get_api () of
      NONE => cgi_die ["bad usage"]
    | SOME req =>
      let
        val fd = acquire_lock ()
      in
        dispatch_req req before
        Posix.IO.close fd
      end
  end
