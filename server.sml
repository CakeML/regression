(*
Implements the server-side regression-test API as a CGI program.

The API is for workers to view and manipulate the job queues.
It also provides a hook for refreshing the queues:
  If there are new jobs on GitHub, they will be added as waiting.
  If there are stale jobs, they will be removed.

Each job is on exactly one list: waiting, running, finished, aborted.
If a job changes list, it can only move to the right.

The waiting queue is refreshed as follows:

  1. Clear the waiting jobs list
  2. Get the current snapshots from GitHub (see below)
  3. Filter out from the snapshots:
    - any with the same CakeML head commit as a running job
    - any with the same snapshot (head+base+HOL commits) as a finished job
  4. Add the remaining snapshots to the waiting jobs list

The running queue may be refreshed by removing timed out jobs. (Not implemented yet.)

Current snapshots from GitHub:

    CakeML:
      - branch "master"
      - each open, mergeable pull request
        in this case, there are two commits:
          - the pull request commit (head)
          - the master commit to merge into (base)
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

fun claim (id,name) =
  let
    val f = Int.toString id
    val old = OS.Path.concat("waiting",f)
    val new = OS.Path.concat("running",f)
    val fd = acquire_lock ()
    val () =
      if OS.FileSys.access(new,[OS.FileSys.A_READ]) then
        cgi_die 500 ["job ",f, " is both waiting and running"]
      else OS.FileSys.rename{old = old, new = new}
    val out = TextIO.openAppend new
    val () = print_claimed out (name,Date.fromTimeUniv(Time.now()))
    val () = TextIO.closeOut out
    val inp = TextIO.openIn new
    val sha = get_head_sha (read_bare_snapshot inp)
              handle Option => cgi_die 500 ["job ",f," has invalid file format"]
  in
    TextIO.closeIn inp;
    Posix.IO.close fd;
    GitHub.set_status f sha Pending
  end

fun append (id,line) =
  let
    val f = Int.toString id
    val p = OS.Path.concat("running",f)
    val fd = acquire_lock ()
    val out = TextIO.openAppend p handle e as IO.Io _ => (cgi_die 409 ["job ",f," is not running: cannot append"]; raise e)
  in
    print_log_entry out (Date.fromTimeUniv(Time.now()),line);
    TextIO.closeOut out;
    Posix.IO.close fd
  end

fun log (id,_,len) =
  let
    val f = Int.toString id
    val p = OS.Path.concat("running",f)
    val fd = acquire_lock ()
    val out = TextIO.openAppend p
              handle e as IO.Io _ => (cgi_die 409 ["job ",f," is not running: cannot log"]; raise e)
  in
    outputN_from_stdIn(out, len);
    TextIO.closeOut out;
    Posix.IO.close fd
  end

fun upload (id,name,len) =
  let
    val jid = Int.toString id
    val dir = OS.Path.concat(artefacts_dir,jid)
    val () = if OS.FileSys.access(dir,[]) then () else OS.FileSys.mkDir dir
    val () = cgi_assert (OS.FileSys.isDir dir) 500 [dir," exists and is not a directory"]
    val f = OS.Path.concat(dir,name)
    val () = cgi_assert (not(OS.FileSys.access(f,[]))) 409 ["artefact ",name," already exists for job ",jid]
    val out = TextIO.openOut f
  in
    outputN_from_stdIn(out, len);
    TextIO.closeOut out
    (* TODO: update link on downloads page on CakeML main site? *)
  end

fun finish id =
  let
    val f = Int.toString id
    val old = OS.Path.concat("running",f)
    val new = OS.Path.concat("finished",f)
    val fd = acquire_lock ()
    val () =
      if OS.FileSys.access(old,[OS.FileSys.A_READ]) then
        if OS.FileSys.access(new,[OS.FileSys.A_READ]) then
          cgi_die 500 ["job ",f, " is both running and finished"]
        else OS.FileSys.rename{old = old, new = new}
      else cgi_die 409 ["job ",f," is not running: cannot finish"]
    val inp = TextIO.openIn new
    val sha = get_head_sha (read_bare_snapshot inp)
    val status = read_status inp
    val inp = (TextIO.closeIn inp; TextIO.openIn new)
    val pr_info = read_job_pr inp
    val branch =
      case read_job_pr inp of
        NONE => "master"
      | SOME (_, branch) => Substring.string branch
  in
    TextIO.closeIn inp;
    Posix.IO.close fd;
    GitHub.set_status f sha status;
    send_email (String.concat[status_to_string status,": Job ",f," ",branch])
               (String.concat["See ",server,"/job/",f,"\n"]);
    ()
  end

fun abort id =
  let
    val f = Int.toString id
    val old = OS.Path.concat("finished",f)
    val new = OS.Path.concat("aborted",f)
    val fd = acquire_lock ()
  in
    if OS.FileSys.access(old,[OS.FileSys.A_READ]) then
      if OS.FileSys.access(new,[OS.FileSys.A_READ]) then
        cgi_die 500 ["job ",f, " is both finished and aborted"]
      else (OS.FileSys.rename{old = old, new = new}; Posix.IO.close fd)
    else cgi_die 409 ["job ",f," is not finished: cannot abort"]
  end

fun refresh () =
  let
    val snapshots = get_current_snapshots ()
    val fd = acquire_lock ()
    val () = clear_list "waiting"
    (* TODO: stop timed out jobs *)
    val running_ids = running()
    val finished_ids = finished()
    val snapshots = filter_out (same_head "running") running_ids snapshots
    val snapshots = filter_out (same_snapshot "finished") finished_ids snapshots
    val avoid_ids = running_ids @ finished_ids @ aborted()
    val () = if List.null snapshots then ()
             else ignore (List.foldl (add_waiting avoid_ids) 1 snapshots)
  in Posix.IO.close fd end

datatype request = Api of api | Html of html_request

fun check_auth auth ua =
  if auth = SOME (String.concat["Bearer ",cakeml_token]) orelse
     Option.map (String.isPrefix "GitHub-Hookshot/") ua = SOME true
  then ()
  else cgi_die 401 ["Unauthorized: ", Option.valOf auth handle Option => "got nothing"]

fun get_api () =
  case (OS.Process.getEnv "PATH_INFO",
        OS.Process.getEnv "REQUEST_METHOD") of
    (NONE, SOME "GET") => SOME (Html Overview)
  | (SOME path_info, SOME "GET") =>
      if String.isPrefix "/api" path_info then
        Option.map (Api o G) (get_from_string (String.extract(path_info,4,NONE)))
      else if String.isPrefix "/job/" path_info then
        Option.map (Html o DisplayJob) (id_from_string (String.extract(path_info,5,NONE)))
      else if String.isPrefix "/queue/" path_info then
        SOME (Html (DisplayQueue (String.extract(path_info,7,NONE))))
      else if path_info = "/" then SOME (Html Overview)
      else cgi_die 404 [path_info," not found"]
  | (SOME path_info, SOME "POST") =>
    let
      val () = cgi_assert (String.isPrefix "/api" path_info) 400 [path_info," not found"]
      val () = check_auth (OS.Process.getEnv "HTTP_AUTHORIZATION")
                          (OS.Process.getEnv "HTTP_USER_AGENT")
      val len = Option.mapPartial Int.fromString
                  (OS.Process.getEnv "CONTENT_LENGTH")
    in
      Option.map (Api o P)
        (post_from_string (String.extract(path_info,4,NONE)) len)
    end
  | _ => NONE

local
  fun id_list ids = String.concatWith " " (List.map Int.toString ids)
in
  fun dispatch api =
    let
      val response =
        case api of
          (G Waiting) => id_list (waiting())
        | (G (Job id)) => file_to_string (job id)
        | (P p) => post_response p
      val () =
        case api of
          (G _) => ()
        | (P Refresh) => refresh ()
        | (P (Claim x)) => claim x
        | (P (Append x)) => append x
        | (P (Log x)) => log x
        | (P (Upload x)) => upload x
        | (P (Finish x)) => finish x
        | (P (Abort x)) => abort x
    in
      write_text_response 200 response
    end
end

fun dispatch_req req =
  let in
    case req of
      (Api api) => dispatch api
    | (Html req) => html_response req
  end handle e => cgi_die 500 [exnMessage e]

fun main () =
  let
    val () = ensure_dirs ()
  in
    case get_api () of
      NONE => cgi_die 400 ["bad usage"]
    | SOME req => dispatch_req req
  end
