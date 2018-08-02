(*
Implements the server-side regression-test API as a CGI program.

The API is for workers to view and manipulate the job queues.
It also provides a hook for refreshing the queues:
  If there are new jobs on GitHub, they will be added as waiting.
  If there are stale jobs, they will be removed.

Each job is on exactly one list: waiting, running, stopped, aborted.
If a job changes list, it can only move to the right.

The waiting queue is refreshed as follows:

  1. Clear the waiting jobs list
  2. Get the current snapshots from GitHub (see below)
  3. Filter out from the snapshots:
    - any with the same CakeML head commit as a running job
    - any with the same snapshot (head+base+HOL commits) as a stopped job
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

fun job conn id =
  let
    val f = Int.toString id
    val q = queue_of_job conn f
  in
    OS.Path.concat(q,f)
  end

fun claim conn (id,name) =
  let
    val f = Int.toString id
    val old = OS.Path.concat("waiting",f)
    val new = OS.Path.concat("running",f)
    val fd = acquire_lock ()
    val () =
      if OS.FileSys.access(new,[OS.FileSys.A_READ]) then
        cgi_die conn 500 ["job ",f, " is both waiting and running"]
      else OS.FileSys.rename{old = old, new = new}
    val out = TextIO.openAppend new
    val () = print_claimed out (name,Date.fromTimeUniv(Time.now()))
    val () = TextIO.closeOut out
    val inp = TextIO.openIn new
    val sha = get_head_sha (read_bare_snapshot inp)
              handle Option => cgi_die conn 500 ["job ",f," has invalid file format"]
  in
    TextIO.closeIn inp;
    Posix.IO.close fd;
    GitHub.set_status conn f sha Pending
  end

fun append conn (id,line) =
  let
    val f = Int.toString id
    val p = OS.Path.concat("running",f)
    val fd = acquire_lock ()
    val out = TextIO.openAppend p handle e as IO.Io _ => (cgi_die conn 409 ["job ",f," is not running: cannot append"]; raise e)
  in
    print_log_entry out (Date.fromTimeUniv(Time.now()),line);
    TextIO.closeOut out;
    Posix.IO.close fd
  end

fun log conn (id,_,len) =
  let
    val f = Int.toString id
    val p = OS.Path.concat("running",f)
    val fd = acquire_lock ()
    val out = TextIO.openAppend p
              handle e as IO.Io _ => (cgi_die conn 409 ["job ",f," is not running: cannot log"]; raise e)
  in
    outputN_from_socket conn (out, len);
    TextIO.closeOut out;
    Posix.IO.close fd
  end

fun upload conn (id,name,len) =
  let
    val jid = Int.toString id
    val dir = OS.Path.concat(artefacts_dir,jid)
    val () = if OS.FileSys.access(dir,[]) then () else OS.FileSys.mkDir dir
    val () = cgi_assert conn (OS.FileSys.isDir dir) 500 [dir," exists and is not a directory"]
    val f = OS.Path.concat(dir,name)
    val () = cgi_assert conn (not(OS.FileSys.access(f,[]))) 409 ["artefact ",name," already exists for job ",jid]
    val out = TextIO.openOut f
  in
    outputN_from_socket conn (out, len);
    TextIO.closeOut out
    (* TODO: update link on downloads page on CakeML main site? *)
  end

fun stop conn id =
  let
    val f = Int.toString id
    val old = OS.Path.concat("running",f)
    val new = OS.Path.concat("stopped",f)
    val fd = acquire_lock ()
    val () =
      if OS.FileSys.access(old,[OS.FileSys.A_READ]) then
        if OS.FileSys.access(new,[OS.FileSys.A_READ]) then
          cgi_die conn 500 ["job ",f, " is both running and stopped"]
        else OS.FileSys.rename{old = old, new = new}
      else cgi_die conn 409 ["job ",f," is not running: cannot stop"]
    val inp = TextIO.openIn new
    val sha = get_head_sha (read_bare_snapshot inp)
    val status = read_status inp
    val inp = (TextIO.closeIn inp; TextIO.openIn new)
    val typ = read_job_type inp
  in
    TextIO.closeIn inp;
    Posix.IO.close fd;
    GitHub.set_status conn f sha status;
    send_email conn
      (String.concat[status_to_string status,": Job ",f," (",typ,")"])
      (String.concat["See ",server,"/job/",f,"\n"]);
    ()
  end

fun abort conn id =
  let
    val f = Int.toString id
    val old = OS.Path.concat("stopped",f)
    val new = OS.Path.concat("aborted",f)
    val fd = acquire_lock ()
  in
    if OS.FileSys.access(old,[OS.FileSys.A_READ]) then
      if OS.FileSys.access(new,[OS.FileSys.A_READ]) then
        cgi_die conn 500 ["job ",f, " is both stopped and aborted"]
      else (OS.FileSys.rename{old = old, new = new}; Posix.IO.close fd)
    else cgi_die conn 409 ["job ",f," is not stopped: cannot abort"]
  end

fun refresh conn () =
  let
    val () = OS.Process.sleep (Time.fromSeconds 20)
    val snapshots = get_current_snapshots conn
    val fd = acquire_lock ()
    val () = clear_list conn "waiting"
    (* TODO: stop timed out jobs *)
    val running_ids = running conn ()
    val stopped_ids = stopped conn ()
    val snapshots = filter_out (same_head conn "running") running_ids snapshots
    val snapshots = filter_out (same_snapshot conn "stopped") stopped_ids snapshots
    val avoid_ids = running_ids @ stopped_ids @ aborted conn ()
    val () = if List.null snapshots then ()
             else ignore (List.foldl (add_waiting conn avoid_ids) 1 snapshots)
  in Posix.IO.close fd end

datatype request = Api of api | Html of html_request

fun check_auth conn auth ua =
  if auth = SOME (String.concat["Bearer ",cakeml_token]) orelse
     Option.map (String.isPrefix "GitHub-Hookshot/") ua = SOME true
  then ()
  else cgi_die conn 401 ["Unauthorized: ", Option.valOf auth handle Option => "got nothing"]

fun get_api conn getEnv =
  case (getEnv "PATH_INFO",
        getEnv "REQUEST_METHOD") of
    (NONE, SOME "GET") => SOME (Html Overview)
  | (SOME path_info, SOME "GET") =>
      if String.isPrefix "/api" path_info then
        Option.map (Api o G) (get_from_string (String.extract(path_info,4,NONE)))
      else if String.isPrefix "/job/" path_info then
        Option.map (Html o DisplayJob) (id_from_string (String.extract(path_info,5,NONE)))
      else if path_info = "/" then SOME (Html Overview)
      else cgi_die conn 404 [path_info," not found"]
  | (SOME path_info, SOME "POST") =>
    let
      val () = cgi_assert conn (String.isPrefix "/api" path_info) 400 [path_info," not found"]
      val () = check_auth conn (getEnv "HTTP_AUTHORIZATION")
                          (getEnv "HTTP_USER_AGENT")
      val len = Option.mapPartial Int.fromString
                  (getEnv "CONTENT_LENGTH")
    in
      Option.map (Api o P)
        (post_from_string (String.extract(path_info,4,NONE)) len)
    end
  | _ => NONE

local
  fun id_list ids = String.concatWith " " (List.map Int.toString ids)
in
  fun dispatch conn api =
    let
      val response =
        case api of
          (G Waiting) => id_list (waiting conn ())
        | (G (Job id)) => file_to_string (job conn id)
        | (P p) => post_response p
      val () =
        case api of
          (G _) => ()
        | (P Refresh) => refresh conn ()
        | (P (Claim x)) => claim conn x
        | (P (Append x)) => append conn x
        | (P (Log x)) => log conn x
        | (P (Upload x)) => upload conn x
        | (P (Stop x)) => stop conn x
        | (P (Abort x)) => abort conn x
    in
      write_text_response conn 200 response
    end
end

fun dispatch_req conn req =
  let in
    case req of
      (Api api) => dispatch conn api
    | (Html req) => html_response conn req
  end handle e => cgi_die conn 500 [exnMessage e]

fun serve conn getEnv =
  let
    val () = ensure_dirs conn
  in
    case get_api conn getEnv of
      NONE => cgi_die conn 400 ["bad usage"]
    | SOME req => dispatch_req conn req
  end

fun main () =
  let
    val args = CommandLine.arguments()
    val port = Option.valOf (Int.fromString (List.hd args))
               handle Empty => 5000 | Option => 5000
    val listener = make_listener port
    fun loop () =
      let
        val (connection, _) = Socket.accept listener
        val environment = scgi_env (recvNetstring connection)
      in
        serve connection environment;
        Socket.close connection;
        loop ()
      end
  in loop () end
  handle e => (TextIO.output(TextIO.stdErr,String.concat["Exception: ",exnMessage e,"\n"]); raise e)
