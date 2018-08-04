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
    val () = acquire_lock ()
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
    release_lock ();
    GitHub.set_status conn f sha Pending
  end

fun append conn (id,line) =
  let
    val f = Int.toString id
    val p = OS.Path.concat("running",f)
    val () = acquire_lock ()
    val out = TextIO.openAppend p handle e as IO.Io _ => (cgi_die conn 409 ["job ",f," is not running: cannot append"]; raise e)
  in
    print_log_entry out (Date.fromTimeUniv(Time.now()),line);
    TextIO.closeOut out;
    release_lock ()
  end

fun log conn (id,_,len) =
  let
    val f = Int.toString id
    val p = OS.Path.concat("running",f)
    val () = acquire_lock ()
    val out = TextIO.openAppend p
              handle e as IO.Io _ => (cgi_die conn 409 ["job ",f," is not running: cannot log"]; raise e)
  in
    outputN_from_socket conn (out, len);
    TextIO.closeOut out;
    release_lock ()
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
    val () = acquire_lock ()
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
    release_lock ();
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
    val () = acquire_lock ()
  in
    if OS.FileSys.access(old,[OS.FileSys.A_READ]) then
      if OS.FileSys.access(new,[OS.FileSys.A_READ]) then
        cgi_die conn 500 ["job ",f, " is both stopped and aborted"]
      else (OS.FileSys.rename{old = old, new = new}; release_lock ())
    else cgi_die conn 409 ["job ",f," is not stopped: cannot abort"]
  end

fun refresh () =
  let
    val () = OS.Process.sleep (Time.fromSeconds 20)
    val snapshots = get_current_snapshots thread_die ()
    val () = acquire_lock ()
    val () = clear_list thread_die "waiting"
    (* TODO: stop timed out jobs *)
    val running_ids = running thread_die ()
    val stopped_ids = stopped thread_die ()
    val snapshots = filter_out (same_head thread_die "running") running_ids snapshots
    val snapshots = filter_out (same_snapshot thread_die "stopped") stopped_ids snapshots
    val avoid_ids = running_ids @ stopped_ids @ aborted thread_die ()
    val () = if List.null snapshots then ()
             else ignore (List.foldl (add_waiting avoid_ids) 1 snapshots)
  in release_lock () end

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
          (G Waiting) => id_list (waiting (cgi_die conn 500) ())
        | (G (Job id)) => file_to_string (job conn id)
        | (P p) => post_response p
      val () =
        case api of
          (G _) => ()
        | (P Refresh) => ignore (Thread.Thread.fork (refresh, []))
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
  end
  handle SML90.Interrupt => raise SML90.Interrupt
       | e => cgi_die conn 500 [exnMessage e]

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
    fun loop listener threads =
      let
        val threads = limit_threads 16 threads
        val (connection, _) = Socket.accept listener
        fun handle_request () =
          let in
            (serve connection
               (scgi_env (recvNetstring connection))
             handle SML90.Interrupt => raise SML90.Interrupt
                  | e => (Socket.close connection handle OS.SysErr _ => ();
                          thread_die ["Exception: ", exnMessage e]));
            Socket.close connection
          end
          handle SML90.Interrupt => raise SML90.Interrupt
               | e => thread_die ["Exception: ", exnMessage e]
        val thread = Thread.Thread.fork (handle_request, [])
      in
        loop listener (thread::threads)
      end handle OS.SysErr _ => loop (make_listener port) threads
  in loop (make_listener port) [] end
  handle e => die ["Exception: ", exnMessage e]
