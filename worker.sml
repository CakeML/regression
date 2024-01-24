(*
  Worker that claims and runs regression test jobs.

  Assumes the following are available:
    /usr/bin/curl
    /usr/bin/git
    /usr/bin/time
    poly

  Also assumes the default shell (/bin/sh) understands
    [var=val] cmd [args ...] >file 2>&1
  to mean redirect both stdout and stderr to file when running
  cmd on args in an environment augmented with var set to val (if present),
  and >>file instead of >file appends to file instead of truncating it.
*)

use "apiLib.sml"; open apiLib

fun usage_string name = String.concat[
  name, " [options]\n\n",
  "Runs waiting jobs from ",server,"/\n\n",
  "Summary of options:\n",
  "  --no-wait    : Exit when no waiting jobs are found rather than waiting for them.\n",
  "                 Will still check for more waiting jobs after completing a job.\n",
  "  --no-loop    : Exit after finishing a job, do not check for more waiting jobs.\n",
  "  --select id  : Ignore the waiting jobs list and instead attempt to claim job <id>.\n",
  "  --resume id  : Assume job <id> has previously been claimed by this worker and\n",
  "                 attempt to start running it again. If the job fails again,\n",
  "                 exit (even without --no-loop).\n",
  "  --upload id  : Assume this worker has just finished job <id> and upload its build\n",
  "                 artefacts (usually automatic after master succeeds), then exit.\n",
  "  --finish id  : Mark job <id> as finished, then exit.\n",
  "  --abort id   : Mark job <id> as having aborted, i.e., finished without a proper\n",
  "                 success or failure, then exit.\n",
  "  --release id : Release job <id> on GitHub, then exit.\n",
  "  --refresh    : Refresh the server's waiting queue from GitHub then exit.\n"];

(*

  All work will be carried out in the current directory
  (or subdirectories of it) with no special permissions.
  Assumes this directory starts empty except for the
  worker executable and one file
    name
  containing this worker node's identity. This could be
  created as follows:
    uname -norm > name
  It must not start with whitespace nor contain any single quotes.

  Uses bare clones of HOL and CakeML, which are created if necessary.

  For each HOL commit, there is a HOL working directory: HOL-<sha>.
  This directory is assumed to be built if it exists; it is built when created.
  If building HOL fails, its working directory needs to be manually deleted (we
  keep it around for diagnostics).

  For each job, there is a CakeML working directory: cakeml-<jid>.

  Jobs are handled as follows:
    1. Find a job in the waiting queue
    2. Claim the job
    3. Set up HOL working directory according to the job snapshot
    4. Build HOL if necessary, capturing stdout and stderr
       On failure:
         1. Append "FAILED: building HOL"
         2. Log the captured output
         3. Finish the job
    5. Set up cakeml working directory according to the job snapshot
    6. For each directory in the CakeML build sequence
       1. Append "Starting <dir>"
       2. Holmake in that directory,
            capturing stdout and stderr,
            and capturing time and memory usage
          On failure:
            1. Append "FAILED: <dir>"
            2. Log the captured output
            3. Finish the job
       3. Append "Finished <dir>: <time> <memory>"
    7. Append "SUCCESS"
    8. Finish the job
*)

fun warn ls = (
  TextIO.output(TextIO.stdErr,String.concat ls);
  TextIO.output(TextIO.stdErr,"\n"))

fun die ls = (
  warn ls;
  OS.Process.exit OS.Process.failure;
  raise (Fail "impossible"))

fun diag ls = (
  TextIO.output(TextIO.stdOut,String.concat ls);
  TextIO.output(TextIO.stdOut,"\n"))

fun assert b ls = if b then () else die ls

fun file_to_line f =
  let
    val inp = TextIO.openIn f
    val lopt = TextIO.inputLine inp
    val () = TextIO.closeIn inp
  in
    case lopt of NONE => ""
    | SOME line => String.extract(line,0,SOME(String.size line - 1))
  end

val orig_working_dir = OS.FileSys.getDir ()

val status_dir = OS.Path.concat (orig_working_dir, "status")
fun status_fname jid = OS.Path.concat (status_dir, "job_" ^ jid)

fun status_begin resumed jid = let
    val pid = Posix.ProcEnv.getpid ()
    val pid_string = SysWord.fmt StringCvt.DEC (Posix.Process.pidToWord pid)
    val _ = OS.FileSys.mkDir status_dir handle OS.SysErr _ => ()
    val f = TextIO.openOut (status_fname jid)
    val msg = "Job started.\njob: " ^ jid ^ "\npid: " ^ pid_string ^ "\n"
    val r_msg = "resumed: " ^ (if resumed then "true" else "false") ^ "\n"
  in TextIO.output (f, msg ^ r_msg); TextIO.closeOut f end

fun status_append jid line = let
    val f = TextIO.openAppend (status_fname jid)
  in TextIO.output (f, line ^ "\n"); TextIO.closeOut f end

fun status_last_line jid = let
    val f = TextIO.openIn (status_fname jid)
    fun get prev = case TextIO.inputLine f of
        NONE => prev
      | SOME line => get (if null (String.tokens Char.isSpace line)
          then prev else line)
    val last = get ""
  in TextIO.closeIn f; last end

val system_output = system_output die

val capture_file = "regression.log"
val timing_file = "timing.log"

fun system_capture_with redirector cmd_args =
  let
    (* This could be implemented using Posix without relying on the shell *)
    val status = OS.Process.system(String.concat[cmd_args, redirector, capture_file, " 2>&1"])
  in OS.Process.isSuccess status end

structure API = struct
  val endpoint = String.concat[server,"/api"]
  val std_options = ["--silent","--show-error","--header",String.concat["Authorization: Bearer ",cakeml_token]]
  fun curl_cmd api = (curl_path,
      std_options
    @ api_curl_args api
    @ [String.concat[endpoint,api_to_string api]])
  val raw_post = system_output o curl_cmd o P
  val get = system_output o curl_cmd o G
  fun post p =
    let
      val expected = post_response p
      val response = raw_post p
    in
      assert (response=expected)
        ["Unexpected response:\nWanted: ",expected,"Got: ",response]
    end
end

fun system_capture_loop redirector id cmd_args = let
    val jid = Int.toString id
    val dir = OS.FileSys.getDir ()
    val msg = "Task: " ^ cmd_args ^ "\nDir: " ^ dir ^ "\nRunning."
    val _ = status_append jid msg
    val res = system_capture_with redirector cmd_args
    val killed = String.isPrefix "Restart" (status_last_line jid)
  in if killed then
    (API.post (Append (id, "Restarting task."));
        system_capture_loop redirector id cmd_args)
  else res end

val system_capture = system_capture_loop " >"
val system_capture_append = system_capture_loop " >>"

val hol_remote = "https://github.com/HOL-Theorem-Prover/HOL.git"
val cakeml_remote = "https://github.com/CakeML/cakeml.git"

val HOLDIR_git = "HOL.git"
val CAKEMLDIR_git = "cakeml.git"

fun mk_HOLDIR sha = String.concat["HOL-",sha]
fun mk_CAKEMLDIR jid = String.concat["cakeml-",jid]

val artefacts_file = OS.Path.concat("developers","artefacts")

val git_path = "/usr/bin/git"
fun git_reset sha = (git_path,["reset","--hard","--quiet",sha])
val git_fetch = (git_path,["fetch","origin"])
val git_fetch_all = (git_path,["fetch","--prune","origin","+refs/heads/*:refs/heads/*","+refs/pull/*/head:refs/heads/pr/*"])
fun git_sha_of head = (git_path,["rev-parse","--verify",head])
val git_head = git_sha_of "HEAD"
val git_merge_head = git_sha_of "MERGE_HEAD"

local
  open OS.FileSys
in
  fun ensure_bare_clone_exists remote options dir =
    if access (dir,[]) then
      if isDir dir then
        let
          val () = chDir dir
          val output = system_output (git_path,["remote","get-url","origin"])
          val () = chDir OS.Path.parentArc
        in
          assert (String.isPrefix remote output) [dir," remote misconfigured"]
        end
      else die [dir," is not a directory"]
    else
      let
        val () = diag [dir," does not exist: will clone"]
        val status = OS.Process.system (String.concat[git_path," clone --bare ",options,remote," ",dir])
      in
        assert (OS.Process.isSuccess status) ["git clone failed for ",dir]
      end
  fun update_bare_clone fetch dir =
    let in
      chDir dir;
      system_output fetch;
      chDir OS.Path.parentArc
    end
end

fun link_poly_includes () =
  let
    val includes_file = "poly-includes.ML"
    val f = up includes_file
  in
    if OS.FileSys.access(f,[OS.FileSys.A_READ]) then
      Posix.FileSys.symlink { old = up f,
                              new = OS.Path.concat("tools-poly",includes_file) }
      before diag ["Linking to custom ",includes_file]
    else ()
  end

fun prepare_hol HOLDIR sha =
  if OS.FileSys.access(HOLDIR,[]) then
    OS.FileSys.access(OS.Path.concat(HOLDIR,OS.Path.concat("bin","build")),[OS.FileSys.A_EXEC])
    orelse die [HOLDIR, " exists but does not contain executable bin/build"]
  else
    let
      val status = OS.Process.system (String.concat[git_path," clone --shared ",HOLDIR_git," ",HOLDIR])
    in
      assert (OS.Process.isSuccess status) ["Failed to create working copy of HOL"];
      OS.FileSys.chDir HOLDIR;
      system_output (git_reset sha);
      link_poly_includes ();
      OS.FileSys.chDir OS.Path.parentArc;
      false
    end

fun prepare_cakeml HOLDIR CAKEMLDIR x =
  let
    val status = OS.Process.system (String.concat[git_path," clone --shared ",CAKEMLDIR_git," ",CAKEMLDIR])
    val () = assert (OS.Process.isSuccess status) ["Failed to create working copy of CakeML"]
    val () = OS.FileSys.chDir CAKEMLDIR
    val _ =
      case x of
        Bbr sha => system_output (git_reset sha)
      | Bpr {head_sha, base_sha} =>
          (system_output (git_reset base_sha);
           system_output (git_path,["merge","--no-commit","--no-ff","--quiet",head_sha]))
    val () = output_to_file ("HOLDIR", HOLDIR)
  in
    OS.FileSys.chDir OS.Path.parentArc
  end

local
  val configure_hol = "poly --script tools/smart-configure.sml"
in
  fun build_hol HOLDIR reused id =
    let
      val () = OS.FileSys.chDir HOLDIR
      val configured =
        reused orelse system_capture id configure_hol
      val built = configured andalso
                  system_capture_append id "bin/build --nograph"
      val () = OS.FileSys.chDir OS.Path.parentArc
      val () = if built then () else
               (API.post (Append (id, "FAILED: building HOL"));
                API.post (Log(id,capture_file,0));
                API.post (Finish id))
    in
      built
    end
end

fun upload id p =
  if OS.FileSys.access(p,[])
  then API.post (Upload(id,p,0))
  else warn ["Could not find ",p," to upload."]

fun upload_artefacts CAKEMLDIR id =
  let
    val f = OS.Path.concat(CAKEMLDIR,artefacts_file)
  in
    let
      val inp = TextIO.openIn f
      fun loop () =
        case TextIO.inputLine inp of NONE => TextIO.closeIn inp
        | SOME line => (upload id (OS.Path.concat(CAKEMLDIR, trimr line)); loop ())
    in loop () end
    handle e as OS.SysErr _ => warn ["Could not find artefacts list ",f,"\n",exnMessage e]
  end

local
  val resume_file = "resume"
  val time_options = String.concat["--format='%e %M' --output='",timing_file,"'"]
  fun pad dir =
    let
      val z = String.size dir
      val n = if z < max_dir_length then max_dir_length - z
              else (warn ["max_dir_length is too small for ",dir]; 1)
    in CharVector.tabulate(n,(fn _ => #" ")) end
  val no_skip = ((fn _ => false), "Starting ")
in
  fun run_regression HOLDIR CAKEMLDIR resumed is_master id =
    let
      val root = OS.FileSys.getDir()
      val holdir = OS.Path.concat(root,HOLDIR)
      val holmake_cmd =
        String.concat["HOLDIR='",holdir,"' /usr/bin/time ",time_options,
                      " '",holdir,"/bin/Holmake' --qof"]
      val cakemldir = OS.Path.concat(root,CAKEMLDIR)
      val () = OS.FileSys.chDir CAKEMLDIR
               handle e as OS.SysErr _ => die ["Failed to enter ",CAKEMLDIR,
                                               "\n","root: ",root,
                                               "\n",exnMessage e]
      val () = assert (OS.FileSys.getDir() = cakemldir) ["impossible failure"]
      val seq = TextIO.openIn("developers/build-sequence")
                handle e as OS.SysErr _ => die ["Failed to open build-sequence: ",exnMessage e]
      val skip =
        (if resumed then (not o equal (file_to_string resume_file), "Resuming ")
                    else no_skip)
        handle (e as IO.Io _) => die["Failed to load resume file: ",exnMessage e]
      fun loop skip =
        case TextIO.inputLine seq of NONE => true
        | SOME line =>
          if String.isPrefix "#" line orelse
             String.isPrefix "\n" line orelse
             #1 skip line
          then loop skip else
          let
            val () = output_to_file (resume_file, line)
            val line_in = until_space line
            val (dir, artefacts) = case String.tokens (fn c => c = #":") line_in of [] => ("", [])
                                                                                  | x::y => (x, y)
            val () = API.post (Append(id, String.concat[#2 skip,dir]))
            val entered = (OS.FileSys.chDir dir; true)
                          handle e as OS.SysErr _ => (API.post (Append(id, exnMessage e)); false)
          in
            if entered andalso system_capture id holmake_cmd then
              (if is_master then app ((upload id) o trimr) artefacts else ();
               API.post (Append(id,
                 String.concat["Finished ",dir,pad dir,file_to_line timing_file]));
               OS.FileSys.chDir cakemldir;
               loop no_skip)
            else
              (API.post (Append(id,String.concat["FAILED: ",dir]));
               API.post (Log(id,capture_file,0));
               API.post (Finish id);
               false)
          end
      val success = loop skip
      val () = OS.FileSys.chDir root
      val () =
        if success then
          let in
            API.post (Append(id,"SUCCESS"));
            API.post (Finish id)
          end
        else ()
    in
      success
    end
end

fun validate_resume jid bhol bcml =
  let
    val () = diag ["Checking HOL for resuming job ",jid]
    val HOLDIR = mk_HOLDIR bhol
    val () = OS.FileSys.chDir HOLDIR
             handle e as OS.SysErr _ => die ["Could not enter ",HOLDIR,"\n",exnMessage e]
    val head = system_output git_head
    val () = assert (String.isPrefix bhol head) ["Wrong HOL commit: wanted ",bhol,", at ",head]
    val () = OS.FileSys.chDir OS.Path.parentArc
    val () = diag ["Checking CakeML for resuming job ",jid]
    val CAKEMLDIR = mk_CAKEMLDIR jid
    val () = OS.FileSys.chDir CAKEMLDIR
             handle e as OS.SysErr _ => die ["Could not enter ",CAKEMLDIR,"\n",exnMessage e]
    val () =
      case bcml of
        Bbr sha => assert (String.isPrefix sha (system_output git_head)) ["Wrong CakeML commit: wanted ",sha]
      | Bpr {head_sha, base_sha} =>
        (assert (String.isPrefix base_sha (system_output git_head)) ["Wrong CakeML base commit: wanted ",base_sha];
         assert (String.isPrefix head_sha (system_output git_merge_head)) ["Wrong CakeML head commit: wanted ",head_sha])
    val () = OS.FileSys.chDir OS.Path.parentArc
  in
    true
  end

fun work resumed id =
  let
    val response = API.get (Job id)
    val jid = Int.toString id
    val _ = status_begin resumed jid
  in
    if String.isPrefix "Error:" response
    then (warn [response]; false) else
    let
      val inp = TextIO.openString response
      val {bcml,bhol} = read_bare_snapshot inp
                        handle Option => die["Job ",jid," returned invalid response"]
      val () = TextIO.closeIn inp
      val () = update_bare_clone git_fetch HOLDIR_git
      val () = update_bare_clone git_fetch_all CAKEMLDIR_git
      val HOLDIR = mk_HOLDIR bhol
      val CAKEMLDIR = mk_CAKEMLDIR jid
      val built_hol =
        if resumed then validate_resume jid bhol bcml
        else let
          val () = diag ["Preparing HOL for job ",jid]
          val reused = prepare_hol HOLDIR bhol
          val s = if reused then "Reusing" else "Building"
          val () = diag [s," HOL for job ",jid]
          val () = API.post (Append(id,String.concat[s," HOL"]))
          val built_hol = build_hol HOLDIR reused id
        in
          diag ["Preparing CakeML for job ",jid];
          prepare_cakeml HOLDIR CAKEMLDIR bcml;
          built_hol
        end
      val is_master = case bcml of Bbr _ => true | _ => false
    in
      built_hol andalso
      (diag ["Running regression for job ",jid];
       run_regression HOLDIR CAKEMLDIR resumed is_master id)
    end
  end

fun wait () =
  let
    val cmd = curl_path
    (* 20 minute timeout in any case *)
    val timeout = ["-m", "1200"]
    val args = API.std_options @ timeout
        @ [String.concat[host,"/regression-updates.cgi"]]
    val proc = Unix.execute(cmd,args)
               handle e as OS.SysErr _ => die[curl_path," failed to execute on",String.concatWith" "args,"\n",exnMessage e]
    val inp = Unix.textInstreamOf proc
    fun loop () =
      case TextIO.inputLine inp of NONE => ()
      | SOME line =>
        if String.size line = 1 then ()
        else loop ()
    val () = loop ()
    val () = Unix.kill(proc,Posix.Signal.int)
  in Unix.reap proc end

fun arg_job_action name [arg, jid] action =
  if arg = name then
    case Int.fromString jid of
      SOME id => (action jid id; OS.Process.exit OS.Process.success)
    | NONE => die ["Invalid argument after ", arg, ": ", jid]
  else ()
  | arg_job_action _ _ _ = ()

fun extract_arg name args =
  let val (x, args) = List.partition (equal name) args
  in (not (List.null x), args) end

fun main () =
  let
    val args = CommandLine.arguments()
    val () = if List.exists (fn a => a="--help" orelse a="-h" orelse a="-?") args
             then (
               TextIO.output(TextIO.stdOut, usage_string(CommandLine.name()));
               OS.Process.exit OS.Process.success )
             else ()
    val () = if args = ["--refresh"]
             then (
               TextIO.output(TextIO.stdOut, API.raw_post Refresh);
               OS.Process.exit OS.Process.success )
             else ()
    val () = arg_job_action "--abort" args (fn jid => fn id => (
               diag ["Marking job ", jid, " as aborted."];
               API.post (Abort id)))
    val () = arg_job_action "--finish" args (fn jid => fn id => (
               diag ["Marking job ", jid, " as finished."];
               API.post (Finish id)))
    val () = arg_job_action "--upload" args (fn jid => fn id => (
                 diag ["Uploading artefacts for job ",jid,"."];
                 upload_artefacts (mk_CAKEMLDIR jid) id))
    val () = arg_job_action "--release" args (fn jid => fn id => (
               diag ["Releasing job ", jid, " on GitHub."];
               API.post (Release id)))
    val (no_wait, args) = extract_arg "--no-wait" args
    val (no_loop, args) = extract_arg "--no-loop" args
    datatype selection_target = Resume | Select
    exception UnexpectedArgs
    val selection =
      (case args of
         [] => NONE
       | [arg, jid] =>
         (case Int.fromString jid of
            SOME id => if arg = "--resume" then SOME(Resume, id) else
                       if arg = "--select" then SOME(Select, id) else
                       raise UnexpectedArgs
          | _ => raise UnexpectedArgs)
       | _ => raise UnexpectedArgs)
      handle UnexpectedArgs =>
             die [String.concatWith" "("Unexpected arguments:"::args)]
    val name = file_to_line "name"
               handle IO.Io _ => die["Could not determine worker name. Try uname -norm >name."]
    val () = ensure_bare_clone_exists hol_remote "--single-branch " HOLDIR_git
    val () = ensure_bare_clone_exists cakeml_remote "" CAKEMLDIR_git
    fun loop selection =
      let
        val waiting_ids =
          case selection of SOME(_, id) => [id] | NONE =>
          List.map (Option.valOf o Int.fromString)
            (String.tokens Char.isSpace (API.get Waiting))
      in
        case waiting_ids of [] =>
          if no_wait then diag ["No waiting jobs. Exiting."]
          else (diag ["No waiting jobs. Will wait for them."]; wait (); loop NONE)
        | (id::_) => (* could prioritise for ids that match our HOL dir *)
          let
            val jid = Int.toString id
            val () = diag ["About to work on ",server,"/job/",jid]
            val resumed = case selection of SOME (Resume, _) => true | _ => false
            val claim = Claim(id,name)
            val claim_response = post_response claim
            val response = if resumed then claim_response
                           else API.raw_post claim
            val success =
              if response=claim_response
              then work resumed id
              else (warn ["Claim of job ",jid," failed: ",response]; false)
          in
            if no_loop orelse (resumed andalso not success)
            then diag ["Finished work. Exiting."]
            else (diag ["Finished work. Looking for more."]; loop NONE)
          end
      end handle e => die ["Unexpected failure: ",exnMessage e]
  in loop selection end
