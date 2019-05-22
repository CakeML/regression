(*
  Functions for manipulating the queues in the filesystem on the server,
  including for getting information from GitHub with which to update them.

  We use the filesystem as a database and put all state in it.
  Everything is relative to the current directory.

  We expect to be single-threaded, and use a lock file called
    lock
  to ensure this.

  Job lists are implemented as directories:
    waiting, running, finished, aborted

  Jobs are implemented as files with their id as filename.

  The directory a job is in indicates its status.

  File contents for a job:
    - HOL and CakeML commits
    - optional worker name and start time
    - output
  More concretely:
    CakeML: <SHA>
      <short message> [<date>]
    [#<PR> (<PR-name>)
     Merging into: <SHA>
      <short message> [<date>]]
    HOL: <SHA>
      <short message> [<date>]
    [Machine: <worker name>]

    [<date> Claimed job]
    [<date> <line of output>]
    ...
*)

use "apiLib.sml";

structure serverLib = struct

open apiLib

val content_type_text = "Content-Type: text/plain\n"

fun http_status 200 = ""
  | http_status 400 = "Status:400 Bad Request\n"
  | http_status 401 = "Status:401 Unauthorized\n"
  | http_status 404 = "Status:404 Not Found\n"
  | http_status 409 = "Status:409 Conflict\n"
  | http_status 500 = "Status:500 Internal Server Error\n"
  | http_status _ = "Status:500 Impossible Server Error\n"

fun write_text_response st s =
  let in
    TextIO.output(TextIO.stdOut, content_type_text);
    TextIO.output(TextIO.stdOut, http_status st);
    TextIO.output(TextIO.stdOut, "\n");
    TextIO.output(TextIO.stdOut, s)
  end

fun cgi_die st ls =
  let in
    write_text_response st "Error:\n";
    List.app (fn s => TextIO.output(TextIO.stdOut, s)) ls;
    TextIO.output(TextIO.stdOut,"\n");
    OS.Process.exit OS.Process.success;
    raise (Fail "impossible")
  end

fun cgi_assert b st ls = if b then () else cgi_die st ls

local
  open Posix.IO Posix.FileSys
  val flock = FLock.flock {ltype=F_WRLCK, whence=SEEK_SET, start=0, len=0, pid=NONE}
  val smode = S.flags[S.irusr,S.iwusr,S.irgrp,S.iroth]
  val lock_name = "lock"
in
  fun acquire_lock() =
    let
      val fd = Posix.FileSys.creat(lock_name,smode)
      val _ = Posix.IO.setlkw(fd,flock)
    in fd end
end

type obj = { hash : string, message : string, date : Date.date }
val empty_obj : obj = { hash = "", message = "", date = Date.fromTimeUniv Time.zeroTime }
fun with_hash x (obj : obj) : obj = { hash = x, message = #message obj, date = #date obj }
fun with_message x (obj : obj) : obj = { hash = #hash obj, message = x, date = #date obj }
fun with_date d (obj : obj) : obj = { hash = #hash obj, message = #message obj, date = d }

type pr = { num : int, head_ref : string, head_obj : obj, base_obj : obj, labels : string list }
val empty_pr : pr = { num = 0, head_ref = "", head_obj = empty_obj, base_obj = empty_obj, labels = [] }
fun with_num x (pr : pr) : pr = { num = x, head_ref = #head_ref pr, head_obj = #head_obj pr, base_obj = #base_obj pr, labels = #labels pr }
fun with_head_ref x (pr : pr) : pr = { num = #num pr, head_ref = x, head_obj = #head_obj pr, base_obj = #base_obj pr, labels = #labels pr }
fun with_head_obj x (pr : pr) : pr = { num = #num pr, head_ref = #head_ref pr, head_obj = x, base_obj = #base_obj pr, labels = #labels pr }
fun with_base_obj x (pr : pr) : pr = { num = #num pr, head_ref = #head_ref pr, head_obj = #head_obj pr, base_obj = x, labels = #labels pr }
fun with_labels x (pr : pr) : pr = { num = #num pr, head_ref = #head_ref pr, head_obj = #head_obj pr, base_obj = #base_obj pr, labels = x }

datatype integration = Branch of string * obj | PR of pr
type snapshot = { cakeml : integration, hol : obj }

fun bare_of_pr ({num,head_ref,head_obj,base_obj,labels}:pr) : bare_pr =
  {head_sha = #hash head_obj, base_sha = #hash base_obj}
fun bare_of_integration (Branch (_,obj)) = Bbr (#hash obj)
  | bare_of_integration (PR pr) = Bpr (bare_of_pr pr)
fun bare_of_snapshot ({cakeml,hol}:snapshot) : bare_snapshot =
  {bcml = bare_of_integration cakeml, bhol = #hash hol}

type log_entry = Date.date * line
type log = log_entry list

type job = {
  id : int,
  snapshot : snapshot,
  claimed : (worker_name * Date.date) option,
  output : log
}

val machine_date = Date.fmt "%Y-%m-%dT%H:%M:%SZ"
val pretty_date = Date.fmt "%b %d %H:%M:%S"
val pretty_date_moment = "MMM DD HH:mm:ss"
fun machine_secs s = String.concat["PT",Int.toString s,"S"]

fun print_claimed out (worker,date) =
  let
    fun pr s = TextIO.output(out,s)
    val prl = List.app pr
  in
    prl ["Machine: ",worker,"\n\n",machine_date date," Claimed job\n"]
  end

fun print_log_entry out (date,line) =
  let
    fun pr s = TextIO.output(out,s)
    val prl = List.app pr
  in
    prl [machine_date date, " ", line, "\n"]
  end

fun print_snapshot out (s:snapshot) =
  let
    fun pr s = TextIO.output(out,s)
    val prl = List.app pr
    fun print_obj obj =
      prl [#hash obj, "\n  ", #message obj, " [", machine_date (#date obj), "]\n"]

    val () = pr "CakeML: "
    val () =
      case #cakeml s of
        Branch (head_ref,base_obj) => print_obj base_obj
      | PR {num,head_ref,head_obj,base_obj,labels} => (
               print_obj head_obj;
               prl ["#", Int.toString num, " (", head_ref, ")\nMerging into: "];
               print_obj base_obj
             )
    val () = pr "HOL: "
    val () = print_obj (#hol s)
  in () end

fun print_job out (j:job) =
  let
    val () = print_snapshot out (#snapshot j)
    val () = case #claimed j of NONE => () | SOME claimed => print_claimed out claimed
    val () = List.app (print_log_entry out) (#output j)
  in () end

val artefacts_dir = "artefacts"
val queue_dirs = ["waiting","running","finished","aborted"]

local
  open OS.FileSys
in
  fun ensure_dirs () =
    let
      val dir = openDir(getDir())
      fun loop ls =
        case readDir dir of NONE => ls
      | SOME d => if isDir d then loop (List.filter(not o equal d) ls)
                  else if List.exists (equal d) ls then cgi_die 500 [d," exists and is not a directory"]
                  else loop ls
      val dirs = loop (artefacts_dir::queue_dirs)
      val () = if List.null dirs then () else
               let val fd = acquire_lock () in
                 List.app mkDir dirs;
                 Posix.IO.close fd
               end
    in
      closeDir dir
    end
end

local
  open OS.FileSys
in
  fun read_list q () =
    let
      val dir = openDir q handle OS.SysErr _ => cgi_die 500 ["could not open ",q," directory"]
      fun badFile f = cgi_die 500 ["found bad filename ",f," in ",q]
      fun loop acc =
        case readDir dir of NONE => acc before closeDir dir
      | SOME f => if isDir (OS.Path.concat(q,f)) handle OS.SysErr _ => cgi_die 500 [f, " disappeared from ", q, " unexpectedly"]
                  then cgi_die 500 ["found unexpected directory ",f," in ",q]
                  else case Int.fromString f of NONE => badFile f
                       | SOME id => if check_id f id then loop (insert id acc) else badFile f
      val ids = loop []
    in ids end
  fun clear_list q =
    let
      val dir = openDir q handle OS.SysErr _ => cgi_die 500 ["could not open ",q," directory"]
      fun loop () =
        case readDir dir of NONE => closeDir dir
      | SOME f =>
        let
          val f = OS.Path.concat(q,f)
          val () = remove f
                   handle (e as OS.SysErr _) =>
                     cgi_die 500 ["unexpected error removing ",f,"\n",exnMessage e]
        in loop () end
    in loop () end
  fun read_artefacts jid =
    let
      val dir = openDir(OS.Path.concat(artefacts_dir,jid))
      fun loop acc =
        case readDir dir of NONE => acc before closeDir dir
        | SOME f => loop (f :: acc)
    in loop [] end handle OS.SysErr _ => []
end

val waiting = List.rev o read_list "waiting"
val running = read_list "running"
val finished = read_list "finished"
val aborted = read_list "aborted"

val queue_funs = [waiting,running,finished,aborted]

fun queue_of_job f =
  let
    fun mk_path dir = OS.Path.concat(dir,f)
    fun access dir = OS.FileSys.access(mk_path dir, [OS.FileSys.A_READ])
  in
    Option.valOf (List.find access queue_dirs)
    handle Option => cgi_die 404 ["job ",f," not found"]
  end

fun read_job_snapshot q id : bare_snapshot =
  let
    val f = OS.Path.concat(q,Int.toString id)
    val inp = TextIO.openIn f handle IO.Io _ => cgi_die 500 ["cannot open ",f]
    val bs = read_bare_snapshot inp
             handle Option => cgi_die 500 [f," has invalid file format"]
    val () = TextIO.closeIn inp
  in bs end

fun timings_of_dir dir files =
  let
    fun foldthis (f,(t,fs)) =
      let
        val inp = TextIO.openIn f handle IO.Io _ => cgi_die 500 ["cannot open ",f]
      in
        (case read_total_time dir inp of NONE => (t,fs) | SOME s => (t+s,f::fs))
        before TextIO.closeIn inp
      end handle e => cgi_die 500 ["unexpected error on ",f,"\n",exnMessage e]
  in
    List.foldl foldthis (0,[]) files
  end

fun get_head_sha ({bcml,...}:bare_snapshot) =
  case bcml of Bbr sha => sha | Bpr {head_sha,...} => head_sha

fun same_snapshot q = equal o read_job_snapshot q
fun same_head q id =
  equal (get_head_sha (read_job_snapshot q id)) o get_head_sha

fun filter_out eq ids snapshots =
  let
    exception Return
    fun check_null x = if List.null x then raise Return else x
    fun remove_all_matching_this_id (id,snapshots) =
      check_null
        (List.filter (not o eq id o bare_of_snapshot) snapshots)
  in
    List.foldl remove_all_matching_this_id snapshots ids
    handle Return => []
  end

fun first_unused_id avoid_ids id =
  let
    fun loop id =
      if List.exists (equal id) avoid_ids
      then loop (id+1) else id
  in loop id end

fun add_waiting avoid_ids (snapshot,id) =
  let
    val id = first_unused_id avoid_ids id
    val f = Int.toString id
    val path = OS.Path.concat("waiting",f)
    val () = cgi_assert (not(OS.FileSys.access(path, []))) 500 ["job ",f," already exists waiting"]
    val out = TextIO.openOut path
    val () = print_snapshot out snapshot
    val () = TextIO.closeOut out
  in id+1 end

local
  val email_file = "email.txt"
in
  fun send_email subject body =
    let
      val () = output_to_file (email_file,body)
      val mail_cmd = ("/usr/bin/mail",
        ["-s",subject,"-r",to_address,"-m",email_file,"-.",to_address])
    in
      system_output (cgi_die 500) mail_cmd
    end
end

structure GitHub = struct
  val token = until_space (file_to_string "github-token")
  val graphql_endpoint = "https://api.github.com/graphql"
  fun graphql_curl_cmd query = (curl_path,["--silent","--show-error",
    "--header",String.concat["Authorization: Bearer ",token],
    "--request","POST",
    "--data",String.concat["{\"query\" : \"",query,"\"}"],
    graphql_endpoint])
  val graphql = system_output (cgi_die 500) o graphql_curl_cmd

  fun cakeml_status_endpoint sha =
    String.concat["/repos/",github_user,"/",github_repo,"/statuses/",sha]

  fun status_json id (st,desc) =
    String.concat[
      "{\"state\":\"",st,"\",",
      "\"target_url\":\"",server,"/job/",id,"\",",
      "\"description\":\"",desc,"\",",
      "\"context\":\"cakeml-regression-test\"}"]

  val rest_endpoint = "https://api.github.com"
  val rest_version = "application/vnd.github.v3+json"
  fun rest_curl_cmd endpoint data = (curl_path,["--silent","--show-error",
    "--header",String.concat["Authorization: Bearer ",token],
    "--header",String.concat["Accept: ",rest_version],
    "--write-out", "%{http_code}",
    "--output", "/dev/null",
    "--request","POST",
    "--data",data,
    String.concat[rest_endpoint,endpoint]])

  fun status Pending = ("pending","regression test in progress")
    | status Success = ("success","regression test succeeded")
    | status Failure = ("failure","regression test failed")
    | status Aborted = ("error","regression test aborted")

  fun set_status id sha st =
    let
      val cmd =
        rest_curl_cmd
          (cakeml_status_endpoint sha)
          (status_json id (status st))
      val response = system_output (cgi_die 500) cmd
    in
      cgi_assert
        (String.isPrefix "201" response)
        500 ["Error setting GitHub commit status\n",response]
    end
end


(* We ask for the first 100 PRs/labels below. GitHub requires some
   limit. We don't expect to hit 100. *)
val cakeml_query = String.concat [
  "{repository(name: \\\"cakeml\\\", owner: \\\"CakeML\\\"){",
  "defaultBranchRef { target { ... on Commit {",
  " oid messageHeadline committedDate }}}",
  "pullRequests(baseRefName: \\\"master\\\", first: 100, states: [OPEN]",
  " orderBy: {field: CREATED_AT, direction: DESC}){",
  " nodes { mergeable number headRefName",
  " labels(first: 100) { nodes { name } }",
  " headRef { target { ... on Commit {",
  " oid messageHeadline committedDate }}}",
  "}}}}" ]

val hol_query = String.concat [
  "{repository(name: \\\"HOL\\\", owner: \\\"HOL-Theorem-Prover\\\"){",
  "defaultBranchRef { target { ... on Commit {",
  " oid messageHeadline committedDate }}}}}" ]

structure ReadJSON = struct

  exception ReadFailure of string
  fun die ls = raise (ReadFailure (String.concat ls))

  type 'a basic_reader = substring -> ('a * substring)
  type 'a reader = 'a -> 'a basic_reader

  fun transform f (reader:'a basic_reader) : 'b reader
  = fn acc => fn ss =>
    let val (v, ss) = reader ss
    in (f v acc, ss) end

  val replace_acc : 'a basic_reader -> 'a reader =
    fn r => transform (fn x => fn _ => x) r

  fun post_read f (reader:'a basic_reader) : 'b basic_reader
  = fn ss => let val (v, ss) = reader ss
    in (f v, ss) end

  fun read1 ss c =
    case Substring.getc ss of SOME (c',ss) =>
      if c = c' then ss
      else die ["expected ",String.str c," got ",String.str c']
    | _ => die ["expected ",String.str c," got nothing"]

  val read_string : string basic_reader = fn ss =>
    let
      val ss = read1 ss #"\""
      fun loop ss acc =
        let
          val (chunk,ss) = Substring.splitl (not o equal #"\"") ss
          val z = Substring.size chunk
          val (c,ss) = Substring.splitAt(ss,1)
        in
          if 0 < z andalso Substring.sub(chunk,z-1) = #"\\" then
              loop ss (c::chunk::acc)
          else
            (Option.valOf(String.fromCString(Substring.concat(List.rev(chunk::acc)))),
             ss)
        end
    in
      loop ss []
    end

  val int_from_ss = Option.valOf o Int.fromString o Substring.string

  fun bare_read_date ss =
    let
      val (year,ss) = Substring.splitAt(ss,4)
      val ss = read1 ss #"-"
      val (month,ss) = Substring.splitAt(ss,2)
      val ss = read1 ss #"-"
      val (day,ss) = Substring.splitAt(ss,2)
      val ss = read1 ss #"T"
      val (hour,ss) = Substring.splitAt(ss,2)
      val ss = read1 ss #":"
      val (minute,ss) = Substring.splitAt(ss,2)
      val ss = read1 ss #":"
      val (second,ss) = Substring.splitAt(ss,2)
      val ss = read1 ss #"Z"
      val date = Date.date {
        day = int_from_ss day,
        hour = int_from_ss hour,
        minute = int_from_ss minute,
        month = month_from_int (int_from_ss month),
        offset = SOME (Time.zeroTime),
        second = int_from_ss second,
        year = int_from_ss year }
    in (date, ss) end
    handle Subscript => raise Option | ReadFailure _ => raise Option

  fun read_date ss =
    let
      val (s, ss) = read_string ss
      val (date, e) = bare_read_date (Substring.full s)
      val () = if Substring.isEmpty e then () else raise Option
    in (date, ss) end

  fun read_dict (dispatch : (string * 'a reader) list) : 'a reader
  = fn acc => fn ss =>
    let
      val ss = read1 ss #"{"
      fun loop ss acc =
        case Substring.getc ss of
          SOME(#"}",ss) => (acc, ss)
        | SOME(#",",ss) => loop ss acc
        | _ =>
          let
            val (key, ss) = read_string ss
            val ss = read1 ss #":"
            val (acc, ss) = assoc key dispatch acc ss
          in loop ss acc end
    in loop ss acc end

  fun read_opt_list read_item acc ss =
    let
      val ss = read1 ss #"["
      fun loop ss acc =
        case Substring.getc ss of
          SOME(#"]",ss) => (List.rev acc, ss)
        | SOME(#",",ss) => loop ss acc
        | _ =>
          (case read_item ss
           of (NONE, ss) => loop ss acc
            | (SOME v, ss) => loop ss (v::acc))
    in loop ss acc end

  fun read_list read_item acc ss = read_opt_list
    (post_read SOME read_item) acc ss

  fun mergeable_only "MERGEABLE" acc = acc
    | mergeable_only _ _ = NONE

  fun read_number ss =
    let val (n,ss) = Substring.splitl Char.isDigit ss
    in (int_from_ss n, ss) end

  val read_obj : obj basic_reader =
    read_dict
      [("oid", transform with_hash read_string)
      ,("messageHeadline", transform with_message read_string)
      ,("committedDate", transform with_date read_date)
      ] empty_obj

  val read_label : string basic_reader = read_dict
    [("name", replace_acc read_string)] ""
  val read_labels = read_dict
    [("nodes", read_list read_label)] []

  val read_pr : pr option basic_reader =
    read_dict
      [("mergeable", transform mergeable_only read_string)
      ,("number", transform (Option.map o with_num) read_number)
      ,("headRefName", transform (Option.map o with_head_ref) read_string)
      ,("labels", transform (Option.map o with_labels) read_labels)
      ,("headRef",
        read_dict
          [("target", transform (Option.map o with_head_obj) read_obj)])]
      (SOME empty_pr)

end

val no_test_labels = ["test failing", "no test"]
fun is_no_test_label l = List.exists (equal l) no_test_labels
fun do_test (PR pr) = not (List.exists is_no_test_label (#labels pr))
  | do_test _ = true

fun read_cakeml_integrations response = let
    open ReadJSON
    fun add_master obj acc = (Branch("master",obj)::acc)
    (* This assumes the PR base always matches master.
       We could read it from GitHub instead. *)
    fun add_prs prs [m as (Branch(_,base_obj))] =
      m :: List.map (PR o with_base_obj base_obj) prs
    | add_prs _ _ = cgi_die 500 ["add_prs"]
  in
    #1 (
      read_dict
      [("data",
        read_dict
        [("repository",
          read_dict
          [("defaultBranchRef",
            read_dict
            [("target", transform add_master read_obj)])
          ,("pullRequests",
            read_dict
            [("nodes", transform add_prs (read_opt_list read_pr []))])
          ])])] []
      (Substring.full response)
    )
  end

local
  open ReadJSON
in
  fun get_current_snapshots () : snapshot list =
    let
      val response = GitHub.graphql cakeml_query
      val cakeml_integrations = List.filter do_test
        (read_cakeml_integrations response)
      val response = GitHub.graphql hol_query
      val (hol_obj,ss) =
        read_dict
        [("data",
          read_dict
          [("repository",
            read_dict
            [("defaultBranchRef",
              read_dict
              [("target", replace_acc read_obj)])])])]
        empty_obj (Substring.full response)
    in
      List.map (fn i => { cakeml = i, hol = hol_obj } )
        (List.rev cakeml_integrations) (* after rev: oldest pull request first, master last *)
    end
    handle ReadFailure s => cgi_die 500 ["Could not read response from GitHub: ",s]
end

fun read_last_date inp =
  let
    fun loop acc =
      case TextIO.inputLine inp of NONE => acc
      | SOME line =>
        let
          val (date,_) = ReadJSON.bare_read_date (Substring.full line)
        in
          loop (SOME date)
        end handle Option => loop acc
  in loop NONE end

val html_response_header = "Content-Type: text/html\n\n<!doctype html>"

val style_href = "/regression-style.css"

structure HTML = struct
  val attributes = List.map (fn (k,v) => String.concat[k,"='",v,"'"])
  fun start_tag tag [] = String.concat["<",tag,">"]
    | start_tag tag attrs = String.concat["<",tag," ",String.concatWith" "(attributes attrs),">"]
  fun end_tag tag = String.concat["</",tag,">"]
  fun element tag attrs body = String.concat[start_tag tag attrs, String.concat body, end_tag tag]
  fun elt tag body = element tag [] [body]
  val html = element "html" [("lang","en")]
  val head = element "head" []
  val meta = start_tag "meta" [("charset","utf-8")]
  val stylesheet = start_tag "link" [("rel","stylesheet"),("type","text/css"),("href",style_href)]
  val momentjs = element "script" [("src","https://cdnjs.cloudflare.com/ajax/libs/moment.js/2.19.1/moment.min.js")] []
  val localisejs = element "script" [] [
    "function localiseTimes(all) {",
    "var ls = document.getElementsByTagName('time');",
    "for (var i = 0; i < ls.length; i++) {",
    "if (ls[i].getAttribute('class') == 'ago') {",
    "ls[i].innerHTML = ' [' + moment(ls[i].getAttribute('datetime')).fromNow() + ']';}",
    "else if (ls[i].getAttribute('class') == 'since') {",
    "ls[i].innerHTML = '[elapsed: ' + moment(ls[i].getAttribute('datetime')).fromNow(true) + ']';}",
    "else if (ls[i].getAttribute('class') == 'duration') {",
    "ls[i].innerHTML = '[average: ' + moment.duration(ls[i].getAttribute('datetime')).humanize() + ']';}",
    "else if (all) {",
    "ls[i].innerHTML = moment(ls[i].getAttribute('datetime')).format('",
    pretty_date_moment,"');}}}"]
  val updatejs = element "script" [] [
    "var upds = new EventSource('/regression-updates.cgi');",
    "upds.onmessage = function(e) { if (location.pathname.includes('/job/' + e.data) ||",
                                       "!(location.pathname.includes('/job/')))",
                                    "location.reload(); };" ]
  val title = elt "title" "CakeML Regression Test"
  val shortcut = start_tag "link" [("rel","shortcut icon"),("href","/cakeml-icon.png")]
  val header = head [meta,stylesheet,title,shortcut,momentjs,localisejs,updatejs]
  val body = element "body" [("onload","localiseTimes(true); setInterval(localiseTimes,60000,false);")]
  val h2 = elt "h2"
  val h3 = elt "h3"
  val strong = elt "strong"
  val pre = elt "pre"
  fun time d = element "time" [("datetime",machine_date d)] [pretty_date d]
  fun time_ago d = element "time" [("datetime",machine_date d),("class","ago")] [" [",pretty_date d,"]"]
  fun duration s = element "time" [("datetime",machine_secs s),("class","duration")] [Int.toString s,"s"]
  fun a_attrs attrs href body = element "a" (("href",href)::attrs) [body]
  val a = a_attrs []
  fun status_attrs Success = [("class","success")]
    | status_attrs Failure = [("class","failure")]
    | status_attrs _ = []
  val code = elt "code"
  val li = elt "li"
  fun ul attrs ls = element "ul" attrs (List.map li ls)
  val td = elt "td"
  val th = elt "th"
  fun tr f attrs ls = element "tr" attrs (List.map f ls)
  fun table attrs titles rows =
      let
        val t = tr th [] titles
        val r = List.map (tr td []) rows
      in element "table" attrs ( t :: r) end
  val footer = element "footer" []
end

datatype html_request = Overview | DisplayJob of id | DisplayQueue of string

local
  open HTML
in

  fun cakeml_commit_link s =
    a (String.concat[cakeml_github,"/commit/",s]) s
  fun hol_commit_link s =
    a (String.concat[hol_github,"/commit/",s]) s
  fun cakeml_pr_link attrs pr branch =
    a_attrs attrs (String.concat[cakeml_github,"/pull/",Substring.string(Substring.triml 1 pr)]) branch
  fun cakeml_pr_link_number pr =
    a (String.concat[cakeml_github,"/pull/",Substring.string(Substring.triml 1 pr)]) (Substring.string pr)

  fun escape_char #"<" = "&lt;"
    | escape_char #">" = "&gt;"
    | escape_char c = if Char.isPrint c orelse Char.isSpace c then String.str c else ""
  val escape = String.translate escape_char

  fun job_link q id =
    let
      val jid = Int.toString id
      val f = OS.Path.concat(q,jid)
      val inp = TextIO.openIn f
      val pr_info = read_job_pr inp
                handle IO.Io _ => cgi_die 500 ["cannot open ",f]
                     | Option => cgi_die 500 [f," has invalid file format"]
      val worker = read_job_worker inp
      val status = read_status inp
      val () = TextIO.closeIn inp
      val typ_attrs = if q = "finished" then status_attrs status else []
      val trim_ends = Substring.triml 1 o Substring.trimr 1 o trim_ws
      val typ_string =
        case pr_info of NONE => a_attrs typ_attrs (String.concat[cakeml_github,"/tree/master"]) "master"
                      | SOME (pr, branch) => cakeml_pr_link typ_attrs pr (escape (Substring.string (trim_ends branch)))
      val inp = TextIO.openIn f
      val {bcml,bhol} = read_bare_snapshot inp
             handle Option => cgi_die 500 [f," has invalid file format"]
      val (head_sha,base_sha) =
          case bcml of Bbr sha => (sha,sha)
                     | Bpr {head_sha,base_sha} => (head_sha,base_sha)
      val last_date = read_last_date inp
      val () = TextIO.closeIn inp
      val ago_string =
        case last_date of NONE => "No date"
        | SOME date => time_ago date
    in
        [ a (String.concat[base_url,"/job/",jid]) jid
        , typ_string
        , ago_string
        , a (String.concat[cakeml_github,"/commit/",head_sha]) (code (String.substring (head_sha,0,7)))
        , a (String.concat[cakeml_github,"/commit/",base_sha]) (code (String.substring (base_sha,0,7)))
        , a (String.concat[hol_github,"/commit/",bhol])        (code (String.substring (bhol,0,7)))
        , code (until_space worker)
        ]
    end

  fun html_job_list lim (q,ids) =
    if List.null ids then []
    else
      let
        val title = element "h2" [] [q," " ,a (String.concat[base_url,"/queue/",q]) "(all)"]
        val table_titles = ["job","branch","time",code "HEAD","merge-base",code "HOL","worker"]
        val id_list = case lim of
                          NONE   => ids
                        | SOME n => List.take (ids,n) handle Subscript => ids
        val table_rows   = List.map (job_link q) id_list
      in [title , table [("class","jobs")] table_titles table_rows]
      end

  fun format_rusage s =
    let
      val timing = String.tokens Char.isSpace s
      val ts = read_secs (List.nth(timing,0))
      val tm = Int.quot(ts,60) val ss = Int.rem(ts,60)
      val hh = Int.quot(tm,60) val mm = Int.rem(tm,60)
      val tK = Option.valOf(Int.fromString(List.nth(timing,1)))
      val tM = Int.quot(tK,1000) val K = Int.rem(tK,1000)
      val tG = Int.quot(tM,1000) val M = Int.rem(tM,1000)
      fun i2 c n =
        String.concat[if n < 10 then String.str c else "", Int.toString n]
      val i2s = i2 #" " and i20 = i2 #"0"
      fun i3s n suffix =
        String.concat [
          if n < 10 then "  "
          else if n < 100 then " " else "",
          Int.toString n, suffix]
    in
      String.concat[
        if hh > 0 then
          String.concat [i2s hh, "h", i20 mm, "m", i20 ss, "s"]
        else if mm > 0 then
          String.concat ["   ", i2s mm, "m", i20 ss, "s"]
        else
          String.concat ["      ", i2s ss, "s"],
        " ",
        if tG > 0 then
          i3s tG "GB"
        else if M > 0 then
          i3s M "MB" else
          i3s K "kB"]
    end handle Subscript => raise Option

  fun process_message s =
    let
      val ss = Substring.full s
      val (msg_br,date_nl) = Substring.splitr (not o equal #"[") ss
      val (date,rest) = ReadJSON.bare_read_date date_nl
      val msg = Substring.trimr 2 msg_br
    in
      (* reversed *)
      ["\n", time_ago date, escape (Substring.string msg)]
    end handle Option => [escape s]

  local open OS.Path in
    fun artefact_link jid f =
      String.concat
        [a (String.concat[host,concat(concat(concat(base base_url,artefacts_dir),jid),f)]) f, "\n"]
  end

  fun process_artefacts jid acc =
    let
      val arts = read_artefacts jid
    in
      if List.null arts then acc
      else
        String.concat (List.map (artefact_link jid) arts) ::
        strong "Artefacts:" ::
        acc
    end

  fun process jid s =
    let
      val inp = TextIO.openString s
      fun read_line () = Option.valOf (TextIO.inputLine inp)
      val prefix = "CakeML: "
      val sha = extract_prefix_trimr prefix (read_line ()) handle Option => cgi_die 500 ["failed to find line ",prefix]
      val acc = [String.concat[strong (trimr prefix),cakeml_commit_link sha,"\n"]]
      val acc = process_message (read_line ()) @ acc
      val line = read_line ()
      val (line,acc) =
        if String.isPrefix "#" line then
          let
            val (pr, branch) = extract_word line
            val line = String.concat[cakeml_pr_link_number pr,
                                     escape (Substring.string branch)]
            val acc = line::acc
            val prefix = "Merging into: "
            val sha = extract_prefix_trimr prefix (read_line ()) handle Option => cgi_die 500 ["failed to find line ",prefix]
            val acc = (String.concat[strong (trimr prefix),cakeml_commit_link sha,"\n"])::acc
            val acc = process_message (read_line ()) @ acc
          in (read_line (), acc) end
        else (line,acc)
      val prefix = "HOL: "
      val sha = extract_prefix_trimr prefix line handle Option => cgi_die 500 ["failed to find line ",prefix]
      val acc = (String.concat[strong (trimr prefix),hol_commit_link sha,"\n"])::acc
      val acc = process_message (read_line ()) @ acc
      exception Return of string list
      val acc =
        let
          val prefix = "Machine: "
          val line = read_line () handle Option => raise (Return acc)
          val name = extract_prefix_trimr prefix line handle Option => raise (Return (line::acc))
          val acc = (String.concat[strong (trimr prefix),escape name,"\n"])::acc
          val line = read_line () handle Option => raise (Return acc)
        in
          if String.size line = 1
          then line :: process_artefacts jid acc
          else raise (Return (line::acc))
        end handle Return acc => escape (TextIO.inputAll inp) :: acc
      fun format_log_line s =
        let
          val prefix = " Finished "
          val rest = extract_prefix_trimr prefix s
          val (dir,rest) = extract_word rest
          val (space,rest) = Substring.splitl Char.isSpace rest
        in
          String.concat[
            prefix,
            Substring.string dir,
            Substring.string space,
            format_rusage (Substring.string rest),
            "\n"]
        end handle Option => escape s
      fun elapsed_time (acc as (dir_part::time_part::rest)) =
          let in let
            val prefix = " Starting "
            val dir = extract_prefix_trimr prefix dir_part
            (* val pad = CharVector.tabulate(max_dir_length - String.size dir,(fn _ => #" ")) *)
            val (l,r) = Substring.splitAt (Substring.full time_part,6)
            val files =
              List.map (fn id => OS.Path.concat("running",Int.toString id)) (running()) @
              List.map (fn id => OS.Path.concat("finished",Int.toString id)) (finished())
            val (t,fs) = timings_of_dir dir files
            val average = if List.null fs then [] else [" ",duration(Int.quot(t,List.length fs))]
            val line = String.concat [
              time_part, prefix, dir, " ",
              Substring.string l, "class='since' ", Substring.string r,
              String.concat average, "\n" ]
          in
            line :: rest
          end handle Option => acc | Subscript => acc end
        | elapsed_time acc = acc
      fun loop acc =
        let
          val line = read_line()
        in
          let
            val (date,rest) = ReadJSON.bare_read_date (Substring.full line)
            val acc = time date :: acc
            val acc = format_log_line (Substring.string rest) :: acc
          in
            loop acc
          end handle Option => escape (TextIO.inputAll inp) :: acc
        end handle Option => elapsed_time acc
    in
      String.concat(List.rev (loop acc)) before
      TextIO.closeIn inp
    end

  fun req_body Overview =
    List.concat
      (ListPair.map (html_job_list (SOME 10))
         (queue_dirs,
          List.map (fn f => f()) queue_funs))
    @ [footer [a host "CakeML main page",
               a "https://github.com/CakeML/regression" "Site code on GitHub",
               a (String.concat["https://validator.w3.org/nu/?doc=",server]) "Valid HTML",
               a (String.concat["https://jigsaw.w3.org/css-validator/validator?uri=",host,style_href]) "Valid CSS"]]
  | req_body (DisplayJob id) =
    let
      val jid = Int.toString id
      val q = queue_of_job jid
      val f = OS.Path.concat(q,jid)
      val s = file_to_string f
    in
      [a base_url "Overview", h3 (a jid (String.concat["Job ",jid])), pre (process jid s)]
    end
  | req_body (DisplayQueue q) =
    let
      val _ = cgi_assert (List.exists (equal q) queue_dirs) 500 ["Unknown queue: ", q]
    in a base_url "Overview" :: html_job_list NONE (q, read_list q ())
    end

  fun html_response req =
    let
      val result = html ([header,body (req_body req)]) (* catch errors first *)
    in
      TextIO.output(TextIO.stdOut, html_response_header);
      TextIO.output(TextIO.stdOut, result)
    end

end

end
