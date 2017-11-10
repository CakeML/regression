(*
The API that the server and worker agree on.

Reference:

  waiting:
    returns space-separated list of ids of waiting jobs

  running:
    returns space-separated list of ids of running jobs

  stopped:
    returns space-separated list of ids of stopped jobs

  refresh:
    update the queues according to the current state on GitHub
    returns "refreshed"

  job id:
    returns information on job <id>
    including:
      - commits (including pull request integration, if any)
      - (worker) name and time started (if any)
      - output so far

  claim id name:
    worker <name> claims job <id>.
    <name> is in the query string.
    returns "claimed"
    fails if <id> is not currently waiting

  append id line:
    record <line> as additional output for job <id> with a timestamp.
    <line> is in the query string.
    returns "appended"
    fails if <id> is not currently running

  log id data:
    append <data> as additional output for job <id>.
    <data> is POST data.
    returns "logged"
    fails if <id> is not currently running

  stop id:
    mark job <id> as stopped
    returns "stopped"
    sends email with output
    fails if <id> is not currently running

  retry id:
    create a new job with the same commits as job <id>
    returns "retried as job <new id>"
    fails if <id> is not currently stopped
    fails if there is already a running or waiting job for the same commits

  all failures return text starting with "Error:"
*)
use "utilLib.sml";

structure apiLib = struct

open utilLib

type id = int
type worker_name = string
type line = string

fun check_id f id =
  0 <= id andalso Int.toString id = f

datatype api = Waiting | Running | Stopped | Refresh
             | Job of id | Claim of id * worker_name
             | Append of id * line (* not including newline *)
             | Stop of id | Retry of id

val refresh_response = "refreshed\n"
val claim_response = "claimed\n"
val append_response = "appended\n"
val stop_response = "stopped\n"
val log_response = "logged\n"

fun percent_decode s =
  let
    fun loop ss acc =
      let
        val (chunk,ss) = Substring.splitl (not o equal #"%") ss
      in
        if Substring.isEmpty ss then
          Substring.concat(List.rev(chunk::acc))
        else
          let
            val (ns,ss) = Substring.splitAt(Substring.triml 1 ss,2)
            val n = #1 (Option.valOf (Int.scan StringCvt.HEX Substring.getc ns))
            val c = Substring.full (String.str (Char.chr n))
          in
            loop ss (c::chunk::acc)
          end
      end
  in
    loop (Substring.full s) []
    handle e => (TextIO.output(TextIO.stdErr,String.concat["percent decode failed on ",s,"\n",exnMessage e,"\n"]); raise e)
  end

fun api_to_string Waiting = "/waiting"
  | api_to_string Running = "/running"
  | api_to_string Stopped = "/stopped"
  | api_to_string Refresh = "/refresh"
  | api_to_string (Job id) = String.concat["/job/",Int.toString id]
  | api_to_string (Claim (id,name)) = String.concat["/claim/",Int.toString id]
  | api_to_string (Append (id,line)) = String.concat["/append/",Int.toString id]
  | api_to_string (Stop id) = String.concat["/stop/",Int.toString id]
  | api_to_string (Retry id) = String.concat["/retry/",Int.toString id]

fun api_curl_args (Append (_,line)) = ["--get","--data-urlencode",String.concat["line=",line]]
  | api_curl_args (Claim  (_,name)) = ["--get","--data-urlencode",String.concat["name=",name]]
  | api_curl_args _ = []

fun id_from_string n =
  case Int.fromString n of NONE => NONE
  | SOME id => if check_id n id then SOME id else NONE

fun read_query prefix s =
  case String.tokens (equal #"&") s of [s] =>
    if String.isPrefix (String.concat[prefix,"="]) s then
      SOME (percent_decode (String.extract(s,String.size prefix + 1,NONE)))
    else NONE
  | _ => NONE

fun api_from_string s q =
  if s = "/waiting" then SOME Waiting
  else if s = "/running" then SOME Running
  else if s = "/stopped" then SOME Stopped
  else if s = "/refresh" then SOME Refresh
  else (case String.tokens (equal #"/") s of
    ["job",n] => Option.map Job (id_from_string n)
  | ["claim",n] => Option.mapPartial
                    (fn id => Option.map (fn s => Claim(id,s))
                              (Option.mapPartial (read_query "name") q))
                    (id_from_string n)
  | ["append",n] => Option.mapPartial
                    (fn id => Option.map (fn s => Append(id,s))
                              (Option.mapPartial (read_query "line") q))
                    (id_from_string n)
  | ["stop",n] => Option.map Stop (id_from_string n)
  | ["retry",n] => Option.map Retry (id_from_string n)
  | _ => NONE)

type bare_pr = { head_sha : string, base_sha : string }
datatype bare_integration = Bbr of string | Bpr of bare_pr
type bare_snapshot = { bcml : bare_integration, bhol : string }

fun extract_prefix_trimr prefix line =
  let
    val line = Substring.full line
    val () = if Substring.isPrefix prefix line then () else raise Option
  in
    Substring.string(
      Substring.dropr Char.isSpace
        (Substring.triml (String.size prefix) line))
  end

fun read_bare_snapshot inp =
  let
    fun read_line () = Option.valOf (TextIO.inputLine inp)

    val head_sha = extract_prefix_trimr "CakeML: " (read_line())
    val _ = read_line ()
    val line = read_line ()
    val (line,base_sha) =
      if String.isPrefix "#" line then
        let
          val line = read_line ()
          val _ = read_line ()
        in (read_line(), SOME (extract_prefix_trimr "Merging into: " line)) end
      else (line, NONE)
    val hol_sha = extract_prefix_trimr "HOL: " line
    val () = TextIO.closeIn inp
  in
    { bcml = case base_sha
               of NONE => Bbr head_sha
                | SOME base_sha => Bpr { head_sha = head_sha, base_sha = base_sha }
    , bhol = hol_sha }
  end

end
