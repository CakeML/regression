(*
  Small library of useful code.
*)
structure utilLib = struct

  fun equal x y = x = y

  fun find f (x::xs) = if f x then x else find f xs
    | find _ _ = raise Match

  fun assoc k [] = raise Match
    | assoc k ((k',v)::ls) = if k = k' then v else assoc k ls

  fun insert x [] = [x]
    | insert x (y::xs) =
      if x >= y then x::y::xs
      else y::(insert x xs)

  val until_space =
    Substring.string o Substring.takel (not o Char.isSpace) o Substring.full

  fun month_from_int 1 = Date.Jan
    | month_from_int 2 = Date.Feb
    | month_from_int 3 = Date.Mar
    | month_from_int 4 = Date.Apr
    | month_from_int 5 = Date.May
    | month_from_int 6 = Date.Jun
    | month_from_int 7 = Date.Jul
    | month_from_int 8 = Date.Aug
    | month_from_int 9 = Date.Sep
    | month_from_int 10 = Date.Oct
    | month_from_int 11 = Date.Nov
    | month_from_int 12 = Date.Dec
    | month_from_int _ = raise Match

  fun file_to_string f =
    let val inp = TextIO.openIn f in TextIO.inputAll inp before TextIO.closeIn inp end

  fun output_to_file (f,s) =
    let
      val out = TextIO.openOut f
      val () = TextIO.output(out,s)
    in TextIO.closeOut out end

  val curl_path = "/usr/bin/curl"

  fun extract_prefix_trimr prefix line =
    let
      val line = Substring.full line
      val () = if Substring.isPrefix prefix line then () else raise Option
    in
      Substring.string(
        Substring.dropr Char.isSpace
          (Substring.triml (String.size prefix) line))
    end

  fun extract_word s =
    let val (s1,s2) = Substring.splitl (not o Char.isSpace) (Substring.full s)
    in (s1, Substring.string s2) end

  local
    open Unix
  in
    fun system_output die (cmd,args) =
      let
        val proc = execute (cmd,args)
                   handle e as OS.SysErr _ =>
                     (die[cmd," failed to execute on ",String.concatWith" "args,"\n",exnMessage e];
                      raise e) (* re-raise because SML does not support first-class polymorphism *)
        val output = TextIO.inputAll (textInstreamOf proc)
        val status = reap proc
      in
        if OS.Process.isSuccess status then output
        else die[cmd," failed on ",String.concatWith" "args]
      end
  end

  local
    val chunk_size = 65536
  in
    fun outputN_from_stdIn (out,len) =
      let
        fun loop len =
          if chunk_size < len then
            (TextIO.output(out,TextIO.inputN(TextIO.stdIn,chunk_size));
             loop (len - chunk_size))
          else
            TextIO.output(out,TextIO.inputN(TextIO.stdIn,len))
      in loop len end
  end

end
