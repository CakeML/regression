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

  fun pairList [] = []
    | pairList [_] = raise Match
    | pairList (x::y::ls) = (x,y)::(pairList ls)

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

  fun Word8_fromChar c = Word8.fromInt (Char.ord c)
  fun Word8Vector_fromCharVector vec =
    Word8Vector.tabulate(CharVector.length vec,
      (fn i => Word8_fromChar (CharVector.sub(vec, i))))

  fun Char_fromWord8 b = Char.chr (Word8.toInt b)
  fun CharVector_fromWord8Vector vec =
    CharVector.tabulate(Word8Vector.length vec,
      (fn i => Char_fromWord8 (Word8Vector.sub(vec, i))))

  fun scgi_format_env body =
    let
      val alist = pairList(String.fields (equal (Char.chr 0)) body)
      val len = Option.valOf(Int.fromString(assoc "CONTENT_LENGTH" alist))
      fun f (k,v) = String.concat[k, "=", v]
    in
      (List.map f alist, len)
    end

  exception SocketClosed

  fun make_listener port =
    let
      val socket = INetSock.TCP.socket ()
      val sock_addr = INetSock.any port
      val () = Socket.bind (socket, sock_addr)
      val () = Socket.listen (socket, 5)
    in
      socket
    end

  fun sendVec (socket, vec) =
    let
      fun loop slice =
        if Word8VectorSlice.isEmpty slice then ()
        else let
          val sent = Socket.sendVec(socket, slice)
          val slice = Word8VectorSlice.subslice(slice, sent, NONE)
        in loop slice end
    in loop (Word8VectorSlice.full vec) end

  fun sendStr socket str = sendVec (socket, Word8Vector_fromCharVector str)

  fun recvVecN (socket, n) =
    let
      val arr = Word8Array.array(n, 0wx0)
      fun loop (slice, received) =
        let
          val more = Socket.recvArr(socket, slice)
          val received = received + more
        in
          if more = 0 then raise SocketClosed
          else if n <= received then Word8Array.vector arr
          else loop (Word8ArraySlice.subslice(slice, received, NONE), received)
        end
    in
      if n = 0 then Word8Array.vector arr
      else loop (Word8ArraySlice.full arr, 0)
    end

  fun recvVecUntil (socket, f) =
    let
      fun loop (acc, i) =
        let
          val v = Socket.recvVec(socket, 1)
        in
          if Word8Vector.length v = 0 then raise SocketClosed
          else if f (Word8Vector.sub(v, 0)) then
            Word8ArraySlice.vector(Word8ArraySlice.slice(acc, 0, SOME i))
          else
            if i < Word8Array.length acc  then
              (Word8Array.update(acc, i, Word8Vector.sub(v, 0));
               loop (acc, i+1))
            else
              let
                fun t j = if j < i then Word8Array.sub(acc, j)
                          else if j = i then Word8Vector.sub(v, 0)
                          else 0wx0
              in
                loop (Word8Array.tabulate(Word8Array.length acc * 2, t), i+1)
              end
        end
    in
      loop (Word8Array.array(1, 0wx0), 0)
    end

  fun recvNetstring socket =
    let
      val len = Option.valOf (
                  Int.fromString(
                    CharVector_fromWord8Vector(
                      recvVecUntil (socket, equal (Word8_fromChar#":")))))
      val str = CharVector_fromWord8Vector(
                  recvVecN (socket, len))
    in
      str before
        (if Word8Vector.sub(recvVecN (socket, 1), 0) = (Word8_fromChar#",")
         then () else raise Fail "recvNetstring: no trailing comma")
    end handle Option => raise Fail "recvNetstring: bad length"

  fun file_to_string f =
    let val inp = TextIO.openIn f in TextIO.inputAll inp before TextIO.closeIn inp end

  fun output_to_file (f,s) =
    let
      val out = TextIO.openOut f
      val () = TextIO.output(out,s)
    in TextIO.closeOut out end

  val curl_path = "/usr/bin/curl"

  fun up p = OS.Path.concat(OS.Path.parentArc,p)

  fun trimr s =
    Substring.string(Substring.dropr Char.isSpace (Substring.full s))

  fun extract_prefix_trimr prefix line =
    let
      val line = Substring.full line
      val () = if Substring.isPrefix prefix line then () else raise Option
    in
      Substring.string(
        Substring.dropr Char.isSpace
          (Substring.triml (String.size prefix) line))
    end

  val extract_word =
    Substring.splitl (not o Char.isSpace) o Substring.full

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
