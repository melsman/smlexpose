structure SmlLex = struct

datatype token = Signature | Sig | Type | And | Val | End | Lpar |
         Rpar | Lbra | Rbra | Eq | Comma | Sub | Arrow | Colon | Star
         | Tyvar of string | Id of string | Dot | Pipe | Digit of char
         | Letter of char | Pling | Gt | Underscore | Newline

(* pr_token : token -> string *)
fun pr_token t =
    case t of
        Signature => "signature"
      | Sig => "sig"
      | Type => "type"
      | And => "and" | Val => "val" | End => "end"
      | Lpar => "("  | Rpar => ")" | Lbra => "{" | Rbra => "}"
      | Eq => "=" | Comma => ","
      | Sub => "-"
      | Arrow => "->" | Colon => ":"
      | Star => "*"
      | Tyvar s => s | Id s => s
      | Dot => "."
      | Pipe => "|"
      | Pling => "'"
      | Gt => ">"
      | Letter c => "L(" ^ String.str c ^ ")"
      | Digit c => "D(" ^ String.str c ^ ")"
      | Underscore => "_"
      | Newline => "\\n"

type filename = Region.filename
type loc = Region.loc
type reg = Region.reg
fun loc0 f : loc = (1,0,f) (* line 1, char 0 *)

datatype state = StartS
               | LparS of loc
               | CommentStarS
               | CommentS
               | SymbS of token * loc * loc   (* for lexing "->" *)
               | IdS of string * loc * loc
               | TvS of string * loc * loc

(* lexChar : char -> token option *)
fun lexChar c =
    case c of
        #"{" => SOME Lbra
      | #"}" => SOME Rbra
      | #"(" => SOME Lpar
      | #")" => SOME Rpar
      | #"." => SOME Dot
      | #"," => SOME Comma
      | #"*" => SOME Star
      | #"=" => SOME Eq
      | #"-" => SOME Sub
      | #":" => SOME Colon
      | #"|" => SOME Pipe
      | #"'" => SOME Pling
      | #">" => SOME Gt
      | #"\n" => SOME Newline
      | c => if Char.isDigit c then SOME(Digit c)
             else if Char.isAlpha c then SOME(Letter c)
             else NONE

(* lexError : loc -> string -> 'a *)
fun lexError loc s =
    let val msg = "Lexical error at location " ^ Region.ppLoc loc ^ ": " ^ s
    in raise Fail msg
    end

fun resolveId s =
    case s of
        "signature" => Signature
      | "sig" => Sig
      | "type" => Type
      | "and" => And
      | "val" => Val
      | "end" => End
      | s => Id s

type procstate = (token * reg) list * state * loc

(* process : word * procstate -> procstate *)
fun process (c,(tokens,state,loc)) =
    let val elem = lexChar c
        fun proc (tokens,state,loc) =
            case (state, elem) of
                (CommentS,      SOME Star)       => (tokens, CommentStarS, Region.next loc)
              | (CommentS,      SOME Newline)    => (tokens, CommentS, Region.newline loc)
              | (CommentS,      _)               => (tokens, CommentS, Region.next loc)
              | (CommentStarS,  SOME Rpar)       => (tokens, StartS, Region.next loc)
              | (CommentStarS,  SOME Star)       => (tokens, CommentStarS, Region.next loc)
              | (CommentStarS,  SOME Newline)    => (tokens, CommentS, Region.newline loc)
              | (CommentStarS,  _)               => (tokens, CommentS, Region.next loc)
              | (StartS,        SOME Lpar)       => (tokens, LparS loc, Region.next loc)
              | (LparS _,       SOME Star)       => (tokens, CommentS, Region.next loc)
              | (LparS l0,      _)               => proc ((Lpar,(l0,l0))::tokens,StartS,loc)

              | (StartS,        SOME (Letter c)) => (tokens, IdS(String.str c,loc,loc), Region.next loc)
              | (IdS(s,l0,_),   SOME (Letter c)) => (tokens, IdS(s ^ String.str c,l0,loc), Region.next loc)
              | (IdS(s,l0,_),   SOME Underscore) => (tokens, IdS(s ^ "_",l0,loc), Region.next loc)
              | (IdS(s,l0,_),   SOME Pling)      => (tokens, IdS(s ^ "'",l0,loc), Region.next loc)
              | (IdS(s,l0,_),   SOME Dot)        => (tokens, IdS(s ^ ".",l0,loc), Region.next loc)
              | (IdS(s,l0,_),   SOME (Digit c))  => (tokens, IdS(s ^ String.str c,l0,loc), Region.next loc)
              | (IdS(s,l0,l1),  _)               => proc ((resolveId s,(l0,l1))::tokens, StartS, loc)

              | (StartS,        SOME Pling)      => (tokens, TvS("'",loc,loc), Region.next loc)
              | (TvS(s,l0,_),   SOME (Letter c)) => (tokens, TvS(s ^ String.str c,l0,loc), Region.next loc)
              | (TvS(s,l0,_),   SOME Underscore) => (tokens, TvS(s ^ "_",l0,loc), Region.next loc)
              | (TvS(s,l0,_),   SOME (Digit c))  => (tokens, TvS(s ^ String.str c,l0,loc), Region.next loc)
              | (TvS(s,l0,l1),  _)               => proc ((Tyvar s,(l0,l1))::tokens, StartS, loc)

              | (StartS,        SOME Sub)        => (tokens, SymbS(Sub,loc,loc), Region.next loc)

              | (SymbS(Sub,l0,_), SOME Gt)       => ((Arrow,(l0,loc))::tokens, StartS, Region.next loc)
              | (SymbS(t,l0,l1), _)              => proc ((t,(l0,l1))::tokens, StartS, loc)

              | (StartS,        SOME Newline)    => (tokens,StartS, Region.newline loc)
              | (StartS,        SOME s)          => ((s,(loc,loc))::tokens,StartS, Region.next loc)
              | (StartS,        NONE)            => if Char.isSpace c then (tokens,state,Region.next loc)
                                                    else lexError loc
                                                                  ("don't know what to do with " ^ String.str c)
    in proc(tokens,state,loc)
    end

(* pr_tokens : token list -> string *)
fun pr_tokens ts = String.concatWith " " (List.map pr_token ts)

(* lex : string -> string -> (token * reg) list *)
fun lex filename s =
    let val s = s^" "  (* pad some whitespace to keep the lexer happy *)
        val (tokens,state,_) = CharVector.foldl process (nil,StartS,Region.loc0 filename) s
    in case state of
           StartS => rev tokens
         | CommentS => raise Fail "lex error - CommentS"
         | _ => raise Fail "lex error"
    end
end
