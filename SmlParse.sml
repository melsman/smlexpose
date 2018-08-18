structure SmlParse : SML_PARSE = struct

val p_debug = false
fun debug f =
    if p_debug then print(f())
    else ()

open SmlAst
structure PComb = ParseComb(type token=token
                            val pr_token = SmlLex.pr_token)

type reg = Region.reg
val botreg = (Region.botloc,Region.botloc)

open PComb infix >>> ->> >>- ?? ??? || oo oor

(* p_id : string p *)
fun p_id nil = NO (Region.botloc,fn () => "expecting identifier but found end-of-file")
  | p_id ((L.Id id,r)::ts) = OK(id,r,ts)
  | p_id ((t,r:reg)::_) = NO (#1 r,fn() => ("expecting identifier but found token " ^ SmlLex.pr_token t))

val p_sigid = p_id
val p_tycon = p_id
val p_vid = p_id

(* p_tv : string p *)
fun p_tv nil = NO (Region.botloc,fn () => "expecting type variable but found end-of-file")
  | p_tv ((L.Tyvar tv,r)::ts) = OK(tv,r,ts)
  | p_tv ((t,r:reg)::_) = NO (#1 r,fn() => ("expecting type variable but found token " ^ SmlLex.pr_token t))

(* is_symb : Lexer.token -> bool *)
fun is_symb t =
    case t of
      L.Star => true
    | L.Dot => true
    | L.Comma => true
    | L.Gt => true
    | L.Eq => true
    | L.Pipe => true
    | _ => false

(* p_symb : token p *)
fun p_symb nil = NO (Region.botloc,fn()=>"reached end-of-file")
  | p_symb ((t,r:reg)::ts) =
    if is_symb t then OK(t,r,ts)
    else NO (#1 r,
             fn () => ("expecting symbol but found token " ^
                       SmlLex.pr_token t))

(* SML Parsing *)

(* Grammar:

   sigdec ::= SIGNATURE sigid EQ sigexp

   sigexp ::= SIG specs END

   specs ::= spec < specs >

   spec ::= TYPE tyvarseq tycon
          | VAL vid COLON ty

   tyvarseq ::= tyvar
              | LPAR tyvars RPAR

   tyvars ::= tyvar < COMMA tyvars >

   ty ::= ty1 -> ty1 | ty1

   ty1 ::= ty2 * ... * ty2
         | ty2

   ty2 ::= LPAR tys RPAR tycon
        | ty3 tycons

   ty3 ::= tyvar | tycon | LPAR ty RPAR | LBRA lty RBRA

   tys ::= ty < COMMA tys >

   tycons ::= tycon < tycons >

   lty ::= lab COLON ty < COMMA lty >

*)

fun p_ty ts =
    ((p_ty1 ??? (eat L.Arrow ->> p_ty)) Arr) ts

(*    ((((p_ty1 >>- eat L.Arrow) >>> p_ty1) oor (fn ((t1,t2),r) => Arr(t1,t2,r))) || p_ty1) ts *)

and p_ty1 ts =
    let fun p_tys ts =
            (((p_ty2 oo (fn t => [t])) ??? (eat L.Star ->> p_tys))
                 (fn (ts1,ts2,r) => ts1@ts2)) ts
    in p_tys oor (fn ([t],r) => t | (ts,r) => Tup(ts,r))
    end ts

and p_ty2 ts =
    let fun p_tys ts = (((p_ty oo (fn t => [t])) ??? (eat L.Comma ->> p_tys))
                            (fn (ts1,ts2,r) => ts1@ts2)) ts
        fun p_ptys ts = ((eat L.Lpar ->> p_tys) >>- (eat L.Rpar)) ts
        fun p_tycons ts =
            (((p_tycon oor (fn (t,r) => [(t,r)])) ?? p_tycons) (op@)) ts
        fun build t nil = t
          | build t ((tc,r)::tcs) = build (Tc([t],tc,r)) tcs
    in ((p_ptys >>> p_tycon) oor (fn ((ts,tc),r) => Tc(ts,tc,r)))
    || ((p_ty3 ?? p_tycons) (fn (t,tycons) => build t tycons))
    end ts

and p_ty3 ts =
    ((p_tv oor Tv) ||
     (p_tycon oor (fn (tc,r) => Tc(nil,tc,r))) ||
     ((eat L.Lpar ->> p_ty) >>- (eat L.Rpar))) ts

fun p_spec ts =
    let val p_tspec = (eat L.Type ->> p_tycon)
                          oor (fn (tc,r) => TypeSpec([],tc,NONE,r))
        val p_vspec = ((eat L.Val ->> p_vid) >>> (eat L.Colon ->> p_ty))
                          oor (fn ((vid,ty),r) => ValSpec(vid,ty,r))
    in ((p_tspec || p_vspec) ??? p_spec) (fn (s1,s2,r) => SeqSpec(s1,s2))
    end ts

val p_sigexp : sigexp p =
    (((eat L.Sig ->> p_spec) >>- (eat L.End)) oor Sig)

val p_sigdec : sigdec p =
    ((eat L.Signature ->> p_sigid) >>> (eat L.Eq ->> p_sigexp) oor
                                   (fn ((sigid,sigexp),r) => Sigdec(sigid,sigexp,r)))

exception ParseErr of Region.loc * string
fun parse (ts:(token*reg)list) : sigdec =
    case p_sigdec ts of
      OK (sd,_,_) =>
      let val () = debug (fn () => "AST is\n " ^ pr_sigdec sd ^ "\n")
      in sd
      end
    | NO (l,f) => raise ParseErr (l,f())
end
