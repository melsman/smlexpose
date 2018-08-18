
structure SmlAst = struct

structure L = SmlLex

type token = L.token
type reg = Region.reg
type sigid = string
type strid = string
type vid = string
type lab = string
type tv = string
type tc = string

datatype ty = Tv of tv * reg
            | Tc of ty list * tc * reg
            | Rec of (lab*ty) list * reg
            | Arr of ty * ty * reg
            | Tup of ty list * reg

datatype spec = TypeSpec of tv list * tc * ty option * reg
              | ValSpec of vid * ty * reg
              | SeqSpec of spec * spec

datatype sigexp = Sig of spec * reg

datatype sigdec = Sigdec of sigid * sigexp * reg

fun pr_seq sep pr nil acc = acc
  | pr_seq sep pr [e] acc = pr e :: acc
  | pr_seq sep pr es acc = "(" :: String.concatWith sep (List.map pr es) :: ")" :: acc

fun pr_tvs tvs = String.concat(pr_seq "," (fn x => x) tvs nil)

fun pr_tys sep tys acc = pr_seq sep (fn ty => String.concat(pr_ty ty [])) tys acc
and pr_ty ty acc =
    case ty of
        Tv(tv,_) => tv :: acc
      | Tc(nil,tc,_) => tc :: acc
      | Tc(tys,tc,_) => pr_tys "," tys (" " :: tc :: acc)
      | Arr(t1,t2,_) => "(" :: pr_ty t1 (") -> " :: pr_ty t2 acc)
      | Tup(tys,r) => pr_tys "*" tys acc
      | _ => raise Fail "pr_ty"

fun pr_spec (TypeSpec(tvs,tc,NONE,r)) acc =
    "type " :: pr_tvs tvs :: " " :: tc :: "\n" :: acc
  | pr_spec (TypeSpec(tvs,tc,SOME ty,r)) acc =
    "type " :: pr_tvs tvs :: " " :: tc :: " = " :: pr_ty ty ("\n" :: acc)
  | pr_spec (SeqSpec(s1,s2)) acc =
    pr_spec s1 (pr_spec s2 acc)
  | pr_spec (ValSpec(vid,ty,r)) acc =
    "val " :: vid :: " : " :: pr_ty ty ("\n" :: acc)

fun pr_sigexp (Sig(spec,r)) = "sig " :: pr_spec spec [" end"]

fun pr_sigdec (Sigdec(sigid,sigexp,r)) =
    String.concat ("signature " :: sigid :: " = " :: pr_sigexp sigexp)
end
