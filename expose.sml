(* Copyright 2018, Martin Elsman, MIT-license *)

structure Expose :
          sig
            type flags = Flags.flags
            val gen_service_defs : {flags:flags,file:string,sigdec:SmlAst.sigdec} -> string
            val gen_client_services : {flags:flags,file:string,sigdec:SmlAst.sigdec} -> string
            val gen_server_exposer : {flags:flags,file:string,sigdec:SmlAst.sigdec} -> string
          end =
struct

structure S = SmlAst

(* ---------------------- *)
(* Utilities              *)
(* ---------------------- *)

fun err r s =
    raise Fail ("Error at : " ^ Region.pp r ^ ": " ^ s)

fun assert b r s = if b then () else err r s

type flags = Flags.flags

infix ==>
fun a ==> b = not a orelse b

fun mapi f xs =
    let fun g n nil = nil
          | g n (x::xs) = f(n,x)::g (n+1) xs
    in g 0 xs
    end

(* ---------------------- *)
(* Service Definitions    *)
(* ---------------------- *)

type env = (S.tc*(S.tv list*S.ty option))list
fun lookup E tc = Util.lookupAlist E tc

fun pp_ty ty = String.concat(S.pr_ty ty nil)

fun subst S ty =
    case ty of
        S.Tv (tv,r) =>
        (case lookup S tv of
               SOME ty => ty
             | NONE => err r ("Type variable " ^ tv ^ " not in scope"))
      | S.Tc(tys,tc,r) => S.Tc(map (subst S) tys,tc,r)
      | S.Rec(xs,r) => S.Rec(map(fn (l,ty) => (l,subst S ty))xs,r)
      | S.Arr(t1,t2,r) => S.Arr(subst S t1,subst S t2,r)
      | S.Tup(ts,r) => S.Tup(map(subst S)ts,r)

fun rea E ty =
    case ty of
        S.Tv _ => ty
      | S.Tc(tys,tc,r) =>
        let val tys = map (rea E) tys
        in case lookup E tc of
               SOME(tvs,NONE) =>
               err r ("Type " ^ tc ^ " is abstract, thus it cannot be exposed")
             | SOME(tvs,SOME ty) =>
               (assert (length tys = length tvs) r "arity mismatch";
                subst (ListPair.zip (tvs,tys)) ty)
             | NONE => S.Tc(tys,tc,r)
        end
      | S.Rec(xs,r) => S.Rec(map(fn (l,ty) => (l,rea E ty))xs,r)
      | S.Arr(t1,t2,r) => S.Arr(rea E t1,rea E t2,r)
      | S.Tup(ts,r) => S.Tup(map(rea E)ts,r)

fun fmt_service_def {method,arg,res} =
    String.concat ["  val ", method, " =\n    {method=", method, ",\n     arg=", arg,
                   ",\n     res=",res, "}"]

fun rec_tup_iso lts =
    let fun f [] = "()"
          | f [(l,_)] = l
          | f ((l1,_)::(l2,t2)::lts) = "(" ^ l1 ^ "," ^ f ((l2,t2)::lts) ^ ")"
    in ("fn " ^ f lts ^ " => {" ^ String.concatWith "," (map (fn (l,_) => l^"="^l) lts) ^ "}",
        "fn {" ^ String.concatWith "," (map #1 lts) ^ "} => " ^ f lts)
    end

fun tup_tup_iso ts =
    let val lts = mapi (fn (i,x) => ("a"^Int.toString i,x)) ts
        fun f [] = "()"
          | f [(l,_)] = l
          | f ((l1,_)::(l2,t2)::lts) = "(" ^ l1 ^ "," ^ f ((l2,t2)::lts) ^ ")"
    in ("fn " ^ f lts ^ " => (" ^ String.concatWith "," (map #1 lts) ^ ")",
        "fn (" ^ String.concatWith "," (map #1 lts) ^ ") => " ^ f lts)
    end

fun pickle ty =
    let fun p ty acc =
            case ty of
                S.Tv (_,r) => err r "type variables are not supported for pickling"
              | S.Arr (_,_,r) => err r "functions cannot be pickled"
              | S.Tc ([],"word",_) => "P.word"::acc
              | S.Tc ([],"word8",_) => "P.word8"::acc
              | S.Tc ([],"word32",_) => "P.word32"::acc
              | S.Tc ([],"int",_) => "P.int"::acc
              | S.Tc ([],"int32",_) => "P.int32"::acc
              | S.Tc ([],"bool",_) => "P.bool"::acc
              | S.Tc ([],"string",_) => "P.string"::acc
              | S.Tc ([],"char",_) => "P.char"::acc
              | S.Tc ([],"real",_) => "P.real"::acc
              | S.Tc ([],"unit",_) => "P.unit"::acc
              | S.Tc ([ty],"list",_) => "P.listGen (" :: p ty (")"::acc)
              | S.Tc ([ty],"option",_) => "P.optionGen (" :: p ty (")"::acc)
              | S.Tc ([ty],"vector",_) => "P.vectorGen (" :: p ty (")"::acc)
              | S.Tc (tys,tc,r) =>
                let val ids = String.tokens(fn c => c = #".") tc
                in if length ids < 2 then
                     err r ("type " ^ pp_ty ty ^ " cannot be pickled")
                   else let val path = List.rev(List.tl(List.rev ids))
                            val t = List.hd(List.rev ids)
                            val path = String.concatWith "." path
                            val pu = if t = "t" then path ^ ".pu"
                                     else path ^ ".pu_" ^ t
                            fun p_s nil acc = acc
                              | p_s [t] acc = p t acc
                              | p_s (t::ts) acc = p t (","::p_s ts acc)
                        in case tys of
                               nil => pu :: acc
                             | _ => pu :: "(" :: p_s tys (")"::acc)
                        end
                end
              | S.Tup (tys,r) =>
                let val (f,g) = tup_tup_iso tys
                in "P.convert0(" :: f :: "," :: g :: ")(" :: p_tup tys (")"::acc)
                end
              | S.Rec(lts,r) =>
                let val tys = map #2 lts
                    val (f,g) = rec_tup_iso lts
                in "P.convert0(" :: f :: "," :: g :: ")(" :: p_tup tys (")"::acc)
                end
        and p_tup tys acc =
            case tys of
                nil => "P.unit"::acc
              | [ty] => p ty acc
              | ty::tys => "P.pairGen0("::p ty (","::p_tup tys (")"::acc))
    in String.concat(p ty nil)
    end

fun template_service_defs_funct body = String.concatWith "\n" [
"functor ServiceDefs (P:PICKLE) : SERVICES where type 'a res = 'a",
"                                            and type ('a,'b) fcn = {method:string,",
"                                                                    arg:'a P.pu,",
"                                                                    res:'b P.pu} =",
"struct",
body,
"end\n"
]

fun template_service_defs_struct body = String.concatWith "\n" [
"structure ServiceDefs : SERVICES where type 'a res = 'a",
"                                   and type ('a,'b) fcn = {method:string,",
"                                                           arg:'a Pickle.pu,",
"                                                           res:'b Pickle.pu} =",
"struct",
"  structure P = Pickle",
body,
"end\n"
]

datatype service_def = TypeDef of {tc:string,tvs:string list,ty:string}
                     | ValDef of {method:string,arg:string,res:string}

fun pp_service_def sd =
    case sd of
        TypeDef {tc,tvs,ty} =>
        let val tvs = S.pr_tvs tvs
            val tvs = if tvs = "" then "" else tvs ^ " "
        in String.concat ["  type ",tvs, tc, " = ", ty]
        end
      | ValDef {method,arg,res} =>
        String.concat ["  val ",method," = \n",
                       "    {method=\"",method,"\",\n",
                       "     arg=", arg, ",\n",
                       "     res=", res, "}"]

fun gen_service_defs {flags:flags,file:string,sigdec:S.sigdec} =
    let fun g_spec (E:env, S.TypeSpec(tvs,tc,SOME ty,r)) : env * service_def list =
            let val sd = TypeDef{tc=tc,tvs=tvs,ty=pp_ty ty}
            in ([(tc,(tvs,SOME(rea E ty)))], [sd])
            end
          | g_spec (E:env, S.TypeSpec(tvs,tc,NONE,r)) : env * service_def list =
            (assert (tc = "res" ==> length tvs = 1) r "abstract type constructor 'res' must have arity 1";
             assert (tc = "fcn" ==> length tvs = 2) r "abstract type constructor 'fcn' must have arity 2";
             assert (tc = "res" ==> lookup E "res" = NONE) r "type constructor 'res' already defined";
             assert (tc = "fcn" ==> lookup E "fcn" = NONE) r "type constructor 'fcn' already defined";
             let val sd = if tc = "res" then TypeDef{tc=tc,tvs=["'a"],ty="'a"}
                          else if tc = "fcn" then TypeDef{tc=tc,tvs=["'a","'b"],
                                                          ty="{method:string,arg:'a P.pu,res:'b P.pu}"}
                          else err r "only type constructors 'fcn' and 'res' may be abstract"
             in ([(tc,(tvs,NONE))], [sd])
             end)
          | g_spec (E, S.ValSpec(vid,ty,r)) =
            (case ty of
                 S.Tc([arg,S.Tc([res],tc_res,r_res)],tc,r) =>
                 (assert (tc = "fcn") r "a service must be of type 'fcn'";
                  assert (tc_res = "res") r "the second parameter to 'fcn' must be a 'res' type";
                  let val sd = ValDef{method=vid,arg=pickle (rea E arg),res=pickle (rea E res)}
                  in ([],[sd])
                  end)
               | _ => err r "a service must be of type 'fcn' with the second parameter a 'res' type")
          | g_spec (E, S.SeqSpec(s1,s2)) =
            let val (E1,r1) = g_spec (E,s1)
                val (E2,r2) = g_spec (E1@E,s2)
            in (E2@E1,r1@r2)
            end
        fun g_sigexp (S.Sig(spec,r)) = g_spec (nil,spec)
        fun g_sigdec (S.Sigdec(sigid,sigexp,r)) =
            (assert (sigid = "SERVICES") r "expecting signature id SERVICES";
             g_sigexp sigexp)
        val (_, xs) = g_sigdec sigdec
        val body = String.concatWith "\n" (map pp_service_def xs)
        val struct_p = Flags.flag_p flags "-struct"
    in if struct_p then template_service_defs_struct body
       else template_service_defs_funct body
    end

(* ---------------------- *)
(* ClientServices functor *)
(* ---------------------- *)

fun template_client_services body = String.concatWith "\n" [
    "functor ClientServices (structure P : PICKLE",
    "                        structure Async : ASYNC",
    "                        val url : string",
    "	                      ): SERVICES where type 'a res = 'a Async.Deferred.t",
    "                                     and type ('a,'b)fcn = 'a -> 'b =",
    "struct",
    "  structure ServiceDefs = ServiceDefs(P)",
    "  structure Deferred = Async.Deferred",
    "  type 'a res = 'a Deferred.t",
    "  type ('a,'b) fcn = 'a -> 'b",
    "",
    "  fun mk_service (sd: ('a,'b)ServiceDefs.fcn) : ('a,'b res)fcn =",
    "      let val {method,arg,res} = sd",
    "          val op >>= = Deferred.Infix.>>= infix >>=",
    "       in fn a =>",
    "              Async.delay (fn () => P.pickle arg a) >>= (fn body =>",
    "              Async.httpRequest {binary=true,",
    "                                 method=method,",
    "                                 url=url,",
    "                                 headers=[],",
    "                                 body=SOME body} >>= (fn r =>",
    "              Async.delay (fn () => P.unpickle res r)))",
    "      end",
    "",
    "  (* services *)",
    body,
    "end",
    ""
    ]

datatype entry = Vid of S.vid*Region.reg
               | Typ of S.tv list * S.tc * S.ty option * Region.reg

fun pp_client_service (Vid(vid,r)) =
    (assert (vid <> "mk_service") r "service cannot be named 'mk_service'";
     SOME("  val " ^ vid ^ " = mk_service ServiceDefs." ^ vid))
  | pp_client_service (Typ(tvs,tc,SOME ty,r)) =
    (assert (tc <> "res" andalso tc <> "fcn") r "res and fcn must be abstract";
     let val tvs = S.pr_tvs tvs
         val tvs = if tvs <> "" then tvs ^ " " else tvs
     in SOME(String.concat("  type " :: tvs :: tc :: " = " :: S.pr_ty ty nil))
     end)
  | pp_client_service (Typ(tvs,tc,NONE,r)) =
    (assert (tc = "res" orelse tc = "fcn") r "only res and fcn may be abstract";
     NONE)

fun services (sigdec: S.sigdec) : entry list =
    let fun g_spec (S.TypeSpec x,acc) = Typ x :: acc
          | g_spec (S.ValSpec(vid,_,r),acc) = Vid(vid,r) :: acc
          | g_spec (S.SeqSpec(s1,s2),acc) = g_spec(s1,g_spec (s2,acc))
        fun g_sigexp (S.Sig(spec,r)) = g_spec (spec,nil)
        fun g_sigdec (S.Sigdec(sigid,sigexp,r)) =
            (assert (sigid = "SERVICES") r "expecting signature id SERVICES";
             g_sigexp sigexp)
    in g_sigdec sigdec
    end

fun gen_client_services {flags:flags,file:string,sigdec:S.sigdec} =
    let val entries = services sigdec
        val body = String.concatWith "\n" (List.mapPartial pp_client_service entries)
    in template_client_services body
    end

(* -------------------- *)
(* ServerExpose functor *)
(* -------------------- *)

fun template_server_expose body = String.concatWith "\n" [
    "  fun wrap (sd : ('a,'b)ServiceDefs.fcn) (f:'a -> 'b) : string -> string =",
    "      P.pickle (#res sd) o f o P.unpickle (#arg sd)",
    "",
    "  fun exposeServices () =",
    "      let val {method,data,...} = Web.request()",
    "          val f : string -> string =",
    "              case method of",
    body,
    "                _ => raise Fail (\"unknown service: \" ^ method)",
    "      in Web.reply(f data)",
    "      end"
    ]

fun template_server_expose_funct body = String.concatWith "\n" [
    "functor ServerExpose(structure Pickle : PICKLE",
    "                     structure Web : WEB",
    "                     structure Services: SERVICES where type 'a res = 'a",
    "                                                    and type ('a,'b) fcn = 'a -> 'b",
    "                    ) : sig val exposeServices: unit -> unit",
    "                        end =",
    "struct",
    "  structure P = Pickle",
    "  structure ServiceDefs = ServiceDefs(P)",
    "",
    template_server_expose body,
    "end",
    ""
    ]

fun template_server_expose_struct body = String.concatWith "\n" [
    "structure ServerExpose : sig val exposeServices: unit -> unit",
    "                         end =",
    "struct",
    "  structure P : PICKLE = Pickle",
    "  structure Services : SERVICES where type 'a res = 'a",
    "                                  and type ('a,'b) fcn = 'a -> 'b = Services",
    "  structure ServiceDefs : SERVICES where type 'a res = 'a",
    "                                     and type ('a,'b) fcn = {method:string,",
    "                                                             arg:'a P.pu,",
    "                                                             res:'b P.pu} = ServiceDefs",
    "",
    template_server_expose body,
    "end",
    ""
    ]


fun pp_server_expose (vid,r) =
    "                \"" ^ vid ^ "\" => wrap ServiceDefs." ^ vid ^ " Services." ^ vid ^ " |"

fun gen_server_exposer {flags:flags,file:string,sigdec:S.sigdec} =
    let val entries = services sigdec
        val vids = List.mapPartial (fn Vid x => SOME x | _ => NONE) entries
        val body = String.concatWith "\n" (map pp_server_expose vids)
        val struct_p = Flags.flag_p flags "-struct"
    in if struct_p then template_server_expose_struct body
       else template_server_expose_funct body
    end

end
