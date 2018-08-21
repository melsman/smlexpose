(* Copyright 2018, Martin Elsman, MIT-license *)

signature SML_PARSE = sig
  exception ParseErr of Region.loc * string
  val parse : (SmlLex.token * Region.reg) list -> SmlAst.sigdec
end
