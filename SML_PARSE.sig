signature SML_PARSE = sig
  exception ParseErr of Region.loc * string
  val parse : (SmlLex.token * Region.reg) list -> SmlAst.sigdec
end
