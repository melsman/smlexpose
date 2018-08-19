
fun die s = raise Fail ("SMLexpose." ^ s)

(* Parsing *)
fun parseFile (flags : Flags.flags) (f : string) : unit =
    let val verbose_p = Flags.flag_p flags "-v"
        val silent_p = Flags.flag_p flags "-silent"
        val _ = Util.log (not silent_p) (fn _ => "[Reading file: " ^ f ^ "]")
        val s = Util.readFile f
        val ts = SmlLex.lex f s
        val _ = Util.log verbose_p (fn _ => "File lexed:")
        val _ = Util.log verbose_p (fn _ => " " ^ SmlLex.pr_tokens (map #1 ts))
        val _ = Util.log verbose_p (fn _ => "Parsing tokens...")
        val sd = SmlParse.parse ts
        val _ = Util.log verbose_p (fn _ => "Parsing tokens done...")
        val _ = Util.log verbose_p (fn _ => "Parse success:\n " ^ SmlAst.pr_sigdec sd)
    in ()
    end

fun errHandler (e : exn) : unit =
    case e of
        SmlParse.ParseErr(l,s) =>
        Util.prln ("Parse Error at " ^
                   Region.ppLoc l ^ ": \n  " ^
                   s)
      | Fail s => Util.prln s
      | _ => raise e

fun run (flags,[f]) =
    let val compile_only_p = Flags.flag_p flags "-c"
        val verbose_p = Flags.flag_p flags "-v"
        val silent_p = Flags.flag_p flags "-silent"
    in parseFile flags f handle e => errHandler e
    end
  | run _ = die "expects only one file"

val name = CommandLine.name()

fun version() =
    String.concatWith "\n"
                      ["SMLexpose version: " ^ Version.version,
                       "Version date: " ^ Version.date,
                       "Platform: " ^ Version.platform]

fun usage() =
    version() ^ "\n\n" ^
    "Usage: " ^ name ^ " [OPTIONS]... file.sig\n" ^
    " -c file        : write client structure \n" ^
    " -s file        : write SMLserver exposer file\n" ^
    " -d file        : write common Defs file\n"

(* Parse command line arguments and pass to compileAndRun *)
val () = Flags.runargs {usage = usage,
                        run = run,
                        unaries = []}