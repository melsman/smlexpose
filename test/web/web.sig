(* Copyright 2018, Martin Elsman, MIT-license *)

signature WEB = sig
  val request : unit -> {method:string,data:string}
  val reply   : string -> unit
end
