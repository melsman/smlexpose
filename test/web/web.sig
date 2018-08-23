(* Copyright 2018, Martin Elsman, MIT-license *)

(* A subset of the SMLserver's Web structure *)

signature WEB = sig
  structure Set : sig
    type set
    val iget : set * string -> string option
  end
  structure Conn : sig
    type set
    val getRequestData : unit -> string
    val returnBinary   : string -> unit
    val headers        : unit -> set
    val add_headers    : string * string -> unit
  end where type set = Set.set
end
