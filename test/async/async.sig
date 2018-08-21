(* Copyright 2018, Martin Elsman, MIT-license *)

signature ASYNC = sig
  structure Deferred : DEFERRED

  val usec : int -> unit Deferred.t

  val delay : (unit -> 'a) -> 'a Deferred.t

  val httpRequest : {method: string,
                     url: string,
                     binary: bool,
                     headers: (string*string)list,
                     body: string option} -> string Deferred.t
end
