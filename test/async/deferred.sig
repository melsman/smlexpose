(* Copyright 2018, Martin Elsman, MIT-license *)

(** Generic library for working with deferred values.

The DEFERRED signature specifies basic generic operations for creating
and operating on deferred values, values that can be set once and
which other deferred values can listen to. The library is useful for,
among other thing, composing asyncronous operations in concurrent
settings.

*)

signature DEFERRED = sig
  type 'a t
  val peek   : 'a t -> 'a option
  val ret    : 'a -> 'a t
  val bind   : 'a t * ('a -> 'b t) -> 'b t
  val map    : 'a t * ('a -> 'b) -> 'b t
  val both   : 'a t * 'b t -> ('a * 'b) t
  val any    : 'a t list -> 'a t
  val all    : 'a t list -> 'a list t
  val throw  : exn -> 'a t

  val upon   : 'a t -> ('a -> unit) -> (exn -> unit) -> unit
  val set    : 'a t -> 'a -> unit
  val setexn : 'a t -> exn -> unit
  val new    : unit -> 'a t

  structure Infix : sig
    val >>=  : 'a t * ('a -> 'b t) -> 'b t
    val >>|  : 'a t * ('a -> 'b) -> 'b t
  end
end

(**

[any ds] returns a deferred value that will be filled whenever one of
the deferred values in ds is filled. If one of ds is filled with an
exception before another is filled with a value, the returned deferred
value is filled with the exception. The result of `any[]` is identical
to the result of `new()`.

*)
