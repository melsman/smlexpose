(* A Service file *)

signature SERVICES = sig
  type 'a res
  type ('a,'b) fcn  (* hmm *)
  type ticker = string
  type isodate = string
  val quotes : (ticker, (isodate * real) list res) fcn
end
