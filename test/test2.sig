(* A Service file *)

signature SERVICES = sig
  type 'a res
  type ('a,'b) fcn  (* ~ 'a -> 'b *)
  type ticker = string
  type isodate = string
  val quotes : (ticker, {date:isodate,eod:real,avg:real} list res) fcn
end
