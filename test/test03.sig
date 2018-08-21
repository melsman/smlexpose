(* A Service file *)

signature SERVICES = sig
  type 'a res
  type ('a,'b)fcn
  type 'a t = 'a
  val x : ((real*real) t t,int res)fcn
end
