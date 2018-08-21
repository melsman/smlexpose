(* A Service file *)

signature SERVICES = sig
  type 'a res
  type ('a,'b) fcn
  type x = int * int list
  type s = x
  type 'a t = 'a * 'a list (* sdsd *)
  val a : (int t,int res)fcn
  val b : (int,(real*real)list res) fcn
end
