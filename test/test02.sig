(* A Service file *)

signature SERVICES = sig
  type 'a res
  type ('a,'b) res2
  type x = int * int res
  type s = u
  type 'a t = 'a * 'a list (* sdsd *)
  val a : int res
  val b : (int,(real*real)list) res2
end
