(* A Service file *)

signature SERVICES = sig
  type 'a res
  type ('b,'a) fcn
  val a : (unit,int res)fcn
  val b : (bool * (bool*word) list * real, int res)fcn
end
