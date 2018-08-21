(* A Service file *)

signature SERVICES = sig
  type 'a res
  type ('a,'b)fcn
  type a = (int,real)x t
  val b : ({a:int,b:{x:real,y:string,z:bool}list,c:bool},int res)fcn
  val c : (int*real*Z.Y.t,(int,real) X.Y.isodate res) fcn
end
