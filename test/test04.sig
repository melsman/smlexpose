(* A Service file *)

signature SERVICES = sig
  type 'a res
  type ('a,'b)fcn
  type ('a,'b)x = 'b*'a
  type 'a t = 'a * 'a
  type a = (int,real)x t
  val b : ({a:int,b:{x:real,y:string,z:bool}list,c:bool},int res)fcn
  val c : (int*real*a,(int,real) x res) fcn
end
