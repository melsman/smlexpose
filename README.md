# SMLexpose

SMLexpose is a tool for exposing SMLserver functionality to SMLtoJs
clients. The tool works by parsing an interface (an SML signature) of
a specific form. The interface specifies the functionality to be
exposed by SMLserver in terms of function specifications for the
exposed functions. Here is an example demonstrating the exposure of a
`quotes` function for obtaining stock quotes for a specific stock
ticker (e.g., AAPL):

````sml
signature SERVICES = sig
  type 'a res
  type ('a,'b) fcn  (* ~ 'a -> 'b *)
  type ticker = string
  type isodate = string
  val quotes : (ticker, {date:isodate,eod:real,avg:real} list res) fcn
end
````

From this service specification, SMLexpose may generate three functors
(optionally structures) to be integrated in the SMLserver and SMLtoJs
build projects:

- `ServiceDefs`: This generated functor takes as argument a `Pickle`
  structure and results in a structure for use both in the SMLserver
  context and in the SMLtoJs context. The structure matches the
  `SERVICES` signature and defines how argument values and result
  values are serialised and deserialised in terms of serialisation
  picklers.

- `ClientServices`: This generated functor takes as argument a
  `Pickle` structure, an `Async` structure, matching the `ASYNC`
  signature, and an `url` address for the server entry point. The
  functor returns a structure to be used in the SMLtoJs context. The
  structure matches the `SERVICES` signature with the `'a res` type
  specified to of type `'a Async.Deferred.t`, which allows for
  asyncronous communication between the client and the server.

- `ServerExpose`: This generated functor takes as argument a `Pickle`
  structure, a `Web` structure, and a `Services` structure that
  implements the `SERVICES` signature. The functor returns a structure
  that contains a single variable declaration `exposeServices: unit ->
  unit`.  The function can be used in an SMLserver script that exposes
  the functionality available in the structure `Services : SERVICES`
  on the server.

## Generating the `ServiceDefs` functor

Generating the functor `ServiceDefs` is done by passing the `-d` option to `smlexpose`:

````
$ smlexpose -d SERVICES.sig > ServiceDefs.sml
````

Here is the content (slightly prettified) of the generated file:

````sml
functor ServiceDefs (P:PICKLE) : SERVICES where type 'a res = 'a
                                            and type ('a,'b) fcn = {id:string,
                                                                    arg:'a P.pu,
                                                                    res:'b P.pu} =
struct
  type 'a res = 'a
  type ('a,'b) fcn = {id:string,arg:'a P.pu,res:'b P.pu}
  type ticker = string
  type isodate = string
  val quotes =
    {id="quotes",
     arg=P.string,
     res=P.listGen (P.convert0(fn (date,(eod,avg)) => {date=date,eod=eod,avg=avg},fn {date,eod,avg} => (date,(eod,avg)))(P.pairGen0(P.string,P.pairGen0(P.real,P.real))))}
end
````

## Generating the functor `ClientServices`:

Generating the functor `ClientServices` is done by passing the `-c`
option to `smlexpose` together with an appropriate (relative) url
address (specified with the `-url` option to `smlexpose`):

````
$ smlexpose -c -url "expose_services.sml" SERVICES.sig > ClientServices.sml
````

Here is the content of the generated file:

````sml
functor ClientServices (structure P : PICKLE
                        structure Async : ASYNC
                       ): SERVICES where type 'a res = 'a Async.Deferred.t
                                     and type ('a,'b)fcn = 'a -> 'b =
struct
  structure ServiceDefs = ServiceDefs(P)
  structure Deferred = Async.Deferred
  type 'a res = 'a Deferred.t
  type ('a,'b) fcn = 'a -> 'b

  val url = "expose_services.sml"

  fun mk_service (sd: ('a,'b)ServiceDefs.fcn) : ('a,'b res)fcn =
      let val {id,arg,res} = sd
          val op >>= = Deferred.Infix.>>= infix >>=
       in fn a =>
              Async.httpRequest {binary=true,
                                 method="POST",
                                 url=url,
                                 headers=[("SML-serviceid",id)],
                                 body=SOME(P.pickle arg a)} >>= (fn r =>
              Deferred.ret (P.unpickle res r))
      end

  (* services *)
  type ticker = string
  type isodate = string
  val quotes = mk_service ServiceDefs.quotes
end
````

## Generating the functor `ServerExpose` file

Generating the functor `ServerExpose` is done by passing the `-s` option to `smlexpose`:

````
$ smlexpose -s SERVICES.sig > ServerExpose.sml
````

Here is the content of the generated file:

````sml
functor ServerExpose(structure Pickle : PICKLE
                     structure Web : WEB
                     structure Services: SERVICES where type 'a res = 'a
                                                    and type ('a,'b) fcn = 'a -> 'b
                    ) : sig val exposeServices: unit -> unit
                        end =
struct
  structure P = Pickle
  structure ServiceDefs = ServiceDefs(P)

  fun wrap (sd : ('a,'b)ServiceDefs.fcn) (f:'a -> 'b) : string -> string =
      P.pickle (#res sd) o f o P.unpickle (#arg sd)

  fun exposeServices () =
      let val headers = Web.Conn.headers()
          val id = case Web.Set.iget(headers,"SML-serviceid") of
                       NONE => raise Fail ("ServerExpose: No SML-serviceid header set")
                     | SOME id => id
          val data = Web.Conn.getRequestData()
          val f : string -> string =
              case id of
                "quotes" => wrap ServiceDefs.quotes Services.quotes |
                _ => raise Fail ("ServerExpose: Unknown service: " ^ id)
      in Web.Conn.returnBinary(f data)
      end
end
````

The `exposeServices` function should be called in an SMLserver
script. The Web structure is compatible with the SMLserver Web
structure.

Instead of generating functors, SMLexpose may generate open structures
instead, which is enabled by passing the flag `-struct` to
`smlexpose`. This feature is essential when using SMLexpose's support
for inferring the location of a pickler based on the type
constructor. For qualified (long) identifiers, SMLexpose uses the
following strategy for determining the location of a pickler:

- For a type `Module.t`, it is assumed that a pickler (of type
  `Module.t Pickle.pu`) is available as `Module.pu`.

- For a type `Module.tycon` (with `tycon` different from `t`), it is
  assumed that a pickler (of type `Module.tycon Pickle.pu`) is
  available as `Module.pu_tycon`.

## Building and Testing

To build, test, and install SMLexpose, simply execute the following commands in
a terminal:

````
$ make
$ make test
$ sudo make install
````

The test will, for a number of `SERVICES` signatures, type check (and
compile) the generated modules against interfaces for pickling
(`test/pickle/pickle.sig`), SMLtoJs asynchronous client-server
communication (`test/async/async.sig`), and SMLserver integration
(`test/web/web.sig`).

## License

MIT license.