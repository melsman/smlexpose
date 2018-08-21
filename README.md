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

From this service specification, SMLexpose may generate three files to
be integrated in the SMLserver and SMLtoJs build projects:

- `ServiceDefs.sml`: This generated file contains information for use
  both in the SMLserver context and in the SMLtoJs context, in terms
  of a structure matching the `SERVICES` signature. The structure
  defines how argument values and result values are serialised and
  deserialised in terms of serialisation picklers.

- `ClientServices.sml`: This generated file contains a
  `ClientServices` functor that takes as argument a `Pickle`
  structure, an `Async` structure, matching the `ASYNC` signature, and
  an `url` address for the server entry point. The functor returns a structure to be
  used in the SMLtoJs context. The structure matches the `SERVICES`
  signature with the `'a res` type specified to of type `'a
  Async.Deferred.t`, which allows for asyncronous communication
  between the client and the server.

- `ServerExpose.sml`: This generated file contains a `ServerExpose`
  functor that takes as argument a `Pickle` structure, a `Web`
  structure, and a `Services` structure that implements the `SERVICES`
  signature. The functor returns a structure that contains a single
  variable declaration `exposeServices: unit -> unit`.  The function
  can be used in an SMLserver script that exposes the functionality
  available in the structure `Services : SERVICES` on the server.

## Generating the `ServiceDefs.sml` file

Generating the file `ServiceDefs.sml` is done by passing the `-d` option to `smlexpose`:

````
$ smlexpose -d SERVICE.sig > ServiceDefs.sml
````

Here is the content (slightly prettified) of the generated file:

````sml
functor ServiceDefs (P:PICKLE) : SERVICES where type 'a res = 'a
                                            and type ('a,'b) fcn = {method:string,
                                                                    arg:'a P.pu,
                                                                    res:'b P.pu} =
struct
  type 'a res = 'a
  type ('a,'b) fcn = {method:string,arg:'a P.pu,res:'b P.pu}
  type ticker = string
  type isodate = string
  val quotes =
    {method="quotes",
     arg=P.string,
     res=P.listGen (P.convert0(fn (date,(eod,avg)) => {date=date,eod=eod,avg=avg},
                               fn {date,eod,avg} => (date,(eod,avg)))
			       (P.pairGen0(P.string,P.pairGen0(P.real,P.real))))}
end
````

## Generating the `ClientServices.sml` file

Generating the file `ClientServices.sml` is done by passing the `-c` option to `smlexpose`:

````
$ smlexpose -c SERVICE.sig > ClientServices.sml
````

Here is the content of the generated file:

````sml
functor ClientServices (structure P : PICKLE
                        structure Async : ASYNC
                        val url : string
                         ): SERVICES where type 'a res = 'a Async.Deferred.t
                                     and type ('a,'b)fcn = 'a -> 'b =
struct
  structure ServiceDefs = ServiceDefs(P)
  structure Deferred = Async.Deferred
  type 'a res = 'a Deferred.t
  type ('a,'b) fcn = 'a -> 'b

  fun mk_service (sd: ('a,'b)ServiceDefs.fcn) : ('a,'b res)fcn =
      let val {method,arg,res} = sd
          val op >>= = Deferred.Infix.>>= infix >>=
       in fn a =>
              Async.delay (fn () => P.pickle arg a) >>= (fn body =>
              Async.httpRequest {binary=true,
                                 method=method,
                                 url=url,
                                 headers=[],
                                 body=SOME body} >>= (fn r =>
              Async.delay (fn () => P.unpickle res r)))
      end

  (* services *)
  type ticker = string
  type isodate = string
  val quotes = mk_service ServiceDefs.quotes
end
````

## Generating the `ServerExpose.sml` file

Generating the file `ServerExpose.sml` is done by passing the `-s` option to `smlexpose`:

````
$ smlexpose -s SERVICE.sig > ServerExpose.sml
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
      let val {method,data,...} = Web.request()
          val f : string -> string =
              case method of
                "quotes" => wrap ServiceDefs.quotes Services.quotes |
                _ => raise Fail ("unknown service: " ^ method)
      in Web.reply(f data)
      end
end
````

The `exposeServices` function should be called in an SMLserver
script. The Web structure should match the following signature:

````sml
signature WEB = sig
  val request : unit -> {method:string,data:string}
  val reply   : string -> unit
end
```

It should be fairly simple to construct an appropriate SMLserver
structure that matches this signature.