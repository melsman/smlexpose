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
  val quotes : (ticker, (isodate * real) list res) fcn
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

## Generating the `ClientServices.sml` file

Generating the file `ClientServices.sml` is done by passing the `-c` option to `smlexpose`:

````
$ smlexpose -c SERVICE.sig > ClientServices.sml
````

## Generating the `ServerExposure.sml` file

Generating the file `ServerExposure.sml` is done by passing the `-s` option to `smlexpose`:

````
$ smlexpose -s SERVICE.sig > ServerExposure.sml
````
