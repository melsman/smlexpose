# SMLexpose

SMLexpose is a tool for exposing SMLserver functionality to SMLtoJs
clients. The tool works by parsing an interface (an SML signature) of
a specific form. The interface specifies the functionality to be
exposed by SMLserver in terms of function specifications for the
exposed functions. Here is an example demonstrating the exposure of a
`quotes` function for obtaining stock quotes for a specific stock
ticker (e.g., AAPL):

````
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

- `ClientServices.sml`: This generated file contains a structure to be
  used in the SMLtoJs context. The structure matches the `SERVICES`
  signature with the `'a res` type specified to of type `'a
  Async.Deferred.t`, which allows for asyncronous communication
  between the client and the server.

- `ServerExposure.sml`: This generated file contains a structure
  definining the empty signature. The file can be used as an SMLserver
  script that exposes the functionality available in the structure
  `ServerServices : SERVICES` on the server.
