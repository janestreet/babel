(** This example contains three different versioned protocols to demonstrate three
    different ways of using [Babel]. They each have doc comments in both the interface and
    the implementation explaining everything. In each case, start by looking at the
    interface.

    Each of the modules mentioned below defines a single versioned-rpc. Normally in a
    protocol library such as this, these modules would have names that indicate what those
    rpcs do. It's only for the purposes of navigating this example directory that we've
    instead given them names indicating the style of versioning. *)

(** A protocol demonstrating how a callee can support multiple versions of callers. *)
module Callee_converts = Callee_converts

(** A protocol demonstrating how a caller can support multiple versions of callees. *)
module Caller_converts = Caller_converts

(** A protocol demonstrating how callees and callers can negotiate to use a protocol they
    both understand. *)
module Both_convert = Both_convert
