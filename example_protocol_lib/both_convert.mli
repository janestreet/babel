(** This example demonstrates using [Babel] to allow clients and servers to negotiate the
    best version of a protocol they both understand.

    The signature hides everything about versioning. This allows both client and server
    code to not think about it. Unlike a "callee converts" or "caller converts" versioning
    style, there is no point in preventing either client or server from using the latest
    defined version of the protocol. It also means there is no particular order that you
    have to roll the servers and clients. The downside is that the protocol must be both
    upgradable and downgradable.

    In this "both convert" style of versioned protocols, you have to use a version menu.
    Clients will have to create a [Versioned_rpc.Connection_with_menu.t] in order to use
    [dispatch]. *)

open! Core
open! Async_kernel
open Async_rpc_kernel

module Query : sig
  type t =
    { a : int
    ; b : int
    }
  [@@deriving sexp_of]
end

module Response : sig
  type t =
    { y : int
    ; z : int
    }
  [@@deriving sexp_of]
end

(** [dispatch] requires a [Versioned_rpc.Connection_with_menu.t] so that it can select the
    version it wants to use. *)
val dispatch
  :  Versioned_rpc.Connection_with_menu.t
  -> Query.t
  -> Response.t Or_error.t Deferred.t

(** Notice that [implement] returns a list of implementations rather than just a single
    implementation. Each implementation it returns corresponds to one of the versions of
    the protocol supported by the server. *)
val implement
  :  ('a -> Rpc.Description.t -> Query.t -> Response.t Deferred.t)
  -> 'a Rpc.Implementation.t list
