(** This example demonstrates using [Babel] to serve queries from multiple versions of
    clients.

    The signature hides nearly everything about versioning. This allows both client and
    server code to not think much about it. The only thing betraying the presence of
    versioning is happening is that there are different types for client and server. That
    is, [dispatch] works using the types in [Client] and [implement] works using the types
    in [Server]. In a steady state, the client and server types will be the same, but
    during an incomplete protocol upgrade they may be different. The procedure to upgrade
    the protocol is assumed to work as follows:

    + Define the newest version and expose it in [Server]. This forces you to update the
      call site of [implement].
    + Roll the servers.
    + Expose the newest version in [Client]. This forces you to update call sites of
      [dispatch].
    + Roll the clients.

    This module is demonstrating what things could look like after upgrading
    [Server.Response] but before upgrading [Client.Response].

    In this "callee converts" style of versioned protocols, there is no need to use a
    version menu. *)

open! Core
open! Async_kernel
open Async_rpc_kernel

(** These are the types that clients are expected to know about. *)
module Client : sig
  module Query : sig
    type t =
      { a : int
      ; b : int
      }
    [@@deriving sexp_of]
  end

  module Response : sig
    type t = { z : int } [@@deriving sexp_of]
  end
end

(** This is the same as [Rpc.Rpc.dispatch]. We don't expose the [Rpc.Rpc.t] directly to
    prevent clients from ever thinking about versioning. *)
val dispatch
  :  Rpc.Connection.t
  -> Client.Query.t
  -> Client.Response.t Or_error.t Deferred.t

(** These are the types that servers are expected to know about. *)
module Server : sig
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
end

(** Notice that [implement] returns a list of implementations rather than just a single
    implementation. Each implementation it returns corresponds to one of the versions of
    the protocol supported by the server. *)
val implement
  :  ('a -> Rpc.Description.t -> Server.Query.t -> Server.Response.t Deferred.t)
  -> 'a Rpc.Implementation.t list
