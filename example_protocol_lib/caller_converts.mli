(** This example demonstrates using [Babel] to allow clients to query multiple versions of
    servers.

    The signature hides nearly everything about versioning. This allows both client and
    server code to not think much about it. The only thing betraying the presence of
    versioning is happening is that there are different types for client and server. That
    is, [dispatch] works using the types in [Client] and [implement] works using the types
    in [Server]. In a steady state, the client and server types will be the same, but
    during an incomplete protocol upgrade they may be different. The procedure to upgrade
    the protocol is assumed to work as follows:

    + Define the newest version and expose it in [Client]. This forces you to update call
      sites of [dispatch].
    + Roll the clients.
    + Expose the newest version in [Server]. This forces you to update the call site of
      [implement].
    + Roll the servers.

    This module is demonstrating what things could look like after upgrading
    [Client.Response] but before upgrading [Server.Response].

    In this "caller converts" style of versioned protocols, you have to use a version
    menu. Clients will have to create a [Versioned_rpc.Connection_with_menu.t] in order to
    use [dispatch]. *)

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
    type t =
      { y : int
      ; z : int
      }
    [@@deriving sexp_of]
  end
end

(** [dispatch] requires a [Versioned_rpc.Connection_with_menu.t] so that it can select the
    version it wants to use. *)
val dispatch
  :  Versioned_rpc.Connection_with_menu.t
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
    type t = { z : int } [@@deriving sexp_of]
  end
end

(** This is the same as [Rpc.Rpc.implement]. We don't expose the [Rpc.Rpc.t] directly to
    prevent servers from ever thinking about versioning. *)
val implement
  :  ('a -> Server.Query.t -> Server.Response.t Deferred.t)
  -> 'a Rpc.Implementation.t
