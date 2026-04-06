open! Core
open Async_kernel
open Async_rpc_kernel

(** Like [Rpc.Rpc.Expert.Responder], but with a phantom type parameter for the response
    type. This maintains type safety when writing preserialized responses. *)
type -'response t

(** These functions work the same way as the ones in [Rpc.Rpc.Expert.Responder].

    [schedule] returns a deferred that becomes determined when the write has been flushed.
    The bigstring must not be overwritten until the deferred is determined.

    [write_bigstring] writes immediately (copies the data). *)

val schedule
  :  _ t
  -> Bigstring.t
  -> pos:int
  -> len:int
  -> [ `Connection_closed | `Flushed of unit Deferred.t ]

val write_bigstring : _ t -> Bigstring.t -> pos:int -> len:int -> unit

val write_bin_prot
  :  'response t
  -> 'response Bin_prot.Type_class.writer
  -> 'response
  -> unit

val write_error : _ t -> Error.t -> unit

(** For internal use by [Callee.With_expert_responder]. Clients should use
    [Callee.With_expert_responder] instead of creating responders directly. *)
module Private : sig
  (** Create a Babel-friendly RPC responder from a raw Async_rpc one. *)
  val create : Rpc.Rpc.Expert.Responder.t -> _ t
end
