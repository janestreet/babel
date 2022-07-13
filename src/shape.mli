open! Core
open! Async_rpc_kernel

(** Bin_prot digests of the types used in a protocol. This is useful for expect tests and
    validating protocol compatibility. *)
type t [@@deriving sexp_of]

include Comparable.S_plain with type t := t

val rpc : _ Rpc.Rpc.t -> t
val pipe_rpc : _ Rpc.Pipe_rpc.t -> t
val state_rpc : _ Rpc.State_rpc.t -> t
val one_way : _ Rpc.One_way.t -> t
val streamable_plain_rpc : _ Streamable.Plain_rpc.t -> t
val streamable_pipe_rpc : _ Streamable.Pipe_rpc.t -> t
val streamable_state_rpc : _ Streamable.State_rpc.t -> t
