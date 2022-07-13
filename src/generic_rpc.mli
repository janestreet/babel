open! Core
open Async_rpc_kernel

type t =
  | Rpc : _ Rpc.Rpc.t -> t
  | Pipe : _ Rpc.Pipe_rpc.t -> t
  | State : _ Rpc.State_rpc.t -> t
  | One_way : _ Rpc.One_way.t -> t
  | Streamable_plain : _ Streamable.Plain_rpc.t -> t
  | Streamable_pipe : _ Streamable.Pipe_rpc.t -> t
  | Streamable_state : _ Streamable.State_rpc.t -> t

val description : t -> Rpc.Description.t
val shape : t -> Shape.t
