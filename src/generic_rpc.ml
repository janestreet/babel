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

let description = function
  | Rpc rpc -> Rpc.Rpc.description rpc
  | Pipe pipe -> Rpc.Pipe_rpc.description pipe
  | State state -> Rpc.State_rpc.description state
  | One_way one_way -> Rpc.One_way.description one_way
  | Streamable_plain streamable_plain -> Streamable.Plain_rpc.description streamable_plain
  | Streamable_pipe streamable_pipe -> Streamable.Pipe_rpc.description streamable_pipe
  | Streamable_state streamable_state -> Streamable.State_rpc.description streamable_state
;;

let shape = function
  | Rpc rpc -> Shape.rpc rpc
  | Pipe pipe -> Shape.pipe_rpc pipe
  | State state -> Shape.state_rpc state
  | One_way one_way -> Shape.one_way one_way
  | Streamable_plain streamable_plain -> Shape.streamable_plain_rpc streamable_plain
  | Streamable_pipe streamable_pipe -> Shape.streamable_pipe_rpc streamable_pipe
  | Streamable_state streamable_state -> Shape.streamable_state_rpc streamable_state
;;
