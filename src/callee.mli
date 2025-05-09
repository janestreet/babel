open Core
open Async_kernel
open Async_rpc_kernel

(** An [a t] can be used to define many rpc implementations in one go, based on a single
    model implementation supplied to [implement_multi]. The type [a] is usually going to
    be a function from the query type to the response type, including whatever deferreds,
    pipes, etc. that the implementation will be responsible for. *)
type -_ t

(** From a model implementation, define some number of rpc implementations. In practice,
    the result of the supplied function is usually a function, so it may be enlightening
    to think of the type as:

    {[
      val implement_multi
        :  ?on_exception:Rpc.On_exception.t
        -> ('a -> 'b) t
        -> f:('s -> Rpc.Description.t -> 'a -> 'b)
        -> 's Rpc.Implementation.t list Or_error.t
    ]}

    Returns an error if any rpcs are duplicated. *)
val implement_multi
  :  ?on_exception:Rpc.On_exception.t
  -> 'a t
  -> f:('s -> Rpc.Description.t -> 'a)
  -> 's Rpc.Implementation.t list Or_error.t

val implement_multi_exn
  :  ?on_exception:Rpc.On_exception.t
  -> 'a t
  -> f:('s -> Rpc.Description.t -> 'a)
  -> 's Rpc.Implementation.t list

(** The protocols supported by the callee. Returns an error if any rpcs are duplicated. *)
val shapes : _ t -> (Rpc.Description.t * Shape.t) list Or_error.t

val shapes_exn : _ t -> (Rpc.Description.t * Shape.t) list
val supported_rpcs : _ t -> Generic_rpc.t list Or_error.t
val supported_rpcs_exn : _ t -> Generic_rpc.t list
val descriptions : _ t -> Rpc.Description.t list Or_error.t
val descriptions_exn : _ t -> Rpc.Description.t list

(** Print a description of every supported protocol. This is useful for demonstrating the
    final set of protocols in an expect test. *)
val print_shapes : _ t -> unit

(** [of_list ts] results in a callee that implements everything that each element of [ts]
    implements.

    {[
      implement_multi (of_list ts) ~f = List.concat_map ts ~f:(implement_multi ~f)
    ]} *)
val of_list : 'a t list -> 'a t

(** High level functions for working with callees in the style of
    [Async.Rpc.Rpc.implement]. *)
module Rpc : sig
  type ('q, 'r) implementation := 'q -> 'r Deferred.t

  (** Create a callee which can implement a given rpc. *)
  val singleton : ('q, 'r) Rpc.Rpc.t -> ('q, 'r) implementation t

  (** Extend a callee to be able to implement a given rpc. *)
  val add
    :  ('q, 'r) implementation t
    -> rpc:('q, 'r) Rpc.Rpc.t
    -> ('q, 'r) implementation t

  (** Map over the query type of a callee. *)
  val map_query
    :  ('q1, 'r) implementation t
    -> f:('q1 -> 'q2)
    -> ('q2, 'r) implementation t

  (** Map over the response type of a callee. *)
  val map_response
    :  ('q, 'r1) implementation t
    -> f:('r2 -> 'r1)
    -> ('q, 'r2) implementation t
end

(** High level functions for working with callees in the style of
    [Async.Rpc.Rpc.implement']. *)
module Rpc' : sig
  open Async_rpc_kernel

  type ('q, 'r) implementation := 'q -> 'r

  (** Create a callee which can implement a given rpc. *)
  val singleton : ('q, 'r) Rpc.Rpc.t -> ('q, 'r) implementation t

  (** Extend a callee to be able to implement a given rpc. *)
  val add
    :  ('q, 'r) implementation t
    -> rpc:('q, 'r) Rpc.Rpc.t
    -> ('q, 'r) implementation t

  (** Map over the query type of a callee. *)
  val map_query
    :  ('q1, 'r) implementation t
    -> f:('q1 -> 'q2)
    -> ('q2, 'r) implementation t

  (** Map over the response type of a callee. *)
  val map_response
    :  ('q, 'r1) implementation t
    -> f:('r2 -> 'r1)
    -> ('q, 'r2) implementation t
end

(** High level functions for working with callees in the style of
    [Async.Rpc.Pipe_rpc.implement]. *)
module Pipe_rpc : sig
  open Async_rpc_kernel

  type ('q, 'r, 'e) implementation := 'q -> ('r Pipe.Reader.t, 'e) Result.t Deferred.t

  (** Create a callee which can implement a given rpc. *)
  val singleton
    :  ?leave_open_on_exception:bool (** default: See [Async_rpc.Pipe_rpc.implement] *)
    -> ('q, 'r, 'e) Rpc.Pipe_rpc.t
    -> ('q, 'r, 'e) implementation t

  (** Extend a callee to be able to implement a given rpc. *)
  val add
    :  ?leave_open_on_exception:bool (** default: See [Async_rpc.Pipe_rpc.implement] *)
    -> ('q, 'r, 'e) implementation t
    -> rpc:('q, 'r, 'e) Rpc.Pipe_rpc.t
    -> ('q, 'r, 'e) implementation t

  (** Map over the query type of a callee. *)
  val map_query
    :  ('q1, 'r, 'e) implementation t
    -> f:('q1 -> 'q2)
    -> ('q2, 'r, 'e) implementation t

  (** Map over the response type of a callee.

      Sometimes, [Callee.Pipe_rpc.map_response] is not sufficient. If you use
      [Callee.map_response] instead, you can access the pipe itself, not just the values
      inside it. *)
  val map_response
    :  ('q, 'r1, 'e) implementation t
    -> f:('r2 -> 'r1)
    -> ('q, 'r2, 'e) implementation t

  (** Map over the response type of a callee, possibly filtering out some responses. *)
  val filter_map_response
    :  ('q, 'r1, 'e) implementation t
    -> f:('r2 -> 'r1 option)
    -> ('q, 'r2, 'e) implementation t

  (** Map over the error type of a callee. *)
  val map_error
    :  ('q, 'r, 'e1) implementation t
    -> f:('e2 -> 'e1)
    -> ('q, 'r, 'e2) implementation t
end

(** High level functions for working with callees in the style of
    [Async.Rpc.Pipe_rpc.implement_direct].

    This isn't quite a drop-in replacement because users will need to switch from
    [Rpc.Pipe_rpc.Direct_stream_writer] to Babel's [Direct_stream_writer], but most of the
    same functions are supported. *)
module Pipe_rpc_direct : sig
  open Async_rpc_kernel
  module Direct_stream_writer = Direct_stream_writer

  type ('q, 'r, 'e) implementation :=
    'q -> 'r Direct_stream_writer.t -> (unit, 'e) Result.t Deferred.t

  (** Create a callee which can implement a given rpc. *)
  val singleton
    :  ?leave_open_on_exception:bool (** default: See [Async_rpc.Pipe_rpc.implement] *)
    -> ('q, 'r, 'e) Rpc.Pipe_rpc.t
    -> ('q, 'r, 'e) implementation t

  (** Extend a callee to be able to implement a given rpc. *)
  val add
    :  ?leave_open_on_exception:bool (** default: See [Async_rpc.Pipe_rpc.implement] *)
    -> ('q, 'r, 'e) implementation t
    -> rpc:('q, 'r, 'e) Rpc.Pipe_rpc.t
    -> ('q, 'r, 'e) implementation t

  (** Map over the query type of a callee. *)
  val map_query
    :  ('q1, 'r, 'e) implementation t
    -> f:('q1 -> 'q2)
    -> ('q2, 'r, 'e) implementation t

  (** Map over the response type of a callee. *)
  val map_response
    :  ('q, 'r1, 'e) implementation t
    -> f:('r2 -> 'r1)
    -> ('q, 'r2, 'e) implementation t

  (** Map over the response type of a callee, possibly filtering out some responses. *)
  val filter_map_response
    :  ('q, 'r1, 'e) implementation t
    -> f:('r2 -> 'r1 option)
    -> ('q, 'r2, 'e) implementation t

  (** Map over the error type of a callee. *)
  val map_error
    :  ('q, 'r, 'e1) implementation t
    -> f:('e2 -> 'e1)
    -> ('q, 'r, 'e2) implementation t
end

(** High level functions for working with callees in the style of
    [Async.Rpc.State_rpc.implement]. *)
module State_rpc : sig
  open Async_rpc_kernel

  type ('q, 's, 'u, 'e) implementation :=
    'q -> ('s * 'u Pipe.Reader.t, 'e) Result.t Deferred.t

  (** Create a callee which can implement a given rpc. *)
  val singleton
    :  ?leave_open_on_exception:bool (** default: See [Async_rpc.Pipe_rpc.implement] *)
    -> ('q, 's, 'u, 'e) Rpc.State_rpc.t
    -> ('q, 's, 'u, 'e) implementation t

  (** Extend a callee to be able to implement a given rpc. *)
  val add
    :  ?leave_open_on_exception:bool (** default: See [Async_rpc.Pipe_rpc.implement] *)
    -> ('q, 's, 'u, 'e) implementation t
    -> rpc:('q, 's, 'u, 'e) Rpc.State_rpc.t
    -> ('q, 's, 'u, 'e) implementation t

  (** Map over the query type of a callee. *)
  val map_query
    :  ('q1, 's, 'u, 'e) implementation t
    -> f:('q1 -> 'q2)
    -> ('q2, 's, 'u, 'e) implementation t

  (** Map over the state type of a callee. *)
  val map_state
    :  ('q, 's1, 'u, 'e) implementation t
    -> f:('s2 -> 's1)
    -> ('q, 's2, 'u, 'e) implementation t

  (** Map over the update type of a callee.

      Sometimes, [Callee.State_rpc.map_update] is not sufficient. For example, sometimes
      it might not be possible to convert the response to the desired type, in which case
      it may be appropriate to drop the value from the pipe entirely. For such cases, use
      [Callee.map_response] instead. It gives you access to the pipe itself, not just the
      values inside it, allowing you to use something like [Pipe.filter_map]. *)
  val map_update
    :  ('q, 's, 'u1, 'e) implementation t
    -> f:('u2 -> 'u1)
    -> ('q, 's, 'u2, 'e) implementation t

  (** Map over the update type of a callee, possibly filtering out some updates. *)
  val filter_map_update
    :  ('q, 's, 'u1, 'e) implementation t
    -> f:('u2 -> 'u1 option)
    -> ('q, 's, 'u2, 'e) implementation t

  (** Map over the error type of a callee. *)
  val map_error
    :  ('q, 's, 'u, 'e1) implementation t
    -> f:('e2 -> 'e1)
    -> ('q, 's, 'u, 'e2) implementation t
end

(** High level functions for working with callees in the style of
    [Async.Rpc.State_rpc.implement_direct].

    This isn't quite a drop-in replacement because users will need to switch from
    [Rpc.Pipe_rpc.Direct_stream_writer] to Babel's [Direct_stream_writer], but most of the
    same functions are supported. *)
module State_rpc_direct : sig
  open Async_rpc_kernel
  module Direct_stream_writer = Pipe_rpc_direct.Direct_stream_writer

  type ('q, 's, 'u, 'e) implementation :=
    'q -> 'u Direct_stream_writer.t -> ('s, 'e) Result.t Deferred.t

  (** Create a callee which can implement a given rpc. *)
  val singleton
    :  ?leave_open_on_exception:bool (** default: See [Async_rpc.Pipe_rpc.implement] *)
    -> ('q, 's, 'u, 'e) Rpc.State_rpc.t
    -> ('q, 's, 'u, 'e) implementation t

  (** Extend a callee to be able to implement a given rpc. *)
  val add
    :  ?leave_open_on_exception:bool (** default: See [Async_rpc.Pipe_rpc.implement] *)
    -> ('q, 's, 'u, 'e) implementation t
    -> rpc:('q, 's, 'u, 'e) Rpc.State_rpc.t
    -> ('q, 's, 'u, 'e) implementation t

  (** Map over the query type of a callee. *)
  val map_query
    :  ('q1, 's, 'u, 'e) implementation t
    -> f:('q1 -> 'q2)
    -> ('q2, 's, 'u, 'e) implementation t

  (** Map over the state type of a callee. *)
  val map_state
    :  ('q, 's1, 'u, 'e) implementation t
    -> f:('s2 -> 's1)
    -> ('q, 's2, 'u, 'e) implementation t

  (** Map over the update type of a callee. *)
  val map_update
    :  ('q, 's, 'u1, 'e) implementation t
    -> f:('u2 -> 'u1)
    -> ('q, 's, 'u2, 'e) implementation t

  (** Map over the update type of a callee, possibly filtering out some updates. *)
  val filter_map_update
    :  ('q, 's, 'u1, 'e) implementation t
    -> f:('u2 -> 'u1 option)
    -> ('q, 's, 'u2, 'e) implementation t

  (** Map over the error type of a callee. *)
  val map_error
    :  ('q, 's, 'u, 'e1) implementation t
    -> f:('e2 -> 'e1)
    -> ('q, 's, 'u, 'e2) implementation t
end

(** High level functions for working with callees in the style of
    [Async.Rpc.One_way.implement]. *)
module One_way : sig
  open Async_rpc_kernel

  type 'msg implementation := 'msg -> unit

  (** Create a callee which can implement a given rpc. *)
  val singleton : 'msg Rpc.One_way.t -> 'msg implementation t

  (** Extend a callee to be able to implement a given rpc. *)
  val add : 'msg implementation t -> rpc:'msg Rpc.One_way.t -> 'msg implementation t

  (** Map over the query type of a callee. *)
  val map_msg : 'msg1 implementation t -> f:('msg1 -> 'msg2) -> 'msg2 implementation t
end

(** High level functions for working with callees in the style of
    [Streamable.Plain_rpc.implement]. *)
module Streamable_plain_rpc : sig
  type ('q, 'r) implementation := 'q -> 'r Or_error.t Deferred.t

  (** Create a callee which can implement a given rpc. *)
  val singleton : ('q, 'r) Streamable.Plain_rpc.t -> ('q, 'r) implementation t

  (** Extend a callee to be able to implement a given rpc. *)
  val add
    :  ('q, 'r) implementation t
    -> rpc:('q, 'r) Streamable.Plain_rpc.t
    -> ('q, 'r) implementation t

  (** Map over the query type of a callee. *)
  val map_query
    :  ('q1, 'r) implementation t
    -> f:('q1 -> 'q2)
    -> ('q2, 'r) implementation t

  (** Map over the response type of a callee. *)
  val map_response
    :  ('q, 'r1) implementation t
    -> f:('r2 -> 'r1)
    -> ('q, 'r2) implementation t
end

(** High level functions for working with callees in the style of
    [Streamable.Pipe_rpc.implement]. *)
module Streamable_pipe_rpc : sig
  type ('q, 'r) implementation := 'q -> 'r Pipe.Reader.t Or_error.t Deferred.t

  (** Create a callee which can implement a given rpc. *)
  val singleton
    :  ?leave_open_on_exception:bool (** default: See [Async_rpc.Pipe_rpc.implement] *)
    -> ('q, 'r) Streamable.Pipe_rpc.t
    -> ('q, 'r) implementation t

  (** Extend a callee to be able to implement a given rpc. *)
  val add
    :  ?leave_open_on_exception:bool (** default: See [Async_rpc.Pipe_rpc.implement] *)
    -> ('q, 'r) implementation t
    -> rpc:('q, 'r) Streamable.Pipe_rpc.t
    -> ('q, 'r) implementation t

  (** Map over the query type of a callee. *)
  val map_query
    :  ('q1, 'r) implementation t
    -> f:('q1 -> 'q2)
    -> ('q2, 'r) implementation t

  (** Map over the response type of a callee.

      Sometimes, [Callee.Streamable_pipe_rpc.map_response] is not sufficient. If you use
      [Callee.map_response] instead, you can access the pipe itself, not just the values
      inside it. *)
  val map_response
    :  ('q, 'r1) implementation t
    -> f:('r2 -> 'r1)
    -> ('q, 'r2) implementation t

  (** Map over the response type of a callee, possibly filtering out some responses. *)
  val filter_map_response
    :  ('q, 'r1) implementation t
    -> f:('r2 -> 'r1 option)
    -> ('q, 'r2) implementation t
end

(** High level functions for working with callees in the style of
    [Streamable.State_rpc.implement]. *)
module Streamable_state_rpc : sig
  type ('q, 's, 'u) implementation := 'q -> ('s * 'u Pipe.Reader.t) Or_error.t Deferred.t

  (** Create a callee which can implement a given rpc. *)
  val singleton
    :  ?leave_open_on_exception:bool (** default: See [Async_rpc.Pipe_rpc.implement] *)
    -> ('q, 's, 'u) Streamable.State_rpc.t
    -> ('q, 's, 'u) implementation t

  (** Extend a callee to be able to implement a given rpc. *)
  val add
    :  ?leave_open_on_exception:bool (** default: See [Async_rpc.Pipe_rpc.implement] *)
    -> ('q, 's, 'u) implementation t
    -> rpc:('q, 's, 'u) Streamable.State_rpc.t
    -> ('q, 's, 'u) implementation t

  (** Map over the query type of a callee. *)
  val map_query
    :  ('q1, 's, 'u) implementation t
    -> f:('q1 -> 'q2)
    -> ('q2, 's, 'u) implementation t

  (** Map over the state type of a callee. *)
  val map_state
    :  ('q, 's1, 'u) implementation t
    -> f:('s2 -> 's1)
    -> ('q, 's2, 'u) implementation t

  (** Map over the update type of a callee.

      Sometimes, [Callee.Streamable_state_rpc.map_update] is not sufficient. For example,
      sometimes it might not be possible to convert the response to the desired type, in
      which case it may be appropriate to drop the value from the pipe entirely. For such
      cases, use [Callee.map_response] instead. It gives you access to the pipe itself,
      not just the values inside it, allowing you to use something like [Pipe.filter_map]. *)
  val map_update
    :  ('q, 's, 'u1) implementation t
    -> f:('u2 -> 'u1)
    -> ('q, 's, 'u2) implementation t

  (** Map over the update type of a callee, possibly filtering out some updates. *)
  val filter_map_update
    :  ('q, 's, 'u1) implementation t
    -> f:('u2 -> 'u1 option)
    -> ('q, 's, 'u2) implementation t
end

(** [map t ~f] transforms the model function supplied to [implement_multi]. Since in
    practice we are usually working with functions, it may be enlightening to think of the
    type as:

    {[
      val map : ('a1 -> 'b1) t -> f:(('a2 -> 'b2) -> 'a1 -> 'b1) -> ('a2 -> 'b2) t
    ]}

    Most of the time, you probably would prefer to use more specialized functions, such as
    [Rpc.map_query] and [Rpc.map_response], instead of [map].

    {[
      implement_multi (map t ~f) ~f:g = implement_multi t ~f:(Fn.compose f g)
    ]} *)
val map : 'a t -> f:('b -> 'a) -> 'b t

(** [map_query] is a specialization of [map] for the query type of a callee.

    In practice, most implementations will return deferreds or other complex types, so you
    should prefer to use more specialized functions, such as [Rpc.map_query].

    {[
      implement_multi (map_query t ~f) ~f:g
      = implement_multi t ~f:(fun s query -> g s (f query))
    ]} *)
val map_query : ('a -> 'b) t -> f:('a -> 'c) -> ('c -> 'b) t

(** [map_response] is a specialization of [map] for the return type of an implementation.

    In practice, most implementations will return deferreds or other complex types, so you
    should prefer to use more specialized functions, such as [Rpc.map_response].
    [map_response] is intended to be used for more radical changes to an implementation,
    such as to change the type of rpc protocol.

    {[
      implement_multi (map_response t ~f) ~f:g
      = implement_multi t ~f:(fun s query -> f (g s query))
    ]} *)
val map_response : ('a -> 'b) t -> f:('c -> 'b) -> ('a -> 'c) t
