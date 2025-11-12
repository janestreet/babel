open! Core
open Async_kernel
open Async_rpc_kernel

(** An [a t] selects a dispatch function of type [a], which will typically be a function
    type, given a version menu supplied by the server.

    NOTE Babel does not catch exceptions raised by caller conversion functions. *)
type +_ t

(** The protocols supported by the caller in order from most preferred to least preferred. *)
val shapes : _ t -> (Rpc.Description.t * Shape.t) Nonempty_list.t

val descriptions : _ t -> Rpc.Description.t Nonempty_list.t
val supported_rpcs : _ t -> Generic_rpc.t Nonempty_list.t

(** Print the required set of protocols for each supported dispatching strategy in order
    from most preferred to least preferred. This is useful for demonstrating the final set
    of protocols in an expect test. *)
val print_shapes : _ t -> unit

(** [of_list_decreasing_preference ts] results in a caller that picks the first [t] in
    [ts] compatible with whatever version menu is supplied to [dispatch_multi]. *)
val of_list_decreasing_preference : 'a t Nonempty_list.t -> 'a t

(** High level functions for working with callers in the style of
    [Async.Rpc.Rpc.dispatch]. *)
module Rpc : sig
  type ('q, 'r) dispatch := 'q -> 'r Or_error.t Deferred.t

  (** Determine which supported dispatch strategy to use and invoke the chosen rpcs. *)
  val dispatch_multi
    :  ('q, 'r) dispatch t
    -> Versioned_rpc.Connection_with_menu.t
    -> ('q, 'r) dispatch

  (** Create a new caller supporting a single rpc. *)
  val singleton : ('q, 'r) Rpc.Rpc.t -> ('q, 'r) dispatch t

  (** Add support for dispatching another rpc. [dispatch_multi] will prefer this rpc over
      the ones the caller already supports. *)
  val add : ('q, 'r) dispatch t -> rpc:('q, 'r) Rpc.Rpc.t -> ('q, 'r) dispatch t

  (** A specialization of [map] for the query type of a protocol. *)
  val map_query : ('q1, 'r) dispatch t -> f:('q2 -> 'q1) -> ('q2, 'r) dispatch t

  (** A specialization of [map] for the response type of a protocol. *)
  val map_response : ('q, 'r1) dispatch t -> f:('r1 -> 'r2) -> ('q, 'r2) dispatch t
end

(** High level functions for working with callers in the style of
    [Async.Rpc.Rpc.dispatch_exn]. *)
module Rpc_exn : sig
  open Async_rpc_kernel

  type ('q, 'r) dispatch := 'q -> 'r Deferred.t

  (** Determine which supported dispatch strategy to use and invoke the chosen rpcs. *)
  val dispatch_multi
    :  ('q, 'r) dispatch t
    -> Versioned_rpc.Connection_with_menu.t
    -> ('q, 'r) dispatch

  (** Create a new caller supporting a single rpc. *)
  val singleton : ('q, 'r) Rpc.Rpc.t -> ('q, 'r) dispatch t

  (** Add support for dispatching another rpc. [dispatch_multi] will prefer this rpc over
      the ones the caller already supports. *)
  val add : ('q, 'r) dispatch t -> rpc:('q, 'r) Rpc.Rpc.t -> ('q, 'r) dispatch t

  (** A specialization of [map] for the query type of a protocol. *)
  val map_query : ('q1, 'r) dispatch t -> f:('q2 -> 'q1) -> ('q2, 'r) dispatch t

  (** A specialization of [map] for the response type of a protocol. *)
  val map_response : ('q, 'r1) dispatch t -> f:('r1 -> 'r2) -> ('q, 'r2) dispatch t
end

(** High level functions for working with callers in the style of
    [Async.Rpc.Rpc.dispatch']. There is no [Babel.Caller.Rpc'.dispatch_multi]. Use the
    more general [Babel.Caller.to_dispatch_fun]. *)
module Rpc' : sig
  open Async_rpc_kernel

  type ('q, 'r) dispatch := 'q -> 'r Async_rpc_kernel.Rpc_result.t Deferred.t

  (** Create a new caller supporting a single rpc. *)
  val singleton : ('q, 'r) Rpc.Rpc.t -> ('q, 'r) dispatch t

  (** Add support for dispatching another rpc. [dispatch_multi] will prefer this rpc over
      the ones the caller already supports. *)
  val add : ('q, 'r) dispatch t -> rpc:('q, 'r) Rpc.Rpc.t -> ('q, 'r) dispatch t

  (** A specialization of [map] for the query type of a protocol. *)
  val map_query : ('q1, 'r) dispatch t -> f:('q2 -> 'q1) -> ('q2, 'r) dispatch t

  (** A specialization of [map] for the response type of a protocol. *)
  val map_response : ('q, 'r1) dispatch t -> f:('r1 -> 'r2) -> ('q, 'r2) dispatch t

  (** Convert Rpc' style caller to Rpc style caller *)
  val to_rpc_style : ('q, 'r) dispatch t -> ('q -> 'r Or_error.t Deferred.t) t
end

(** High level functions for working with callers in the style of
    [Async.Rpc.Pipe_rpc.dispatch]. *)
module Pipe_rpc : sig
  open Async_rpc_kernel

  type ('q, 'r, 'e) dispatch :=
    'q -> ('r Pipe.Reader.t * Rpc.Pipe_rpc.Metadata.t, 'e) Result.t Or_error.t Deferred.t

  type ('q, 'r, 'e) dispatch_with_close_reason :=
    'q -> (('r, Error.t) Pipe_with_writer_error.t, 'e) Result.t Or_error.t Deferred.t

  (** Determine which supported dispatch strategy to use and invoke the chosen rpcs. To
      unsubscribe, you can close the pipe. *)
  val dispatch_multi
    :  ('q, 'r, 'e) dispatch t
    -> Versioned_rpc.Connection_with_menu.t
    -> ('q, 'r, 'e) dispatch

  (** [dispatch_multi] but requires handling of the [Pipe_close_reason] when there is an
      [Error]. [Closed_locally] and [Closed_remotely] are not considered errors. *)
  val dispatch_multi_with_close_reason
    :  ('q, 'r, 'e) dispatch t
    -> Versioned_rpc.Connection_with_menu.t
    -> ('q, 'r, 'e) dispatch_with_close_reason

  (** Create a new caller supporting a single rpc. *)
  val singleton : ('q, 'r, 'e) Rpc.Pipe_rpc.t -> ('q, 'r, 'e) dispatch t

  (** Add support for dispatching another rpc. [dispatch_multi] will prefer this rpc over
      the ones the caller already supports. *)
  val add
    :  ('q, 'r, 'e) dispatch t
    -> rpc:('q, 'r, 'e) Rpc.Pipe_rpc.t
    -> ('q, 'r, 'e) dispatch t

  (** A specialization of [map] for the query type of a protocol. *)
  val map_query : ('q1, 'r, 'e) dispatch t -> f:('q2 -> 'q1) -> ('q2, 'r, 'e) dispatch t

  (** A specialization of [map] for the response type of a protocol.

      Sometimes, [Caller.Pipe_rpc.map_response] is not sufficient. For example, sometimes
      it might not be possible to convert the response to the desired type, in which case
      it may be appropriate to drop the value from the pipe entirely. For such cases, use
      [Caller.map_response] instead. It gives you access to the pipe itself, not just the
      values inside it, allowing you to use something like [Pipe.filter_map]. *)
  val map_response
    :  ('q, 'r1, 'e) dispatch t
    -> f:('r1 -> 'r2)
    -> ('q, 'r2, 'e) dispatch t

  (** Same as [map_response] but filters out some responses from the response pipe *)
  val filter_map_response
    :  ('q, 'r1, 'e) dispatch t
    -> f:('r1 -> 'r2 option)
    -> ('q, 'r2, 'e) dispatch t

  (** A specialization of [map] for the error type of a protocol. *)
  val map_error : ('q, 'r, 'e1) dispatch t -> f:('e1 -> 'e2) -> ('q, 'r, 'e2) dispatch t
end

(** High level functions for working with callers in the style of
    [Async.Rpc.Pipe_rpc.dispatch']. *)
module Pipe_rpc' : sig
  open Async_rpc_kernel

  type ('q, 'r, 'e) dispatch :=
    'q
    -> ('r Pipe.Reader.t * Rpc.Pipe_rpc.Metadata.t, 'e) Result.t Rpc_result.t Deferred.t

  (** Create a new caller supporting a single rpc. *)
  val singleton : ('q, 'r, 'e) Rpc.Pipe_rpc.t -> ('q, 'r, 'e) dispatch t

  (** Add support for dispatching another rpc. [dispatch_multi] will prefer this rpc over
      the ones the caller already supports. *)
  val add
    :  ('q, 'r, 'e) dispatch t
    -> rpc:('q, 'r, 'e) Rpc.Pipe_rpc.t
    -> ('q, 'r, 'e) dispatch t

  (** A specialization of [map] for the query type of a protocol. *)
  val map_query : ('q1, 'r, 'e) dispatch t -> f:('q2 -> 'q1) -> ('q2, 'r, 'e) dispatch t

  (** A specialization of [map] for the response type of a protocol.

      Sometimes, [Caller.Pipe_rpc'.map_response] is not sufficient. For example, sometimes
      it might not be possible to convert the response to the desired type, in which case
      it may be appropriate to drop the value from the pipe entirely. For such cases, use
      [Caller.map_response] instead. It gives you access to the pipe itself, not just the
      values inside it, allowing you to use something like [Pipe.filter_map]. *)
  val map_response
    :  ('q, 'r1, 'e) dispatch t
    -> f:('r1 -> 'r2)
    -> ('q, 'r2, 'e) dispatch t

  (** Same as [map_response] but filters out some responses from the response pipe *)
  val filter_map_response
    :  ('q, 'r1, 'e) dispatch t
    -> f:('r1 -> 'r2 option)
    -> ('q, 'r2, 'e) dispatch t

  (** A specialization of [map] for the error type of a protocol. *)
  val map_error : ('q, 'r, 'e1) dispatch t -> f:('e1 -> 'e2) -> ('q, 'r, 'e2) dispatch t

  (** Convert Pipe_rpc' style caller to Pipe_rpc style caller *)
  val to_pipe_rpc_style
    :  ('q, 'r, 'e) dispatch t
    -> ('q
        -> ('r Pipe.Reader.t * Rpc.Pipe_rpc.Metadata.t, 'e) Result.t Or_error.t Deferred.t)
         t
end

(** High level functions for working with callers in the style of
    [Async.Rpc.Pipe_rpc.dispatch_exn]. *)
module Pipe_rpc_exn : sig
  open Async_rpc_kernel

  type ('q, 'r) dispatch := 'q -> ('r Pipe.Reader.t * Rpc.Pipe_rpc.Metadata.t) Deferred.t

  type ('q, 'r) dispatch_with_close_reason :=
    'q -> ('r, Error.t) Pipe_with_writer_error.t Deferred.t

  (** Determine which supported dispatch strategy to use and invoke the chosen rpcs. To
      unsubscribe, you can close the pipe. *)
  val dispatch_multi
    :  ('q, 'r) dispatch t
    -> Versioned_rpc.Connection_with_menu.t
    -> ('q, 'r) dispatch

  (** [dispatch_multi] but requires handling of the [Pipe_close_reason] when there is an
      [Error]. [Closed_locally] and [Closed_remotely] are not considered errors. *)
  val dispatch_multi_with_close_reason
    :  ('q, 'r) dispatch t
    -> Versioned_rpc.Connection_with_menu.t
    -> ('q, 'r) dispatch_with_close_reason

  (** Create a new caller supporting a single rpc. *)
  val singleton : ('q, 'r, _) Rpc.Pipe_rpc.t -> ('q, 'r) dispatch t

  (** Add support for dispatching another rpc. [dispatch_multi] will prefer this rpc over
      the ones the caller already supports. *)
  val add : ('q, 'r) dispatch t -> rpc:('q, 'r, 'e) Rpc.Pipe_rpc.t -> ('q, 'r) dispatch t

  (** A specialization of [map] for the query type of a protocol. *)
  val map_query : ('q1, 'r) dispatch t -> f:('q2 -> 'q1) -> ('q2, 'r) dispatch t

  (** A specialization of [map] for the response type of a protocol.

      Sometimes, [Caller.Pipe_rpc_exn.map_response] is not sufficient. For example,
      sometimes it might not be possible to convert the response to the desired type, in
      which case it may be appropriate to drop the value from the pipe entirely. For such
      cases, use [Caller.map_response] instead. It gives you access to the pipe itself,
      not just the values inside it, allowing you to use something like [Pipe.filter_map]. *)
  val map_response : ('q, 'r1) dispatch t -> f:('r1 -> 'r2) -> ('q, 'r2) dispatch t

  (** Same as [map_response] but filters out some responses from the response pipe *)
  val filter_map_response
    :  ('q, 'r1) dispatch t
    -> f:('r1 -> 'r2 option)
    -> ('q, 'r2) dispatch t
end

(** High level functions for working with callers in the style of
    [Async.Rpc.Pipe_rpc.dispatch_iter]. *)
module Pipe_rpc_iter : sig
  open Async_rpc_kernel
  module Id : T

  type ('q, 'r, 'e) dispatch :=
    'q
    -> f:('r Rpc.Pipe_rpc.Pipe_message.t -> Rpc.Pipe_rpc.Pipe_response.t)
    -> (Id.t, 'e) Result.t Or_error.t Deferred.t

  (** Determine which supported dispatch strategy to use and invoke the chosen rpcs. To
      unsubscribe, you can use [abort]. *)
  val dispatch_multi
    :  ('q, 'r, 'e) dispatch t
    -> Versioned_rpc.Connection_with_menu.t
    -> ('q, 'r, 'e) dispatch

  (** Create a new caller supporting a single rpc. *)
  val singleton : ('q, 'r, 'e) Rpc.Pipe_rpc.t -> ('q, 'r, 'e) dispatch t

  (** Add support for dispatching another rpc. [dispatch_multi] will prefer this rpc over
      the ones the caller already supports. *)
  val add
    :  ('q, 'r, 'e) dispatch t
    -> rpc:('q, 'r, 'e) Rpc.Pipe_rpc.t
    -> ('q, 'r, 'e) dispatch t

  (** A specialization of [map] for the query type of a protocol. *)
  val map_query : ('q1, 'r, 'e) dispatch t -> f:('q2 -> 'q1) -> ('q2, 'r, 'e) dispatch t

  (** A specialization of [map] for the response type of a protocol. *)
  val map_response
    :  ('q, 'r1, 'e) dispatch t
    -> f:('r1 -> 'r2)
    -> ('q, 'r2, 'e) dispatch t

  (** A specialization of [map] for the error type of a protocol. *)
  val map_error : ('q, 'r, 'e1) dispatch t -> f:('e1 -> 'e2) -> ('q, 'r, 'e2) dispatch t

  (** Given the [Id.t] returned from [dispatch_multi], cancel the subscription. *)
  val abort : Id.t -> unit
end

(** High level functions for working with callers in the style of
    [Async.Rpc.State_rpc.dispatch]. *)
module State_rpc : sig
  open Async_rpc_kernel

  type ('q, 's, 'u, 'e) dispatch :=
    'q
    -> ('s * 'u Pipe.Reader.t * Rpc.State_rpc.Metadata.t, 'e) Result.t Or_error.t
         Deferred.t

  type ('q, 's, 'u, 'e) dispatch_with_close_reason :=
    'q -> ('s * ('u, Error.t) Pipe_with_writer_error.t, 'e) Result.t Or_error.t Deferred.t

  (** Determine which supported dispatch strategy to use and invoke the chosen rpcs. To
      unsubscribe, you can close the pipe. *)
  val dispatch_multi
    :  ('q, 's, 'u, 'e) dispatch t
    -> Versioned_rpc.Connection_with_menu.t
    -> ('q, 's, 'u, 'e) dispatch

  (** [dispatch_multi] but requires handling of the [Pipe_close_reason] when there is an
      [Error]. [Closed_locally] and [Closed_remotely] are not considered errors. *)
  val dispatch_multi_with_close_reason
    :  ('q, 's, 'u, 'e) dispatch t
    -> Versioned_rpc.Connection_with_menu.t
    -> ('q, 's, 'u, 'e) dispatch_with_close_reason

  (** Create a new caller supporting a single rpc. *)
  val singleton : ('q, 's, 'u, 'e) Rpc.State_rpc.t -> ('q, 's, 'u, 'e) dispatch t

  (** Add support for dispatching another rpc. [dispatch_multi] will prefer this rpc over
      the ones the caller already supports. *)
  val add
    :  ('q, 's, 'u, 'e) dispatch t
    -> rpc:('q, 's, 'u, 'e) Rpc.State_rpc.t
    -> ('q, 's, 'u, 'e) dispatch t

  (** A specialization of [map] for the query type of a protocol. *)
  val map_query
    :  ('q1, 's, 'u, 'e) dispatch t
    -> f:('q2 -> 'q1)
    -> ('q2, 's, 'u, 'e) dispatch t

  (** A specialization of [map] for the state type of a protocol. *)
  val map_state
    :  ('q, 's1, 'u, 'e) dispatch t
    -> f:('s1 -> 's2)
    -> ('q, 's2, 'u, 'e) dispatch t

  (** A specialization of [map] for the update type of a protocol.

      Sometimes, [Caller.State_rpc.map_update] is not sufficient. For example, sometimes
      it might not be possible to convert the response to the desired type, in which case
      it may be appropriate to drop the value from the pipe entirely. For such cases, use
      [Caller.map_response] instead. It gives you access to the pipe itself, not just the
      values inside it, allowing you to use something like [Pipe.filter_map]. *)
  val map_update
    :  ('q, 's, 'u1, 'e) dispatch t
    -> f:('u1 -> 'u2)
    -> ('q, 's, 'u2, 'e) dispatch t

  (** Same as [map_update] but filters out some responses from the response pipe *)
  val filter_map_update
    :  ('q, 's, 'u1, 'e) dispatch t
    -> f:('u1 -> 'u2 option)
    -> ('q, 's, 'u2, 'e) dispatch t

  (** A specialization of [map] for the error type of a protocol. *)
  val map_error
    :  ('q, 's, 'u, 'e1) dispatch t
    -> f:('e1 -> 'e2)
    -> ('q, 's, 'u, 'e2) dispatch t
end

(** High level functions for working with callers in the style of
    [Async.Rpc.One_way.dispatch]. *)
module One_way : sig
  open Async_rpc_kernel

  type 'msg dispatch := 'msg -> unit Or_error.t

  (** Determine which supported dispatch strategy to use and invoke the chosen rpcs. *)
  val dispatch_multi
    :  'msg dispatch t
    -> Versioned_rpc.Connection_with_menu.t
    -> 'msg dispatch

  (** Create a new caller supporting a single rpc. *)
  val singleton : 'msg Rpc.One_way.t -> 'msg dispatch t

  (** Add support for dispatching another rpc. [dispatch_multi] will prefer this rpc over
      the ones the caller already supports. *)
  val add : 'msg dispatch t -> rpc:'msg Rpc.One_way.t -> 'msg dispatch t

  (** A specialization of [map] for the query type of a protocol. *)
  val map_query : 'msg1 dispatch t -> f:('msg2 -> 'msg1) -> 'msg2 dispatch t
end

(** High level functions for working with callers in the style of
    [Async.Rpc.One_way.dispatch_exn]. *)
module One_way_exn : sig
  open Async_rpc_kernel

  type 'msg dispatch := 'msg -> unit

  (** Determine which supported dispatch strategy to use and invoke the chosen rpcs. *)
  val dispatch_multi
    :  'msg dispatch t
    -> Versioned_rpc.Connection_with_menu.t
    -> 'msg dispatch

  (** Create a new caller supporting a single rpc. *)
  val singleton : 'msg Rpc.One_way.t -> 'msg dispatch t

  (** Add support for dispatching another rpc. [dispatch_multi] will prefer this rpc over
      the ones the caller already supports. *)
  val add : 'msg dispatch t -> rpc:'msg Rpc.One_way.t -> 'msg dispatch t

  (** A specialization of [map] for the query type of a protocol. *)
  val map_query : 'msg1 dispatch t -> f:('msg2 -> 'msg1) -> 'msg2 dispatch t
end

(** High level functions for working with callers in the style of
    [Async.Rpc.One_way.dispatch']. There is no [Babel.Caller.One_way'.dispatch_multi]. Use
    the more general [Babel.Caller.to_dispatch_fun]. *)
module One_way' : sig
  open Async_rpc_kernel

  type 'msg dispatch := 'msg -> unit Async_rpc_kernel.Rpc_result.t

  (** Create a new caller supporting a single rpc. *)
  val singleton : 'msg Rpc.One_way.t -> 'msg dispatch t

  (** Add support for dispatching another rpc. [dispatch_multi] will prefer this rpc over
      the ones the caller already supports. *)
  val add : 'msg dispatch t -> rpc:'msg Rpc.One_way.t -> 'msg dispatch t

  (** A specialization of [map] for the query type of a protocol. *)
  val map_query : 'msg1 dispatch t -> f:('msg2 -> 'msg1) -> 'msg2 dispatch t
end

(** High level functions for working with callers in the style of
    [Streamable.Plain_rpc.dispatch]. *)
module Streamable_plain_rpc : sig
  type ('q, 'r) dispatch := 'q -> 'r Or_error.t Or_error.t Deferred.t

  (** Determine which supported dispatch strategy to use and invoke the chosen rpcs. *)
  val dispatch_multi
    :  ('q, 'r) dispatch t
    -> Versioned_rpc.Connection_with_menu.t
    -> ('q, 'r) dispatch

  (** Create a new caller supporting a single rpc. *)
  val singleton : ('q, 'r) Streamable.Plain_rpc.t -> ('q, 'r) dispatch t

  (** Add support for dispatching another rpc. [dispatch_multi] will prefer this rpc over
      the ones the caller already supports. *)
  val add
    :  ('q, 'r) dispatch t
    -> rpc:('q, 'r) Streamable.Plain_rpc.t
    -> ('q, 'r) dispatch t

  (** A specialization of [map] for the query type of a protocol. *)
  val map_query : ('q1, 'r) dispatch t -> f:('q2 -> 'q1) -> ('q2, 'r) dispatch t

  (** A specialization of [map] for the response type of a protocol. *)
  val map_response : ('q, 'r1) dispatch t -> f:('r1 -> 'r2) -> ('q, 'r2) dispatch t
end

(** High level functions for working with callers in the style of
    [Streamable.Pipe_rpc.dispatch]. *)
module Streamable_pipe_rpc : sig
  type ('q, 'r) dispatch := 'q -> 'r Pipe.Reader.t Or_error.t Or_error.t Deferred.t

  (** Determine which supported dispatch strategy to use and invoke the chosen rpcs. To
      unsubscribe, you can close the pipe. *)
  val dispatch_multi
    :  ('q, 'r) dispatch t
    -> Versioned_rpc.Connection_with_menu.t
    -> ('q, 'r) dispatch

  (** Create a new caller supporting a single rpc. *)
  val singleton : ('q, 'r) Streamable.Pipe_rpc.t -> ('q, 'r) dispatch t

  (** Add support for dispatching another rpc. [dispatch_multi] will prefer this rpc over
      the ones the caller already supports. *)
  val add
    :  ('q, 'r) dispatch t
    -> rpc:('q, 'r) Streamable.Pipe_rpc.t
    -> ('q, 'r) dispatch t

  (** A specialization of [map] for the query type of a protocol. *)
  val map_query : ('q1, 'r) dispatch t -> f:('q2 -> 'q1) -> ('q2, 'r) dispatch t

  (** A specialization of [map] for the response type of a protocol.

      Sometimes, [Caller.Streamable_pipe_rpc.map_response] is not sufficient. For example,
      sometimes it might not be possible to convert the response to the desired type, in
      which case it may be appropriate to drop the value from the pipe entirely. For such
      cases, use [Caller.map_response] instead. It gives you access to the pipe itself,
      not just the values inside it, allowing you to use something like [Pipe.filter_map]. *)
  val map_response : ('q, 'r1) dispatch t -> f:('r1 -> 'r2) -> ('q, 'r2) dispatch t

  (** Same as [map_response] but filters out some responses from the response pipe *)
  val filter_map_response
    :  ('q, 'r1) dispatch t
    -> f:('r1 -> 'r2 option)
    -> ('q, 'r2) dispatch t
end

(** High level functions for working with callers in the style of
    [Streamable.State_rpc.dispatch]. *)
module Streamable_state_rpc : sig
  type ('q, 's, 'u) dispatch :=
    'q -> ('s * 'u Pipe.Reader.t) Or_error.t Or_error.t Deferred.t

  (** Determine which supported dispatch strategy to use and invoke the chosen rpcs. To
      unsubscribe, you can close the pipe. *)
  val dispatch_multi
    :  ('q, 's, 'u) dispatch t
    -> Versioned_rpc.Connection_with_menu.t
    -> ('q, 's, 'u) dispatch

  (** Create a new caller supporting a single rpc. *)
  val singleton : ('q, 's, 'u) Streamable.State_rpc.t -> ('q, 's, 'u) dispatch t

  (** Add support for dispatching another rpc. [dispatch_multi] will prefer this rpc over
      the ones the caller already supports. *)
  val add
    :  ('q, 's, 'u) dispatch t
    -> rpc:('q, 's, 'u) Streamable.State_rpc.t
    -> ('q, 's, 'u) dispatch t

  (** A specialization of [map] for the query type of a protocol. *)
  val map_query : ('q1, 's, 'u) dispatch t -> f:('q2 -> 'q1) -> ('q2, 's, 'u) dispatch t

  (** A specialization of [map] for the state type of a protocol. *)
  val map_state : ('q, 's1, 'u) dispatch t -> f:('s1 -> 's2) -> ('q, 's2, 'u) dispatch t

  (** A specialization of [map] for the update type of a protocol.

      Sometimes, [Caller.Streamable_state_rpc.map_update] is not sufficient. For example,
      sometimes it might not be possible to convert the response to the desired type, in
      which case it may be appropriate to drop the value from the pipe entirely. For such
      cases, use [Caller.map_response] instead. It gives you access to the pipe itself,
      not just the values inside it, allowing you to use something like [Pipe.filter_map]. *)
  val map_update : ('q, 's, 'u1) dispatch t -> f:('u1 -> 'u2) -> ('q, 's, 'u2) dispatch t

  (** Same as [map_update] but filters out some responses from the response pipe *)
  val filter_map_update
    :  ('q, 's, 'u1) dispatch t
    -> f:('u1 -> 'u2 option)
    -> ('q, 's, 'u2) dispatch t
end

(** Produce a dispatch function that is compatible with the given version menu. You should
    prefer to dispatch using the more specialized functions above. You would use this
    function if you want to precompute the version menu checks or if you have changed the
    type to something incompatible with the more specialized functions for particular
    types of Rpc protocols. *)
val to_dispatch_fun
  :  'a t
  -> Versioned_rpc.Menu.t
  -> (Async_rpc_kernel.Rpc.Connection.t -> 'a) Or_error.t

(** Given a version menu, returns the description of the RPC that will be called by the
    dispatch function. *)
val description
  :  _ t
  -> Versioned_rpc.Menu.t
  -> Async_rpc_kernel.Rpc.Description.t Or_error.t

(** Transform the dispatch function generated by a caller. Since in practice we are
    usually working with functions, it may be enlightening to think of the type as:

    {[
      val map : ('a1 -> 'b1) t -> f:(('a1 -> 'b1) -> 'a2 -> 'b2) -> ('a2 -> 'b2) t
    ]}

    Most of the time you should use the higher-level [map_query] and [map_response]
    functions instead of [map]. [map] is intended to be used for more radical changes to
    an implementation, such as to change the type of rpc protocol.

    {[
      to_dispatch_fun (map t ~f) menu = Or_error.map (to_dispatch_fun t menu) ~f
    ]} *)
val map : 'a t -> f:('a -> 'b) -> 'b t

(** A specialization of [map] for the query type of a protocol.

    In practice, most implementations will return deferreds or other complex types, so you
    should prefer to use more specialized functions, such as [Rpc.map_query]. *)
val map_query : ('a -> 'b) t -> f:('c -> 'a) -> ('c -> 'b) t

(** A specialization of [map] for the response type of a protocol.

    In practice, most implementations will return deferreds or other complex types, so you
    should prefer to use more specialized functions, such as [Rpc.map_response]. *)
val map_response : ('a -> 'b) t -> f:('b -> 'c) -> ('a -> 'c) t

(** Return whether any of strategies in the rpc protocol menu are supported by the
    dispatch function. *)
val can_dispatch : _ t -> Versioned_rpc.Connection_with_menu.t -> bool

module Strategy : sig
  (** A dispatch strategy that already knows which RPC to dispatch. *)
  type +'a t

  val dispatch : 'a t -> Async_rpc_kernel.Rpc.Connection.t -> 'a
  val description : _ t -> Async_rpc_kernel.Rpc.Description.t
  val shape : _ t -> Shape.t
end

val to_strategy : 'a t -> Versioned_rpc.Menu.t -> 'a Strategy.t Or_error.t

module Expert : sig
  (** [return rpc f] gives a [t] where {!to_dispatch_fun} returns [Ok (fun _conn -> f)] if
      [rpc] is in the menu. This may be used for more specialized uses of rpcs like
      [Polling_state_rpc] *)
  val return : Generic_rpc.t -> 'a -> 'a t
end
