open! Core
open Async_kernel
open Async_rpc_kernel

(** Like [Rpc.Pipe_rpc.Direct_stream_writer], but with the additional ability to map
    functions over its inputs. *)
type -'a t

(** Create a Babel-friendly direct stream writer from a raw Async_rpc one. *)
val create : 'a Rpc.Pipe_rpc.Direct_stream_writer.t -> 'a t

(** The result of [map_input] and [filter_map_input] writes to the same underlying
    [Rpc.Pipe_rpc.Direct_stream_writer.t] that the original [t] does. *)

val map_input : 'a t -> f:('b -> 'a) -> 'b t
val filter_map_input : 'a t -> f:('b -> 'a option) -> 'b t

(** These functions work the same way as the ones in [Rpc.Pipe_rpc.Direct_stream_writer].

    Note that like in [Rpc.Pipe_rpc.Direct_stream_writer], if [started t] is not
    determined, there are some sharp edges to [write]:
    - Messages are enqueued until the writer does start
    - The flushed deferred returned does not actually correspond to the message being
      flushed *)

val write : 'a t -> 'a -> [ `Flushed of unit Deferred.t | `Closed ]
val write_without_pushback : 'a t -> 'a -> [ `Ok | `Closed ]
val started : _ t -> unit Deferred.t
val close : _ t -> unit
val closed : _ t -> unit Deferred.t
val flushed : _ t -> unit Deferred.t
val is_closed : _ t -> bool

module Expert : sig
  module Transformation_id : Unique_id

  val create_witnessed
    :  'a Rpc.Pipe_rpc.Direct_stream_writer.t
    -> witness:'a Type_equal.Id.t
    -> 'a t

  val map_input_with_id : 'a t -> f:('b -> 'a) -> id:Transformation_id.t -> 'b t

  val filter_map_input_with_id
    :  'a t
    -> f:('b -> 'a option)
    -> id:Transformation_id.t
    -> 'b t

  (** See [Rpc.Pipe_rpc.Direct_stream_writer.Expert.write_without_pushback]. Babel will
      not perform any transformation on the input buffer. *)
  val write_without_pushback
    :  _ t
    -> buf:Bigstring.t
    -> pos:int
    -> len:int
    -> [ `Ok | `Closed ]

  (** See [Rpc.Pipe_rpc.Direct_stream_writer.Expert.schedule_write]. Babel will not
      perform any transformation on the input buffer. *)
  val schedule_write
    :  _ t
    -> buf:Bigstring.t
    -> pos:int
    -> len:int
    -> local_ [ `Closed | `Flushed of unit Deferred.t global ]
end

(** Like [Rpc.Pipe_rpc.Direct_stream_writer.Group], but with the ability to maintain
    multiple subgroups. One [Rpc.Pipe_rpc.Direct_stream_writer.Group] is maintained per
    implemented RPC, so values are only converted and serialized once per RPC.

    Since values are only converted once, the functions passed to
    [Babel.Callee.Pipe_rpc_direct.{filter_,}map_response] must be pure, or unexpected
    results can occur. To determine whether two direct stream writers can be combined,
    [Direct_stream_writer.t] contains a type id for the output type, and a unique
    [Transformation_id] that is intended to uniquely identify the conversion function from
    the input type to the output type.

    Because the library has no way of automatically identifying whether two
    ['a Direct_stream_writer.t]'s with output type ['b] have the same conversion function
    ['a -> 'b option], users must promise that the [Transformation_id.t] is never re-used
    with a different function. To take advantage of the grouping, the [Expert] functions
    above must be used to create the writer. Using the non expert writers will result in a
    direct stream writer which cannot be combined and will be put in its own group.
    [Babel.Callee.Pipe_rpc_direct.{filter_,}map_response] use the [Expert] functions so
    they can be combined. *)
module Group : sig
  type 'a writer := 'a t
  type 'a t

  (** When [buffer] is provided, all subgroups will share the same buffer. *)
  val create : ?buffer:Rpc.Pipe_rpc.Direct_stream_writer.Group.Buffer.t -> unit -> _ t

  (** [create_storing_last_value_and_sending_on_add] will create a group that will
      automatically send a copy of the last value written to each new writer when it's
      added to the group. It's split out as a separate function from [create] as it's not
      safe to re-use a buffer between multiple different groups in this case, as the
      previous value is stored in the buffer. *)
  val create_storing_last_value_and_sending_on_add : unit -> _ t

  (** Add a direct stream writer to the group. Raises if
      [Rpc.Pipe_rpc.Direct_stream_writer.Group.add_exn] would raise. If
      [create_storing_last_value_and_sending_on_add] was used to create the group,
      [add_exn] will additionally write the stored last value to the writer. This will
      only involve conversion and serialization if the new writer uses a different
      protocol than the other writers in the group. *)
  val add_exn : 'a t -> 'a writer -> unit

  (** Remove a direct stream writer from the group. Writers are automatically removed when
      closed - see [Rpc.Pipe_rpc.Direct_stream_writer.Group.remove] *)
  val remove : 'a t -> 'a writer -> unit

  (** Write a message to all direct writers in the group and then waits for flushed. *)
  val write : 'a t -> 'a -> unit Deferred.t

  (** Write a message to all direct writers in the group without waiting for flushed. *)
  val write_without_pushback : 'a t -> 'a -> unit

  (** [flushed_or_closed t] is determined when the underlying writer for each member of
      [t] is flushed or closed. *)
  val flushed_or_closed : 'a t -> unit Deferred.t

  (** Number of elements in the group *)
  val length : _ t -> int

  module For_testing : sig
    val num_subgroups : _ t -> int
  end
end
