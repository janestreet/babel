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
*)

val write : 'a t -> 'a -> [ `Flushed of unit Deferred.t | `Closed ]
val write_without_pushback : 'a t -> 'a -> [ `Ok | `Closed ]
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
end

(** Like [Rpc.Pipe_rpc.Direct_stream_writer.Group], but with the ability to maintain
    multiple subgroups.  One [Rpc.Pipe_rpc.Direct_stream_writer.Group] is maintained per
    implemented RPC, so values are only converted and serialized once per RPC.

    Since values are only converted once, the functions passed to
    [Babel.Callee.Pipe_rpc_direct.{filter_,}map_response] must be pure, or unexpected
    results can occur.  To determine whether two direct stream writers can be combined,
    [Direct_stream_writer.t] contains a type id for the output type, and a unique
    [Transformation_id] that is intended to uniquely identify the conversion function from
    the input type to the output type.

    Because the library has no way of automatically identifying whether two ['a
    Direct_stream_writer.t]'s with output type ['b] have the same conversion function ['a
    -> 'b option], users must promise that the [Transformation_id.t] is never re-used with
    a different function.  To take advantage of the grouping, the [Expert] functions above
    must be used to create the writer.  Using the non expert writers will result in
    a direct stream writer which cannot be combined and will be put in its own group.
    [Babel.Callee.Pipe_rpc_direct.{filter_,}map_response] use the [Expert] functions so they
    can be combined. *)
module Group : sig
  type 'a writer := 'a t
  type 'a t

  val create : unit -> _ t

  (** Add a direct stream writer to the group.  Raises if
      [Rpc.Pipe_rpc.Direct_stream_writer.Group.add_exn] would raise *)
  val add_exn : 'a t -> 'a writer -> unit

  (** Write a message to all direct writers in the group and then waits for flushed. *)
  val write : 'a t -> 'a -> unit Deferred.t

  (** Write a message to all direct writers in the group without waiting for flushed. *)
  val write_without_pushback : 'a t -> 'a -> unit


  module For_testing : sig
    val num_subgroups : _ t -> int
  end
end
