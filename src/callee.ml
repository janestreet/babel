open Core
open Async_kernel
open Async_rpc_kernel
module Fn = Babel_fn

module Description = struct
  include Rpc.Description
  include Comparable.Make_plain (Rpc.Description)
end

module Implementer = struct
  type 'a t =
    { implement :
        's. ?on_exception:Rpc.On_exception.t -> ('s -> 'a) -> 's Rpc.Implementation.t
    ; rpc : Generic_rpc.t
    }

  let map { implement; rpc } ~f =
    let implement ?on_exception implementation =
      implement ?on_exception (fun s -> f (implementation s))
    in
    { implement; rpc }
  ;;
end

open Implementer

(* We collect errors about collisions in [t] to avoid polluting client code with a bunch
   of [Or_error] stuff. The errors are eventually exposed when returning something other
   than a [t]. *)
type 'a t = ('a Implementer.t, Shape.Set.t Lazy.t) Result.t Description.Map.t

module Validated = struct
  type 'a t = (Rpc.Description.t * 'a Implementer.t) list
end

let validate (t : _ t) : _ Validated.t Or_error.t =
  Map.to_alist t
  |> List.map ~f:(fun (description, implementer) ->
    match implementer with
    | Ok implementer -> Ok (description, implementer)
    | Error shapes ->
      Or_error.error_s
        [%message
          "Duplicate rpcs" (description : Rpc.Description.t) (shapes : Shape.Set.t Lazy.t)])
  |> Or_error.combine_errors
;;

let implement_multi ?on_exception t ~f =
  let open Or_error.Let_syntax in
  let%map validated = validate t in
  List.map validated ~f:(fun (description, implementer) ->
    implementer.implement ?on_exception (fun s -> f s description))
;;

let implement_multi_exn ?on_exception t ~f = ok_exn (implement_multi ?on_exception t ~f)

let shapes t =
  let open Or_error.Let_syntax in
  validate t
  >>| List.map ~f:(fun (description, implementer) ->
    description, Generic_rpc.shape implementer.rpc)
;;

let supported_rpcs t =
  Or_error.map
    (validate t)
    ~f:(List.map ~f:(fun ((_ : Description.t), implementer) -> implementer.rpc))
;;

let supported_rpcs_exn t = ok_exn (supported_rpcs t)
let shapes_exn t = ok_exn (shapes t)
let descriptions t = Or_error.map (validate t) ~f:(List.map ~f:fst)
let descriptions_exn t = List.map (shapes_exn t) ~f:fst

let print_shapes t =
  Or_error.map (shapes t) ~f:(fun shapes ->
    List.map shapes ~f:(fun (description, shape) ->
      description.name, (description.version, shape))
    |> String.Map.of_alist_multi)
  |> [%sexp_of: (int * Shape.t) list String.Map.t Or_error.t]
  |> print_s
;;

let map t =
  Tilde_f.Let_syntax.(
    Tilde_f.of_local (Map.map t) >>= Tilde_f.of_local_k Result.map >>= Implementer.map)
;;

let of_list ts =
  let shape_set = function
    | Ok { implement = _; rpc } -> lazy (Shape.Set.singleton (Generic_rpc.shape rpc))
    | Error shapes -> shapes
  in
  List.reduce ts ~f:(fun t1 t2 ->
    Map.merge_skewed t1 t2 ~combine:(fun ~key:_ a b ->
      Error
        (let%map.Lazy a = shape_set a
         and b = shape_set b in
         Set.union a b)))
  |> Option.value ~default:Description.Map.empty
;;

let map_query t = Tilde_f.Let_syntax.(map t >>= Fn.map_input)
let map_response t = Tilde_f.Let_syntax.(map t >>= Fn.map)
let adder t ~rpc ~f = of_list [ t; f rpc ]

let singleton description implementer =
  Description.Map.singleton description (Ok implementer)
;;

module Rpc = struct
  let singleton rpc =
    singleton
      (Rpc.Rpc.description rpc)
      { implement = (fun ?on_exception f -> Rpc.Rpc.implement ?on_exception rpc f)
      ; rpc = Rpc rpc
      }
  ;;

  let add = adder ~f:singleton
  let map_query = map_query
  let map_response t = Tilde_f.Let_syntax.(map_response t >>= Deferred.map)
end

module Rpc' = struct
  open Async_rpc_kernel

  let singleton rpc =
    singleton
      (Rpc.Rpc.description rpc)
      { implement = (fun ?on_exception f -> Rpc.Rpc.implement' ?on_exception rpc f)
      ; rpc = Rpc rpc
      }
  ;;

  let add = adder ~f:singleton
  let map_query = map_query
  let map_response = map_response
end

module Pipe_rpc = struct
  open Async_rpc_kernel

  let singleton ?leave_open_on_exception rpc =
    singleton
      (Rpc.Pipe_rpc.description rpc)
      { implement =
          (fun ?on_exception f ->
            Rpc.Pipe_rpc.implement ?on_exception ?leave_open_on_exception rpc f)
      ; rpc = Pipe rpc
      }
  ;;

  let add ?leave_open_on_exception = adder ~f:(singleton ?leave_open_on_exception)
  let map_query = map_query

  let map_error t =
    Tilde_f.Let_syntax.(
      map_response t >>= Deferred.map >>= Tilde_f.of_local_k Result.map_error)
  ;;

  let filter_map_response t =
    Tilde_f.Let_syntax.(
      map_response t
      >>= Deferred.map
      >>= Tilde_f.of_local_k Result.map
      >>= Pipe.filter_map ?max_queue_length:None)
  ;;

  let map_response t =
    Tilde_f.Let_syntax.(
      map_response t
      >>= Deferred.map
      >>= Tilde_f.of_local_k Result.map
      >>= Pipe_extended.map_batched)
  ;;
end

module Pipe_rpc_direct = struct
  open Async_rpc_kernel
  module Direct_stream_writer = Direct_stream_writer

  let singleton ?leave_open_on_exception rpc =
    let description = Rpc.Pipe_rpc.description rpc in
    let description_sexp = [%sexp_of: Rpc.Description.t] description in
    let witness =
      Type_equal.Id.create
        ~name:
          [%string "[Babel.Callee.Pipe_rpc_direct] type id for %{description_sexp#Sexp}"]
        [%sexp_of: _]
    in
    singleton
      description
      { implement =
          (fun ?on_exception f ->
            Rpc.Pipe_rpc.implement_direct
              ?on_exception
              ?leave_open_on_exception
              rpc
              (fun connection_state query writer ->
                 f
                   connection_state
                   query
                   (Direct_stream_writer.Expert.create_witnessed writer ~witness)))
      ; rpc = Pipe rpc
      }
  ;;

  let add ?leave_open_on_exception = adder ~f:(singleton ?leave_open_on_exception)
  let map_query = map_query

  let map_error t =
    Tilde_f.Let_syntax.(
      map_response t >>= Fn.map >>= Deferred.map >>= Tilde_f.of_local_k Result.map_error)
  ;;

  let filter_map_response t ~f =
    let id = Direct_stream_writer.Expert.Transformation_id.create () in
    Tilde_f.Let_syntax.(
      map_response t
      >>= Fn.map_input
      >>= Direct_stream_writer.Expert.filter_map_input_with_id ~id)
      ~f
  ;;

  let map_response t ~f =
    let id = Direct_stream_writer.Expert.Transformation_id.create () in
    Tilde_f.Let_syntax.(
      map_response t
      >>= Fn.map_input
      >>= Direct_stream_writer.Expert.map_input_with_id ~id)
      ~f
  ;;
end

module State_rpc = struct
  open Async_rpc_kernel

  let singleton ?leave_open_on_exception rpc =
    singleton
      (Rpc.State_rpc.description rpc)
      { implement =
          (fun ?on_exception f ->
            Rpc.State_rpc.implement ?on_exception ?leave_open_on_exception rpc f)
      ; rpc = State rpc
      }
  ;;

  let add ?leave_open_on_exception = adder ~f:(singleton ?leave_open_on_exception)
  let map_query = map_query

  let map_state t =
    Tilde_f.Let_syntax.(
      map_response t >>= Deferred.map >>= Tilde_f.of_local_k Result.map >>= Tuple2.map_fst)
  ;;

  let map_update t =
    Tilde_f.Let_syntax.(
      map_response t
      >>= Deferred.map
      >>= Tilde_f.of_local_k Result.map
      >>= Tuple2.map_snd
      >>= Pipe_extended.map_batched)
  ;;

  let filter_map_update t =
    Tilde_f.Let_syntax.(
      map_response t
      >>= Deferred.map
      >>= Tilde_f.of_local_k Result.map
      >>= Tuple2.map_snd
      >>= Pipe.filter_map ?max_queue_length:None)
  ;;

  let map_error t =
    Tilde_f.Let_syntax.(
      map_response t >>= Deferred.map >>= Tilde_f.of_local_k Result.map_error)
  ;;
end

module State_rpc_direct = struct
  open Async_rpc_kernel
  module Direct_stream_writer = Direct_stream_writer

  let singleton ?leave_open_on_exception rpc =
    let description = Rpc.State_rpc.description rpc in
    let description_sexp = [%sexp_of: Rpc.Description.t] description in
    let witness =
      Type_equal.Id.create
        ~name:
          [%string "[Babel.Callee.State_rpc_direct] type id for %{description_sexp#Sexp}"]
        [%sexp_of: _]
    in
    singleton
      description
      { implement =
          (fun ?on_exception f ->
            Rpc.State_rpc.implement_direct
              ?on_exception
              ?leave_open_on_exception
              rpc
              (fun connection_state query writer ->
                 f
                   connection_state
                   query
                   (Direct_stream_writer.Expert.create_witnessed writer ~witness)))
      ; rpc = State rpc
      }
  ;;

  let add ?leave_open_on_exception = adder ~f:(singleton ?leave_open_on_exception)
  let map_query = map_query

  let map_state t =
    Tilde_f.Let_syntax.(
      map_response t >>= Fn.map >>= Deferred.map >>= Tilde_f.of_local_k Result.map)
  ;;

  let filter_map_update t ~f =
    let id = Direct_stream_writer.Expert.Transformation_id.create () in
    Tilde_f.Let_syntax.(
      map_response t
      >>= Fn.map_input
      >>= Direct_stream_writer.Expert.filter_map_input_with_id ~id)
      ~f
  ;;

  let map_update t ~f =
    let id = Direct_stream_writer.Expert.Transformation_id.create () in
    Tilde_f.Let_syntax.(
      map_response t
      >>= Fn.map_input
      >>= Direct_stream_writer.Expert.map_input_with_id ~id)
      ~f
  ;;

  let map_error t =
    Tilde_f.Let_syntax.(
      map_response t >>= Fn.map >>= Deferred.map >>= Tilde_f.of_local_k Result.map_error)
  ;;
end

module One_way = struct
  open Async_rpc_kernel

  let singleton rpc =
    singleton
      (Rpc.One_way.description rpc)
      { implement =
          (fun ?on_exception f ->
            Rpc.One_way.implement
              ~on_exception:(Option.value on_exception ~default:Close_connection)
              rpc
              f)
      ; rpc = One_way rpc
      }
  ;;

  let add = adder ~f:singleton
  let map_msg = map_query
end

module Streamable_plain_rpc = struct
  let singleton rpc =
    singleton
      (Streamable.Plain_rpc.description rpc)
      { implement =
          (fun ?on_exception f -> Streamable.Plain_rpc.implement ?on_exception rpc f)
      ; rpc = Streamable_plain rpc
      }
  ;;

  let add = adder ~f:singleton
  let map_query = map_query

  let map_response t =
    Tilde_f.Let_syntax.(
      map_response t >>= Deferred.map >>= Tilde_f.of_local_k Or_error.map)
  ;;
end

module Streamable_pipe_rpc = struct
  let singleton ?leave_open_on_exception rpc =
    singleton
      (Streamable.Pipe_rpc.description rpc)
      { implement =
          (fun ?on_exception f ->
            Streamable.Pipe_rpc.implement ?on_exception ?leave_open_on_exception rpc f)
      ; rpc = Streamable_pipe rpc
      }
  ;;

  let add ?leave_open_on_exception = adder ~f:(singleton ?leave_open_on_exception)
  let map_query = map_query

  let filter_map_response t =
    Tilde_f.Let_syntax.(
      map_response t
      >>= Deferred.map
      >>= Tilde_f.of_local_k Result.map
      >>= Pipe.filter_map ?max_queue_length:None)
  ;;

  let map_response t =
    Tilde_f.Let_syntax.(
      map_response t
      >>= Deferred.map
      >>= Tilde_f.of_local_k Result.map
      >>= Pipe_extended.map_batched)
  ;;
end

module Streamable_state_rpc = struct
  let singleton ?leave_open_on_exception rpc =
    singleton
      (Streamable.State_rpc.description rpc)
      { implement =
          (fun ?on_exception f ->
            Streamable.State_rpc.implement ?on_exception ?leave_open_on_exception rpc f)
      ; rpc = Streamable_state rpc
      }
  ;;

  let add ?leave_open_on_exception = adder ~f:(singleton ?leave_open_on_exception)
  let map_query = map_query

  let map_state t =
    Tilde_f.Let_syntax.(
      map_response t >>= Deferred.map >>= Tilde_f.of_local_k Result.map >>= Tuple2.map_fst)
  ;;

  let map_update t =
    Tilde_f.Let_syntax.(
      map_response t
      >>= Deferred.map
      >>= Tilde_f.of_local_k Result.map
      >>= Tuple2.map_snd
      >>= Pipe_extended.map_batched)
  ;;

  let filter_map_update t =
    Tilde_f.Let_syntax.(
      map_response t
      >>= Deferred.map
      >>= Tilde_f.of_local_k Result.map
      >>= Tuple2.map_snd
      >>= Pipe.filter_map ?max_queue_length:None)
  ;;
end
