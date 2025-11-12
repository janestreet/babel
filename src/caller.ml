open Core
open Async_kernel
open Async_rpc_kernel
module Fn = Babel_fn

module Strategy = struct
  type 'a t =
    { dispatch : Rpc.Connection.t -> 'a
    ; rpc : Generic_rpc.t
    }
  [@@deriving fields ~getters]

  let map { dispatch; rpc } ~f =
    let dispatch conn = f (dispatch conn) in
    { dispatch; rpc }
  ;;

  let description t = Generic_rpc.description t.rpc
  let shape t = Generic_rpc.shape t.rpc
  let in_menu t menu = Versioned_rpc.Menu.mem menu (description t)
end

open Strategy

type 'a t = 'a Strategy.t Nonempty_list.t

let map t ~f = Nonempty_list.map t ~f:(fun s -> Strategy.map ~f s)
let shapes t = Nonempty_list.map t ~f:(fun s -> Strategy.description s, Strategy.shape s)
let supported_rpcs t = Nonempty_list.map t ~f:Strategy.rpc
let descriptions t = Nonempty_list.map t ~f:Strategy.description

let print_shapes t =
  print_s [%sexp (shapes t : (Rpc.Description.t * Shape.t) Nonempty_list.t)]
;;

let to_strategy t menu =
  match Nonempty_list.find t ~f:(fun strategy -> Strategy.in_menu strategy menu) with
  | Some strategy -> Ok strategy
  | None ->
    let callee_rpcs = Versioned_rpc.Menu.supported_rpcs menu in
    let caller_rpcs = descriptions t in
    Or_error.error_s
      [%message
        "Could not find any supported rpc in callee"
          (callee_rpcs : Rpc.Description.t list)
          (caller_rpcs : Rpc.Description.t Nonempty_list.t)]
;;

let to_dispatch_fun t menu = to_strategy t menu |> Or_error.map ~f:Strategy.dispatch
let description t menu = to_strategy t menu |> Or_error.map ~f:Strategy.description
let of_list_decreasing_preference = Nonempty_list.concat
let map_query t = Tilde_f.Let_syntax.(map t >>= Fn.map_input)
let map_response t = Tilde_f.Let_syntax.(map t >>= Fn.map)

let can_dispatch t connection_with_menu =
  let menu = Versioned_rpc.Connection_with_menu.menu connection_with_menu in
  to_strategy t menu |> Or_error.is_ok
;;

let dispatch_multi t connection_with_menu query ~on_error =
  let menu = Versioned_rpc.Connection_with_menu.menu connection_with_menu in
  match to_dispatch_fun t menu with
  | Ok f -> f (Versioned_rpc.Connection_with_menu.connection connection_with_menu) query
  | Error _ as error -> on_error error
;;

let dispatch_multi_or_error_deferred t connection_with_menu query =
  dispatch_multi t connection_with_menu query ~on_error:Deferred.return
;;

let dispatch_multi_or_error t connection_with_menu query =
  dispatch_multi t connection_with_menu query ~on_error:Fn.id
;;

let dispatch_multi_exn t connection_with_menu query =
  dispatch_multi t connection_with_menu query ~on_error:ok_exn
;;

let adder t ~rpc ~f = of_list_decreasing_preference [ f rpc; t ]

module Rpc = struct
  let dispatch_multi = dispatch_multi_or_error_deferred

  let singleton rpc =
    Nonempty_list.singleton { dispatch = Rpc.Rpc.dispatch rpc; rpc = Rpc rpc }
  ;;

  let add = adder ~f:singleton
  let map_query = map_query

  let map_response t =
    Tilde_f.Let_syntax.(
      map_response t >>= Deferred.map >>= Tilde_f.of_local_k Or_error.map)
  ;;
end

module Rpc' = struct
  open Async_rpc_kernel

  let singleton rpc =
    Nonempty_list.singleton { dispatch = Rpc.Rpc.dispatch' rpc; rpc = Rpc rpc }
  ;;

  let add = adder ~f:singleton
  let map_query = map_query

  let map_response t =
    Tilde_f.Let_syntax.(map_response t >>= Deferred.map >>= Tilde_f.of_local_k Result.map)
  ;;

  let to_rpc_style caller =
    Nonempty_list.map caller ~f:(fun { dispatch; rpc } ->
      { rpc
      ; dispatch =
          (fun conn query ->
            dispatch conn query
            >>| Rpc_result.or_error
                  ~rpc_description:(Generic_rpc.description rpc)
                  ~connection_description:(Rpc.Connection.description conn)
                  ~connection_close_started:
                    (Rpc.Connection.close_reason conn ~on_close:`started))
      })
  ;;
end

module Rpc_exn = struct
  open Async_rpc_kernel

  let dispatch_multi = dispatch_multi_exn

  let singleton rpc =
    Nonempty_list.singleton { dispatch = Rpc.Rpc.dispatch_exn rpc; rpc = Rpc rpc }
  ;;

  let add = adder ~f:singleton
  let map_query = map_query
  let map_response t = Tilde_f.Let_syntax.(map_response t >>= Deferred.map)
end

module Pipe_rpc = struct
  open Async_rpc_kernel

  let dispatch_multi = dispatch_multi_or_error_deferred

  let dispatch_multi_with_close_reason t connection query =
    Deferred.Or_error.map (dispatch_multi t connection query) ~f:(fun result ->
      Result.map
        result
        ~f:Async_rpc_kernel.Rpc.Pipe_rpc.pipe_with_writer_error_of_pipe_and_metadata)
  ;;

  let singleton rpc =
    Nonempty_list.singleton { dispatch = Rpc.Pipe_rpc.dispatch rpc; rpc = Pipe rpc }
  ;;

  let add = adder ~f:singleton
  let map_query = map_query

  let map_error t =
    Tilde_f.Let_syntax.(
      map_response t
      >>= Deferred.map
      >>= Tilde_f.of_local_k Or_error.map
      >>= Tilde_f.of_local_k Result.map_error)
  ;;

  let filter_map_response t =
    let open Tilde_f.Let_syntax in
    map_response t
    >>= Deferred.map
    >>= Tilde_f.of_local_k Or_error.map
    >>= Tilde_f.of_local_k Result.map
    >>= Tuple2.map_fst
    >>= Pipe.filter_map ?max_queue_length:None
  ;;

  let map_response t =
    let open Tilde_f.Let_syntax in
    map_response t
    >>= Deferred.map
    >>= Tilde_f.of_local_k Or_error.map
    >>= Tilde_f.of_local_k Result.map
    >>= Tuple2.map_fst
    >>= Pipe_extended.map_batched
  ;;
end

module Pipe_rpc' = struct
  open Async_rpc_kernel

  let singleton rpc =
    Nonempty_list.singleton { dispatch = Rpc.Pipe_rpc.dispatch' rpc; rpc = Pipe rpc }
  ;;

  let add = adder ~f:singleton
  let map_query = map_query

  let map_error t =
    Tilde_f.Let_syntax.(
      map_response t
      >>= Deferred.map
      >>= Tilde_f.of_local_k Result.map
      >>= Tilde_f.of_local_k Result.map_error)
  ;;

  let filter_map_response t =
    let open Tilde_f.Let_syntax in
    map_response t
    >>= Deferred.map
    >>= Tilde_f.of_local_k Result.map
    >>= Tilde_f.of_local_k Result.map
    >>= Tuple2.map_fst
    >>= Pipe.filter_map ?max_queue_length:None
  ;;

  let map_response t =
    let open Tilde_f.Let_syntax in
    map_response t
    >>= Deferred.map
    >>= Tilde_f.of_local_k Result.map
    >>= Tilde_f.of_local_k Result.map
    >>= Tuple2.map_fst
    >>= Pipe_extended.map_batched
  ;;

  let to_pipe_rpc_style caller =
    Nonempty_list.map caller ~f:(fun { dispatch; rpc } ->
      { rpc
      ; dispatch =
          (fun conn query ->
            dispatch conn query
            >>| Rpc_result.or_error
                  ~rpc_description:(Generic_rpc.description rpc)
                  ~connection_description:(Rpc.Connection.description conn)
                  ~connection_close_started:
                    (Rpc.Connection.close_reason conn ~on_close:`started))
      })
  ;;
end

module Pipe_rpc_exn = struct
  open Async_rpc_kernel

  let dispatch_multi = dispatch_multi_exn

  let dispatch_multi_with_close_reason t connection query =
    dispatch_multi t connection query
    >>| Async_rpc_kernel.Rpc.Pipe_rpc.pipe_with_writer_error_of_pipe_and_metadata
  ;;

  let singleton rpc =
    Nonempty_list.singleton { dispatch = Rpc.Pipe_rpc.dispatch_exn rpc; rpc = Pipe rpc }
  ;;

  let add = adder ~f:singleton
  let map_query = map_query

  let filter_map_response t =
    Tilde_f.Let_syntax.(
      map_response t
      >>= Deferred.map
      >>= Tuple2.map_fst
      >>= Pipe.filter_map ?max_queue_length:None)
  ;;

  let map_response t =
    Tilde_f.Let_syntax.(
      map_response t >>= Deferred.map >>= Tuple2.map_fst >>= Pipe_extended.map_batched)
  ;;
end

module Pipe_rpc_iter = struct
  open Async_rpc_kernel

  module Id = struct
    type t =
      | T :
          { rpc : _ Rpc.Pipe_rpc.t
          ; connection : Rpc.Connection.t
          ; id : Rpc.Pipe_rpc.Id.t
          }
          -> t
  end

  module Pipe_message = struct
    include Rpc.Pipe_rpc.Pipe_message

    let map (t : _ t) ~f =
      match t with
      | Update a -> Update (f a)
      | Closed _ as t -> t
    ;;
  end

  module Generator = struct
    let map t ~f ~f:k = f (t ~f:k)
  end

  let dispatch_multi t connection_with_menu query ~f =
    let menu = Versioned_rpc.Connection_with_menu.menu connection_with_menu in
    match to_dispatch_fun t menu with
    | Ok dispatch ->
      dispatch
        (Versioned_rpc.Connection_with_menu.connection connection_with_menu)
        query
        ~f
    | Error _ as error -> Deferred.return error
  ;;

  let singleton rpc =
    let dispatch connection query ~f =
      (let open Tilde_f.Let_syntax in
       Rpc.Pipe_rpc.dispatch_iter rpc connection query ~f
       |> Deferred.map
       >>= Tilde_f.of_local_k Or_error.map
       >>= Tilde_f.of_local_k Result.map)
        ~f:(fun id -> Id.T { rpc; connection; id })
    in
    Nonempty_list.singleton { dispatch; rpc = Pipe rpc }
  ;;

  let add = adder ~f:singleton
  let map_query = map_query

  let map_error t =
    let open Tilde_f.Let_syntax in
    map_response t
    >>= Generator.map
    >>= Deferred.map
    >>= Tilde_f.of_local_k Or_error.map
    >>= Tilde_f.of_local_k Result.map_error
  ;;

  let map_response t =
    Tilde_f.Let_syntax.(map_response t >>= Tilde_f.map >>= Pipe_message.map)
  ;;

  let abort (Id.T { rpc; connection; id }) = Rpc.Pipe_rpc.abort rpc connection id
end

module State_rpc = struct
  open Async_rpc_kernel

  let dispatch_multi = dispatch_multi_or_error_deferred

  let dispatch_multi_with_close_reason t connection query =
    Deferred.Or_error.map (dispatch_multi t connection query) ~f:(fun result ->
      Result.map result ~f:(fun (state, pipe, metadata) ->
        state, Rpc.Pipe_rpc.pipe_with_writer_error_of_pipe_and_metadata (pipe, metadata)))
  ;;

  let singleton rpc =
    Nonempty_list.singleton { dispatch = Rpc.State_rpc.dispatch rpc; rpc = State rpc }
  ;;

  let add = adder ~f:singleton
  let map_query = map_query

  let map_state t =
    Tilde_f.Let_syntax.(
      map_response t
      >>= Deferred.map
      >>= Tilde_f.of_local_k Or_error.map
      >>= Tilde_f.of_local_k Result.map
      >>= Tuple3.map_fst)
  ;;

  let filter_map_update t =
    let open Tilde_f.Let_syntax in
    map_response t
    >>= Deferred.map
    >>= Tilde_f.of_local_k Or_error.map
    >>= Tilde_f.of_local_k Result.map
    >>= Tuple3.map_snd
    >>= Pipe.filter_map ?max_queue_length:None
  ;;

  let map_update t =
    let open Tilde_f.Let_syntax in
    map_response t
    >>= Deferred.map
    >>= Tilde_f.of_local_k Or_error.map
    >>= Tilde_f.of_local_k Result.map
    >>= Tuple3.map_snd
    >>= Pipe_extended.map_batched
  ;;

  let map_error t =
    Tilde_f.Let_syntax.(
      map_response t
      >>= Deferred.map
      >>= Tilde_f.of_local_k Or_error.map
      >>= Tilde_f.of_local_k Result.map_error)
  ;;
end

module One_way = struct
  open Async_rpc_kernel

  let dispatch_multi = dispatch_multi_or_error

  let singleton rpc =
    Nonempty_list.singleton { dispatch = Rpc.One_way.dispatch rpc; rpc = One_way rpc }
  ;;

  let add = adder ~f:singleton
  let map_query = map_query
end

module One_way_exn = struct
  open Async_rpc_kernel

  let dispatch_multi = dispatch_multi_exn

  let singleton rpc =
    Nonempty_list.singleton { dispatch = Rpc.One_way.dispatch_exn rpc; rpc = One_way rpc }
  ;;

  let add = adder ~f:singleton
  let map_query = map_query
end

module One_way' = struct
  open Async_rpc_kernel

  let singleton rpc =
    Nonempty_list.singleton { dispatch = Rpc.One_way.dispatch' rpc; rpc = One_way rpc }
  ;;

  let add = adder ~f:singleton
  let map_query = map_query
end

module Streamable_plain_rpc = struct
  let dispatch_multi = dispatch_multi_or_error_deferred

  let singleton rpc =
    Nonempty_list.singleton
      { dispatch = Streamable.Plain_rpc.dispatch' rpc; rpc = Streamable_plain rpc }
  ;;

  let add = adder ~f:singleton
  let map_query = map_query

  let map_response t =
    Tilde_f.Let_syntax.(
      map_response t
      >>= Deferred.map
      >>= Tilde_f.of_local_k Or_error.map
      >>= Tilde_f.of_local_k Or_error.map)
  ;;
end

module Streamable_pipe_rpc = struct
  let dispatch_multi = dispatch_multi_or_error_deferred

  let singleton rpc =
    Nonempty_list.singleton
      { dispatch = Streamable.Pipe_rpc.dispatch' rpc; rpc = Streamable_pipe rpc }
  ;;

  let add = adder ~f:singleton
  let map_query = map_query

  let filter_map_response t =
    Tilde_f.Let_syntax.(
      map_response t
      >>= Deferred.map
      >>= Tilde_f.of_local_k Or_error.map
      >>= Tilde_f.of_local_k Or_error.map
      >>= Pipe.filter_map ?max_queue_length:None)
  ;;

  let map_response t =
    Tilde_f.Let_syntax.(
      map_response t
      >>= Deferred.map
      >>= Tilde_f.of_local_k Or_error.map
      >>= Tilde_f.of_local_k Or_error.map
      >>= Pipe_extended.map_batched)
  ;;
end

module Streamable_state_rpc = struct
  let dispatch_multi = dispatch_multi_or_error_deferred

  let singleton rpc =
    Nonempty_list.singleton
      { dispatch = Streamable.State_rpc.dispatch' rpc; rpc = Streamable_state rpc }
  ;;

  let add = adder ~f:singleton
  let map_query = map_query

  let map_state t =
    Tilde_f.Let_syntax.(
      map_response t
      >>= Deferred.map
      >>= Tilde_f.of_local_k Or_error.map
      >>= Tilde_f.of_local_k Or_error.map
      >>= Tuple2.map_fst)
  ;;

  let filter_map_update t =
    Tilde_f.Let_syntax.(
      map_response t
      >>= Deferred.map
      >>= Tilde_f.of_local_k Or_error.map
      >>= Tilde_f.of_local_k Or_error.map
      >>= Tuple2.map_snd
      >>= Pipe.filter_map ?max_queue_length:None)
  ;;

  let map_update t =
    Tilde_f.Let_syntax.(
      map_response t
      >>= Deferred.map
      >>= Tilde_f.of_local_k Or_error.map
      >>= Tilde_f.of_local_k Or_error.map
      >>= Tuple2.map_snd
      >>= Pipe_extended.map_batched)
  ;;
end

module Expert = struct
  let return rpc a =
    Nonempty_list.singleton
      { dispatch = (fun (_ : Async_rpc_kernel.Rpc.Connection.t) -> a); rpc }
  ;;
end
