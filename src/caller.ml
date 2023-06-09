open Core
open Async_kernel
open Async_rpc_kernel
module Fn = Babel_fn

module Strategy = struct
  type 'a t =
    { dispatch : ?metadata:Rpc_metadata.t -> Rpc.Connection.t -> 'a
    ; rpc : Generic_rpc.t
    }
  [@@deriving fields]

  let map { dispatch; rpc } ~f =
    let dispatch ?metadata conn = f (dispatch ?metadata conn) in
    { dispatch; rpc }
  ;;

  let description t = Generic_rpc.description t.rpc
  let shape t = description t, Generic_rpc.shape t.rpc

  let in_menu t menu =
    let { Rpc.Description.name; version } = description t in
    Versioned_rpc.Menu.supported_versions menu ~rpc_name:name |> Fn.flip Set.mem version
  ;;
end

open Strategy

type 'a t = 'a Strategy.t list

let map t ~f = List.map t ~f:(fun s -> Strategy.map ~f s)
let shapes t = List.map t ~f:Strategy.shape
let supported_rpcs t = List.map t ~f:Strategy.rpc
let descriptions t = List.map (shapes t) ~f:fst
let print_shapes t = print_s [%sexp (shapes t : (Rpc.Description.t * Shape.t) list)]

let find_strategy t menu =
  match List.find t ~f:(fun strategy -> Strategy.in_menu strategy menu) with
  | Some strategy -> Ok strategy
  | None ->
    let callee_rpcs = Versioned_rpc.Menu.supported_rpcs menu in
    let caller_rpcs = descriptions t in
    Or_error.error_s
      [%message
        "Could not find any supported rpc in callee"
          (callee_rpcs : Rpc.Description.t list)
          (caller_rpcs : Rpc.Description.t list)]
;;

let to_dispatch_fun t menu = find_strategy t menu |> Or_error.map ~f:Strategy.dispatch
let description t menu = find_strategy t menu |> Or_error.map ~f:Strategy.description
let of_list_decreasing_preference = List.concat
let map_query t = Tilde_f.Let_syntax.(map t >>= Fn.map_input)
let map_response t = Tilde_f.Let_syntax.(map t >>= Fn.map)

let can_dispatch t connection_with_menu =
  let menu = Versioned_rpc.Connection_with_menu.menu connection_with_menu in
  find_strategy t menu |> Or_error.is_ok
;;

let dispatch_multi ?metadata t connection_with_menu query ~on_error =
  let menu = Versioned_rpc.Connection_with_menu.menu connection_with_menu in
  match to_dispatch_fun t menu with
  | Ok f ->
    f ?metadata (Versioned_rpc.Connection_with_menu.connection connection_with_menu) query
  | Error _ as error -> on_error error
;;

let dispatch_multi_or_error_deferred ?metadata t connection_with_menu query =
  dispatch_multi t ?metadata connection_with_menu query ~on_error:Deferred.return
;;

let dispatch_multi_or_error ?metadata t connection_with_menu query =
  dispatch_multi t ?metadata connection_with_menu query ~on_error:Fn.id
;;

let dispatch_multi_exn ?metadata t connection_with_menu query =
  dispatch_multi t ?metadata connection_with_menu query ~on_error:ok_exn
;;

let adder t ~rpc ~f = of_list_decreasing_preference [ f rpc; t ]

module Rpc = struct
  let dispatch_multi = dispatch_multi_or_error_deferred

  let singleton rpc =
    [ { dispatch = (fun ?metadata -> Rpc.Rpc.dispatch ?metadata rpc); rpc = Rpc rpc } ]
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
    [ { dispatch = (fun ?metadata -> Rpc.Rpc.dispatch' ?metadata rpc); rpc = Rpc rpc } ]
  ;;

  let add = adder ~f:singleton
  let map_query = map_query

  let map_response t =
    Tilde_f.Let_syntax.(map_response t >>= Deferred.map >>= Tilde_f.of_local_k Result.map)
  ;;
end

module Rpc_exn = struct
  open Async_rpc_kernel

  let dispatch_multi = dispatch_multi_exn

  let singleton rpc =
    [ { dispatch = (fun ?metadata -> Rpc.Rpc.dispatch_exn ?metadata rpc); rpc = Rpc rpc }
    ]
  ;;

  let add = adder ~f:singleton
  let map_query = map_query
  let map_response t = Tilde_f.Let_syntax.(map_response t >>= Deferred.map)
end

module Pipe_rpc = struct
  open Async_rpc_kernel

  let dispatch_multi = dispatch_multi_or_error_deferred

  let singleton rpc =
    [ { dispatch = (fun ?metadata -> Rpc.Pipe_rpc.dispatch ?metadata rpc)
      ; rpc = Pipe rpc
      }
    ]
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
    >>= Pipe.map
  ;;
end

module Pipe_rpc_exn = struct
  open Async_rpc_kernel

  let dispatch_multi = dispatch_multi_exn

  let singleton rpc =
    [ { dispatch = (fun ?metadata -> Rpc.Pipe_rpc.dispatch_exn ?metadata rpc)
      ; rpc = Pipe rpc
      }
    ]
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
    Tilde_f.Let_syntax.(map_response t >>= Deferred.map >>= Tuple2.map_fst >>= Pipe.map)
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
    let dispatch ?metadata connection query ~f =
      (let open Tilde_f.Let_syntax in
       Rpc.Pipe_rpc.dispatch_iter ?metadata rpc connection query ~f
       |> Deferred.map
       >>= Tilde_f.of_local_k Or_error.map
       >>= Tilde_f.of_local_k Result.map)
        ~f:(fun id -> Id.T { rpc; connection; id })
    in
    [ { dispatch; rpc = Pipe rpc } ]
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

  let singleton rpc =
    [ { dispatch = (fun ?metadata -> Rpc.State_rpc.dispatch ?metadata rpc)
      ; rpc = State rpc
      }
    ]
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
    >>= Pipe.map
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
    [ { dispatch = (fun ?metadata -> Rpc.One_way.dispatch ?metadata rpc)
      ; rpc = One_way rpc
      }
    ]
  ;;

  let add = adder ~f:singleton
  let map_query = map_query
end

module One_way_exn = struct
  open Async_rpc_kernel

  let dispatch_multi = dispatch_multi_exn

  let singleton rpc =
    [ { dispatch = (fun ?metadata -> Rpc.One_way.dispatch_exn ?metadata rpc)
      ; rpc = One_way rpc
      }
    ]
  ;;

  let add = adder ~f:singleton
  let map_query = map_query
end

module One_way' = struct
  open Async_rpc_kernel

  let singleton rpc =
    [ { dispatch = (fun ?metadata -> Rpc.One_way.dispatch' ?metadata rpc)
      ; rpc = One_way rpc
      }
    ]
  ;;

  let add = adder ~f:singleton
  let map_query = map_query
end

module Streamable_plain_rpc = struct
  let dispatch_multi = dispatch_multi_or_error_deferred

  let singleton rpc =
    [ { dispatch = (fun ?metadata -> Streamable.Plain_rpc.dispatch' ?metadata rpc)
      ; rpc = Streamable_plain rpc
      }
    ]
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
    [ { dispatch = (fun ?metadata -> Streamable.Pipe_rpc.dispatch' ?metadata rpc)
      ; rpc = Streamable_pipe rpc
      }
    ]
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
      >>= Pipe.map)
  ;;
end

module Streamable_state_rpc = struct
  let dispatch_multi = dispatch_multi_or_error_deferred

  let singleton rpc =
    [ { dispatch = (fun ?metadata -> Streamable.State_rpc.dispatch' ?metadata rpc)
      ; rpc = Streamable_state rpc
      }
    ]
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
      >>= Pipe.map)
  ;;
end
