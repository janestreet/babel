module Stable = struct
  open Core.Core_stable

  module Query = struct
    module V1 = Unit.V1
  end

  module Response = struct
    module V1 = struct
      type t = int [@@deriving bin_io, compare, sexp_of]
    end

    module V2 = struct
      type t =
        { a : int
        ; b : int
        }
      [@@deriving bin_io, compare, fields ~getters, sexp_of]
    end
  end
end

open! Core
open! Async
open Async_rpc_kernel
module Direct_stream_writer = Babel.Callee.Pipe_rpc_direct.Direct_stream_writer
module Response = Stable.Response.V2

let make_rpc ~name ~version ~bin_response =
  Rpc.Pipe_rpc.create
    ~name
    ~version
    ~bin_query:[%bin_type_class: Stable.Query.V1.t]
    ~bin_response
    ~bin_error:[%bin_type_class: Nothing.t]
    ()
;;

let make_v1 = make_rpc ~version:1 ~bin_response:[%bin_type_class: Stable.Response.V1.t]
let make_v2 = make_rpc ~version:2 ~bin_response:[%bin_type_class: Stable.Response.V2.t]
let name = "test-rpc"
let v1 = make_v1 ~name
let v2 = make_v2 ~name
let alt_name = "test-rpc-alt"
let v1_alt = make_v1 ~name:alt_name
let v2_alt = make_v2 ~name:alt_name

let get_client implementations =
  let implementations =
    Rpc.Implementations.create_exn
      ~implementations
      ~on_unknown_rpc:`Raise
      ~on_exception:Log_on_background_exn
  in
  let r1, w1 = Pipe.create () in
  let r2, w2 = Pipe.create () in
  let t1 = Pipe_transport.create Pipe_transport.Kind.string r1 w2 in
  let t2 = Pipe_transport.create Pipe_transport.Kind.string r2 w1 in
  let%map (_server : Rpc.Connection.t) =
    Rpc.Connection.create ~implementations ~connection_state:(const ()) t1
    >>| Result.ok_exn
  and client = Rpc.Connection.create ~connection_state:(const ()) t2 >>| Result.ok_exn in
  client
;;

let connect_and_dispatch implementations rpc =
  let%bind client = get_client implementations in
  Rpc.Pipe_rpc.dispatch_exn rpc client () >>| fst
;;

let make_implementations group callee =
  Babel.Callee.implement_multi_exn callee ~f:(fun () (_ : Rpc.Description.t) () writer ->
    Direct_stream_writer.Group.add_exn group writer;
    return (Ok ()))
;;

let%expect_test "Conversion functions only get run once per item" =
  let conversions = ref 0 in
  let callee =
    Babel.Callee.Pipe_rpc_direct.singleton v1
    |> Babel.Callee.Pipe_rpc_direct.map_response ~f:(fun v2 ->
      conversions := !conversions + 1;
      Stable.Response.V2.a v2)
    |> Babel.Callee.Pipe_rpc_direct.add ~rpc:v2
  in
  let group = Direct_stream_writer.Group.create () in
  let implementations = make_implementations group callee in
  let dispatch rpc = connect_and_dispatch implementations rpc in
  let subgroups () = Direct_stream_writer.Group.For_testing.num_subgroups group in
  [%test_result: int] (subgroups ()) ~expect:0;
  let%bind a1 = dispatch v1 in
  [%test_result: int] (subgroups ()) ~expect:1;
  let%bind b1 = dispatch v1 in
  [%test_result: int] (subgroups ()) ~expect:1;
  let%bind a2 = dispatch v2 in
  [%test_result: int] (subgroups ()) ~expect:2;
  let value : Response.t = { a = 5; b = 1 } in
  Direct_stream_writer.Group.write_without_pushback group value;
  let%bind () = Pipe.read_exn a1 >>| [%test_result: int] ~expect:5 in
  let%bind () = Pipe.read_exn b1 >>| [%test_result: int] ~expect:5 in
  let%bind () = Pipe.read_exn a2 >>| [%test_result: Response.t] ~expect:value in
  [%test_result: int] !conversions ~expect:1;
  return ()
;;

let%expect_test "One group can contain multiple rpcs with the same types but different \
                 conversions"
  =
  let callee =
    Babel.Callee.of_list
      [ Babel.Callee.Pipe_rpc_direct.singleton v1
        |> Babel.Callee.Pipe_rpc_direct.map_response ~f:Stable.Response.V2.a
        |> Babel.Callee.Pipe_rpc_direct.add ~rpc:v2
      ; Babel.Callee.Pipe_rpc_direct.singleton v1_alt
        |> Babel.Callee.Pipe_rpc_direct.map_response ~f:Stable.Response.V2.b
        |> Babel.Callee.Pipe_rpc_direct.add ~rpc:v2_alt
      ]
  in
  let group = Direct_stream_writer.Group.create () in
  let implementations = make_implementations group callee in
  let dispatch rpc = connect_and_dispatch implementations rpc in
  let subgroups () = Direct_stream_writer.Group.For_testing.num_subgroups group in
  let%bind a1 = dispatch v1 in
  let%bind a2 = dispatch v2 in
  let%bind b1 = dispatch v1_alt in
  let%bind b2 = dispatch v2_alt in
  [%test_result: int] (subgroups ()) ~expect:4;
  let value : Response.t = { a = 5; b = 1 } in
  Direct_stream_writer.Group.write_without_pushback group value;
  let%bind () = Pipe.read_exn a1 >>| [%test_result: int] ~expect:5 in
  let%bind () = Pipe.read_exn b1 >>| [%test_result: int] ~expect:1 in
  let%bind () = Pipe.read_exn a2 >>| [%test_result: Response.t] ~expect:value in
  let%bind () = Pipe.read_exn b2 >>| [%test_result: Response.t] ~expect:value in
  return ()
;;

let%expect_test "transformation ids only need to uniquely represent functions, not \
                 entire chains"
  =
  let callee = Babel.Callee.Pipe_rpc_direct.singleton v1 in
  let dsws = ref Reversed_list.[] in
  let implementations =
    Babel.Callee.implement_multi_exn
      callee
      ~f:(fun () (_ : Rpc.Description.t) () writer ->
        dsws := writer :: !dsws;
        return (Ok ()))
  in
  let dispatch rpc = connect_and_dispatch implementations rpc in
  let%bind a1 = dispatch v1 in
  let%bind a2 = dispatch v1 in
  let dsws = Reversed_list.rev !dsws in
  let distinct_transform i writer =
    Direct_stream_writer.map_input writer ~f:(fun n -> i + n)
  in
  let same_transform =
    let id = Direct_stream_writer.Expert.Transformation_id.create () in
    fun writer ->
      Direct_stream_writer.Expert.map_input_with_id writer ~f:(fun n -> n * 2) ~id
  in
  let group = Direct_stream_writer.Group.create () in
  List.mapi dsws ~f:distinct_transform
  |> List.map ~f:same_transform
  |> List.iter ~f:(Direct_stream_writer.Group.add_exn group);
  let subgroups () = Direct_stream_writer.Group.For_testing.num_subgroups group in
  [%test_result: int] (subgroups ()) ~expect:2;
  Direct_stream_writer.Group.write_without_pushback group 3;
  let%bind () = Pipe.read_exn a1 >>| [%test_result: int] ~expect:6 in
  let%bind () = Pipe.read_exn a2 >>| [%test_result: int] ~expect:7 in
  return ()
;;

let%expect_test "[store_last_value_and_send_on_add] works for both existing and new \
                 subgroups, without extra conversions"
  =
  let conversions = ref 0 in
  let callee =
    Babel.Callee.Pipe_rpc_direct.singleton v1
    |> Babel.Callee.Pipe_rpc_direct.map_response ~f:(fun v2 ->
      conversions := !conversions + 1;
      Stable.Response.V2.a v2)
    |> Babel.Callee.Pipe_rpc_direct.add ~rpc:v2
  in
  let group =
    Direct_stream_writer.Group.create_storing_last_value_and_sending_on_add ()
  in
  let implementations = make_implementations group callee in
  let dispatch rpc = connect_and_dispatch implementations rpc in
  let%bind a1 = dispatch v1 in
  let value : Response.t = { a = 5; b = 1 } in
  Direct_stream_writer.Group.write_without_pushback group value;
  let%bind () = Pipe.read_exn a1 >>| [%test_result: int] ~expect:5 in
  let%bind b1 = dispatch v1 in
  let%bind () = Pipe.read_exn b1 >>| [%test_result: int] ~expect:5 in
  let%bind a2 = dispatch v2 in
  let%bind () = Pipe.read_exn a2 >>| [%test_result: Response.t] ~expect:value in
  let%bind b2 = dispatch v2 in
  let%bind () = Pipe.read_exn b2 >>| [%test_result: Response.t] ~expect:value in
  [%test_result: int] !conversions ~expect:1;
  return ()
;;

let%expect_test "[store_last_value_and_send_on_add] will save values even when there are \
                 no writers"
  =
  let callee =
    Babel.Callee.Pipe_rpc_direct.singleton v1
    |> Babel.Callee.Pipe_rpc_direct.map_response ~f:(fun { Response.a; b = _ } -> a)
    |> Babel.Callee.Pipe_rpc_direct.add ~rpc:v2
  in
  let group =
    Direct_stream_writer.Group.create_storing_last_value_and_sending_on_add ()
  in
  let implementations = make_implementations group callee in
  let dispatch rpc = connect_and_dispatch implementations rpc in
  let%bind () = Direct_stream_writer.Group.write group { a = 1; b = 1 } in
  let%bind v1_1 = dispatch v1 in
  let%bind () = Pipe.read_exn v1_1 >>| [%test_result: int] ~expect:1 in
  let%bind () = Direct_stream_writer.Group.write group { a = 2; b = 1 } in
  let%bind () = Pipe.read_exn v1_1 >>| [%test_result: int] ~expect:2 in
  Pipe.close_read v1_1;
  let%bind () = Pipe.closed v1_1 in
  let%bind () =
    Deferred.repeat_until_finished () (fun () ->
      match Direct_stream_writer.Group.length group with
      | 0 -> return (`Finished ())
      | _ ->
        let%map () = Scheduler.yield_until_no_jobs_remain () in
        `Repeat ())
  in
  [%test_result: int] (Direct_stream_writer.Group.length group) ~expect:0;
  let%bind () = Direct_stream_writer.Group.write group { a = 3; b = 1 } in
  let%bind v1_2 = dispatch v1 in
  let%bind () = Pipe.read_exn v1_2 >>| [%test_result: int] ~expect:3 in
  let%bind v2_1 = dispatch v2 in
  let%bind () =
    Pipe.read_exn v2_1 >>| [%test_result: Response.t] ~expect:{ a = 3; b = 1 }
  in
  let%bind () = Direct_stream_writer.Group.write group { a = 4; b = 1 } in
  let%bind () = Pipe.read_exn v1_2 >>| [%test_result: int] ~expect:4 in
  let%bind () =
    Pipe.read_exn v2_1 >>| [%test_result: Response.t] ~expect:{ a = 4; b = 1 }
  in
  return ()
;;

let%expect_test "[flushed_or_closed] completes when all writers are flushed" =
  let callee =
    Babel.Callee.Pipe_rpc_direct.singleton v1
    |> Babel.Callee.Pipe_rpc_direct.map_response ~f:Stable.Response.V2.a
    |> Babel.Callee.Pipe_rpc_direct.add ~rpc:v2
  in
  let group = Direct_stream_writer.Group.create () in
  let implementations = make_implementations group callee in
  let dispatch rpc = connect_and_dispatch implementations rpc in
  let%bind a1 = dispatch v1 in
  let%bind a2 = dispatch v2 in
  Direct_stream_writer.Group.write_without_pushback group { a = 5; b = 1 };
  let flushed = Direct_stream_writer.Group.flushed_or_closed group in
  let%bind () = Pipe.read_exn a1 >>| [%test_result: int] ~expect:5 in
  let%bind () =
    Pipe.read_exn a2 >>| [%test_result: Response.t] ~expect:{ a = 5; b = 1 }
  in
  let%bind () = flushed in
  return ()
;;

let%expect_test "[flushed_or_closed] completes when writers are closed" =
  let callee = Babel.Callee.Pipe_rpc_direct.singleton v1 in
  let group = Direct_stream_writer.Group.create () in
  let implementations = make_implementations group callee in
  let dispatch rpc = connect_and_dispatch implementations rpc in
  let%bind a1 = dispatch v1 in
  let%bind a2 = dispatch v1 in
  Direct_stream_writer.Group.write_without_pushback group 10;
  let flushed = Direct_stream_writer.Group.flushed_or_closed group in
  Pipe.close_read a1;
  Pipe.close_read a2;
  let%bind () = flushed in
  return ()
;;

let%expect_test "[flushed_or_closed] completes immediately for empty group" =
  let group = Direct_stream_writer.Group.create () in
  [%test_result: int] (Direct_stream_writer.Group.length group) ~expect:0;
  let%bind () = Direct_stream_writer.Group.flushed_or_closed group in
  return ()
;;

let%expect_test "[flushed_or_closed] waits for all subgroups to be flushed" =
  let callee =
    Babel.Callee.of_list
      [ Babel.Callee.Pipe_rpc_direct.singleton v1
        |> Babel.Callee.Pipe_rpc_direct.map_response ~f:Stable.Response.V2.a
        |> Babel.Callee.Pipe_rpc_direct.add ~rpc:v2
      ; Babel.Callee.Pipe_rpc_direct.singleton v1_alt
        |> Babel.Callee.Pipe_rpc_direct.map_response ~f:Stable.Response.V2.b
        |> Babel.Callee.Pipe_rpc_direct.add ~rpc:v2_alt
      ]
  in
  let group = Direct_stream_writer.Group.create () in
  let implementations = make_implementations group callee in
  let dispatch rpc = connect_and_dispatch implementations rpc in
  let subgroups () = Direct_stream_writer.Group.For_testing.num_subgroups group in
  let%bind a1 = dispatch v1 in
  let%bind a2 = dispatch v2 in
  let%bind b1 = dispatch v1_alt in
  let%bind b2 = dispatch v2_alt in
  [%test_result: int] (subgroups ()) ~expect:4;
  let value : Response.t = { a = 5; b = 1 } in
  Direct_stream_writer.Group.write_without_pushback group value;
  let flushed = Direct_stream_writer.Group.flushed_or_closed group in
  let%bind () = Pipe.read_exn a1 >>| [%test_result: int] ~expect:5 in
  let%bind () = Pipe.read_exn b1 >>| [%test_result: int] ~expect:1 in
  let%bind () = Pipe.read_exn a2 >>| [%test_result: Response.t] ~expect:value in
  let%bind () = Pipe.read_exn b2 >>| [%test_result: Response.t] ~expect:value in
  let%bind () = flushed in
  return ()
;;
