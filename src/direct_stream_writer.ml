open Core
open Async_kernel
open Async_rpc_kernel
module Transformation_id = Unique_id.Int63 ()

type -'a t =
  | T :
      { writer : 'x Rpc.Pipe_rpc.Direct_stream_writer.t
      ; transform : 'a -> 'x option
      ; output_witness : 'x Type_equal.Id.t
      ; transformation_ids : Transformation_id.t list
      }
      -> 'a t

let create_witnessed writer ~witness =
  T { writer; transform = Option.some; output_witness = witness; transformation_ids = [] }
;;

let create writer =
  create_witnessed
    writer
    ~witness:
      (Type_equal.Id.create
         ~name:"[Babel.Direct_stream_writer] unique type witness"
         [%sexp_of: _])
;;

let map_input_with_id (T { writer; transform; output_witness; transformation_ids }) ~f ~id
  =
  T
    { writer
    ; transform = (fun a -> transform (f a))
    ; output_witness
    ; transformation_ids = id :: transformation_ids
    }
;;

let filter_map_input_with_id
      (T { writer; transform; output_witness; transformation_ids })
      ~f
      ~id
  =
  T
    { writer
    ; transform = (fun a -> Option.bind (f a) ~f:transform)
    ; output_witness
    ; transformation_ids = id :: transformation_ids
    }
;;

let map_input t ~f = map_input_with_id t ~f ~id:(Transformation_id.create ())

let filter_map_input t ~f =
  filter_map_input_with_id t ~f ~id:(Transformation_id.create ())
;;

let close (T { writer; _ }) = Rpc.Pipe_rpc.Direct_stream_writer.close writer
let closed (T { writer; _ }) = Rpc.Pipe_rpc.Direct_stream_writer.closed writer
let flushed (T { writer; _ }) = Rpc.Pipe_rpc.Direct_stream_writer.flushed writer
let is_closed (T { writer; _ }) = Rpc.Pipe_rpc.Direct_stream_writer.is_closed writer

let write_assuming_is_open (T { writer; transform; _ }) a =
  match transform a with
  | None -> `Flushed Deferred.unit
  | Some a -> Rpc.Pipe_rpc.Direct_stream_writer.write writer a
;;

let write_without_pushback_assuming_is_open (T { writer; transform; _ }) a =
  match transform a with
  | None -> `Ok
  | Some a -> Rpc.Pipe_rpc.Direct_stream_writer.write_without_pushback writer a
;;

let write t a = if is_closed t then `Closed else write_assuming_is_open t a

let write_without_pushback t a =
  if is_closed t then `Closed else write_without_pushback_assuming_is_open t a
;;

module Group = struct
  type 'a writer = 'a t

  module Subgroup = struct
    type 'a t =
      | T :
          { group : 'x Rpc.Pipe_rpc.Direct_stream_writer.Group.t
          ; transform : 'a -> 'x option
          ; output_witness : 'x Type_equal.Id.t
          ; transformation_ids : Transformation_id.t list
          }
          -> 'a t

    let of_writer_exn
          (T { writer; transform; output_witness; transformation_ids } : _ writer)
      =
      let group = Rpc.Pipe_rpc.Direct_stream_writer.Group.create () in
      Rpc.Pipe_rpc.Direct_stream_writer.Group.add_exn group writer;
      T { group; transform; output_witness; transformation_ids }
    ;;

    let compatible
          (T { output_witness; transformation_ids; _ })
          (T
             { output_witness = writer_output_witness
             ; transformation_ids = writer_transformation_ids
             ; _
             } :
             _ writer)
      =
      Type_equal.Id.same output_witness writer_output_witness
      && [%compare.equal: Transformation_id.t list]
           transformation_ids
           writer_transformation_ids
    ;;

    let add_exn
          (T { group; output_witness; _ })
          (T { writer; output_witness = writer_output_witness; _ } : _ writer)
      =
      let equality =
        Type_equal.Id.same_witness_exn output_witness writer_output_witness
      in
      let T = equality in
      Rpc.Pipe_rpc.Direct_stream_writer.Group.add_exn group writer
    ;;

    let write_without_pushback (T { group; transform; _ }) a =
      match transform a with
      | None -> ()
      | Some a -> Rpc.Pipe_rpc.Direct_stream_writer.Group.write_without_pushback group a
    ;;

    let write (T { group; transform; _ }) a =
      match transform a with
      | None -> return ()
      | Some a -> Rpc.Pipe_rpc.Direct_stream_writer.Group.write group a
    ;;
  end

  type 'a t = 'a Subgroup.t Bag.t

  let create () = Bag.create ()

  let add_exn t writer =
    match Bag.find t ~f:(Fn.flip Subgroup.compatible writer) with
    | Some subgroup -> Subgroup.add_exn subgroup writer
    | None ->
      let subgroup = Subgroup.of_writer_exn writer in
      Bag.add_unit t subgroup
  ;;

  let write_without_pushback t a =
    Bag.iter t ~f:(Fn.flip Subgroup.write_without_pushback a)
  ;;

  let write t a =
    Bag.to_list t |> List.map ~f:(Fn.flip Subgroup.write a) |> Deferred.all_unit
  ;;

  let length t =
    Bag.sum
      (module Int)
      t
      ~f:(fun (Subgroup.T { group; _ }) ->
        Rpc.Pipe_rpc.Direct_stream_writer.Group.length group)
  ;;

  module For_testing = struct
    let num_subgroups = Bag.length
  end
end

module Expert = struct
  module Transformation_id = Transformation_id

  let create_witnessed = create_witnessed
  let map_input_with_id = map_input_with_id
  let filter_map_input_with_id = filter_map_input_with_id
end
