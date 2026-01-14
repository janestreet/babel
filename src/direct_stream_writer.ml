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

let started (T { writer; _ }) = Rpc.Pipe_rpc.Direct_stream_writer.started writer
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
      ~common_buffer
      ~send_last_value_on_add
      =
      let group =
        if send_last_value_on_add
        then Rpc.Pipe_rpc.Direct_stream_writer.Group.create_sending_last_value_on_add ()
        else Rpc.Pipe_rpc.Direct_stream_writer.Group.create ?buffer:common_buffer ()
      in
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

    let remove
      (T { group; output_witness; _ } as group')
      (T { writer; output_witness = writer_output_witness; _ } as writer' : _ writer)
      =
      if compatible group' writer'
      then (
        let equality =
          Type_equal.Id.same_witness_exn output_witness writer_output_witness
        in
        let T = equality in
        Rpc.Pipe_rpc.Direct_stream_writer.Group.remove group writer)
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

    let flushed_or_closed (T { group; _ }) =
      Rpc.Pipe_rpc.Direct_stream_writer.Group.flushed_or_closed group
    ;;
  end

  module Last_value : sig
    type 'a t

    val create : unit -> _ t
    val get : 'a t -> 'a option
    val set : 'a t -> 'a -> unit
  end = struct
    type 'a t = 'a ref Set_once.t

    let create = Set_once.create
    let get t = Option.map (Set_once.get t) ~f:( ! )

    let set t a =
      if Set_once.is_none t then Set_once.set_exn t (ref a) else Set_once.get_exn t := a
    ;;
  end

  module Last_value_if_should_send_on_add = struct
    type 'a t =
      | Do_not_store
      | Store_and_send_on_add of 'a Last_value.t

    let get_last_value = function
      | Do_not_store -> None
      | Store_and_send_on_add last_value -> Last_value.get last_value
    ;;

    let set_last_value t a =
      match t with
      | Do_not_store -> ()
      | Store_and_send_on_add last_value -> Last_value.set last_value a
    ;;
  end

  type 'a t =
    { subgroups : 'a Subgroup.t Bag.t
    ; last_value : 'a Last_value_if_should_send_on_add.t
    ; common_buffer : Rpc.Pipe_rpc.Direct_stream_writer.Group.Buffer.t option
    }

  let aux_create ~common_buffer ~store_last_value_and_send_on_add =
    { subgroups = Bag.create ()
    ; last_value =
        (if store_last_value_and_send_on_add
         then Store_and_send_on_add (Last_value.create ())
         else Do_not_store)
    ; common_buffer
    }
  ;;

  let create ?buffer () =
    aux_create ~common_buffer:buffer ~store_last_value_and_send_on_add:false
  ;;

  let create_storing_last_value_and_sending_on_add () =
    aux_create ~common_buffer:None ~store_last_value_and_send_on_add:true
  ;;

  let add_exn { subgroups; last_value; common_buffer } writer =
    match Bag.find subgroups ~f:(Fn.flip Subgroup.compatible writer) with
    | Some subgroup -> Subgroup.add_exn subgroup writer
    | None ->
      let send_last_value_on_add =
        match last_value with
        | Do_not_store -> false
        | Store_and_send_on_add (_ : _ Last_value.t) -> true
      in
      let subgroup =
        Subgroup.of_writer_exn ~common_buffer writer ~send_last_value_on_add
      in
      Bag.add_unit subgroups subgroup;
      Option.iter
        (Last_value_if_should_send_on_add.get_last_value last_value)
        ~f:(Subgroup.write_without_pushback subgroup)
  ;;

  let remove { subgroups; _ } writer =
    Bag.iter subgroups ~f:(fun subgroup -> Subgroup.remove subgroup writer)
  ;;

  let write_without_pushback { subgroups; last_value; _ } a =
    Bag.iter subgroups ~f:(Fn.flip Subgroup.write_without_pushback a);
    Last_value_if_should_send_on_add.set_last_value last_value a
  ;;

  let write { subgroups; last_value; _ } a =
    let all_written =
      Bag.to_list subgroups |> List.map ~f:(Fn.flip Subgroup.write a) |> Deferred.all_unit
    in
    Last_value_if_should_send_on_add.set_last_value last_value a;
    all_written
  ;;

  let flushed_or_closed { subgroups; _ } =
    Bag.to_list subgroups |> List.map ~f:Subgroup.flushed_or_closed |> Deferred.all_unit
  ;;

  let length t =
    Bag.sum (module Int) t.subgroups ~f:(fun (Subgroup.T { group; _ }) ->
      Rpc.Pipe_rpc.Direct_stream_writer.Group.length group)
  ;;

  module For_testing = struct
    let num_subgroups t = Bag.length t.subgroups
  end
end

module Expert = struct
  module Transformation_id = Transformation_id

  let create_witnessed = create_witnessed
  let map_input_with_id = map_input_with_id
  let filter_map_input_with_id = filter_map_input_with_id

  let write_without_pushback (T { writer; _ }) ~buf ~pos ~len =
    Rpc.Pipe_rpc.Direct_stream_writer.Expert.write_without_pushback writer ~buf ~pos ~len
  ;;

  let schedule_write (T { writer; _ }) ~buf ~pos ~len = exclave_
    Rpc.Pipe_rpc.Direct_stream_writer.Expert.schedule_write writer ~buf ~pos ~len
  ;;
end
