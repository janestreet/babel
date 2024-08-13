open Core
open Async_rpc_kernel
module Callee = Callee
module Caller = Caller
module Generic_rpc = Generic_rpc
module Shape = Shape

let check_compatibility ~caller ~callee =
  let open Or_error.Let_syntax in
  let caller_shapes = Caller.shapes caller in
  let%bind callee_shapes = Callee.shapes callee in
  match
    Nonempty_list.find_map
      caller_shapes
      ~f:(fun ((caller_description, _shape) as caller) ->
        List.find_map callee_shapes ~f:(fun ((callee_description, _shape) as callee) ->
          if [%compare.equal: Rpc.Description.t] caller_description callee_description
          then Some (caller, callee)
          else None))
  with
  | None -> Or_error.error_string "Could not match any rpcs"
  | Some ((caller_description, caller_shape), (callee_description, callee_shape)) ->
    if Shape.equal caller_shape callee_shape
    then Ok caller_description
    else
      Or_error.error_s
        [%message
          "Corresponding rpcs have mismatching shapes"
            (caller_description : Rpc.Description.t)
            (callee_description : Rpc.Description.t)]
;;

let check_compatibility_exn ~caller ~callee = ok_exn (check_compatibility ~caller ~callee)
