open! Core
open! Async_kernel
include Pipe

let map_batched t ~f =
  let max_queue_length =
    (* Async_rpc currently only writes at 1_000 elements to the transport between flushes
       anyway (see [Rpc_transport.Writer.transfer]), so there's no point doing larger
       batches here. *)
    1_000
  in
  map' ~max_queue_length t ~f:(fun q -> Queue.map q ~f |> Deferred.return)
;;
