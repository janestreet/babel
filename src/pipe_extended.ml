open! Core
open! Async_kernel
include Pipe

let map_batched t ~f =
  map
    ~max_batch_size:
      Async_rpc_kernel.Async_rpc_kernel_private.Transport.Writer
      .transfer_default_max_num_values_per_read
    t
    ~f
;;
