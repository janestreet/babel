open! Core
open! Async_kernel
include module type of Pipe

val map_batched : 'a Reader.t -> f:('a -> 'b) -> 'b Reader.t
