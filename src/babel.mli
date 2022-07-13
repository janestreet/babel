open Core
open Async_rpc_kernel

(** A library for helping to implement version-aware Async_rpc protocols. The main modules
    of interest are [Callee] and [Caller], for building implementations and dispatch
    functions, respectively. *)

module Callee = Callee
module Caller = Caller
module Generic_rpc = Generic_rpc
module Shape = Shape

(** Test that the given caller and callee are able to communicate, returning the rpc they
    will select if so. *)
val check_compatibility
  :  caller:_ Caller.t
  -> callee:_ Callee.t
  -> Rpc.Description.t Or_error.t

val check_compatibility_exn : caller:_ Caller.t -> callee:_ Callee.t -> Rpc.Description.t
