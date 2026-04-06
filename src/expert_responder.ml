open! Core
open Async_rpc_kernel

type -'response t = Rpc.Rpc.Expert.Responder.t

let schedule t buf ~pos ~len = Rpc.Rpc.Expert.Responder.schedule t buf ~pos ~len

let write_bigstring t buf ~pos ~len =
  Rpc.Rpc.Expert.Responder.write_bigstring t buf ~pos ~len
;;

let write_bin_prot t writer response =
  Rpc.Rpc.Expert.Responder.write_bin_prot t writer response
;;

let write_error t error = Rpc.Rpc.Expert.Responder.write_error t error

module Private = struct
  let create responder = responder
end
