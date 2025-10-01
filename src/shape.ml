open Core
open Async_rpc_kernel

module T = struct
  type t =
    | Rpc of
        { query : Bin_prot.Shape.Digest.t
        ; response : Bin_prot.Shape.Digest.t
        }
    | Pipe_rpc of
        { query : Bin_prot.Shape.Digest.t
        ; response : Bin_prot.Shape.Digest.t
        ; error : Bin_prot.Shape.Digest.t
        }
    | State_rpc of
        { query : Bin_prot.Shape.Digest.t
        ; state : Bin_prot.Shape.Digest.t
        ; update : Bin_prot.Shape.Digest.t
        ; error : Bin_prot.Shape.Digest.t
        }
    | One_way of { msg : Bin_prot.Shape.Digest.t }
    | Streamable_plain_rpc of
        { query : Bin_prot.Shape.Digest.t
        ; response : Bin_prot.Shape.Digest.t
        }
    | Streamable_pipe_rpc of
        { query : Bin_prot.Shape.Digest.t
        ; response : Bin_prot.Shape.Digest.t
        }
    | Streamable_state_rpc of
        { query : Bin_prot.Shape.Digest.t
        ; state : Bin_prot.Shape.Digest.t
        ; update : Bin_prot.Shape.Digest.t
        }
  [@@deriving compare ~localize, sexp_of]
end

include T
include Comparable.Make_plain (T)

let rpc rpc =
  Rpc
    { query = Bin_prot.Shape.eval_to_digest (Rpc.Rpc.bin_query rpc).shape
    ; response = Bin_prot.Shape.eval_to_digest (Rpc.Rpc.bin_response rpc).shape
    }
;;

let pipe_rpc rpc =
  Pipe_rpc
    { query = Bin_prot.Shape.eval_to_digest (Rpc.Pipe_rpc.bin_query rpc).shape
    ; response = Bin_prot.Shape.eval_to_digest (Rpc.Pipe_rpc.bin_response rpc).shape
    ; error = Bin_prot.Shape.eval_to_digest (Rpc.Pipe_rpc.bin_error rpc).shape
    }
;;

let state_rpc rpc =
  State_rpc
    { query = Bin_prot.Shape.eval_to_digest (Rpc.State_rpc.bin_query rpc).shape
    ; state = Bin_prot.Shape.eval_to_digest (Rpc.State_rpc.bin_state rpc).shape
    ; update = Bin_prot.Shape.eval_to_digest (Rpc.State_rpc.bin_update rpc).shape
    ; error = Bin_prot.Shape.eval_to_digest (Rpc.State_rpc.bin_error rpc).shape
    }
;;

let one_way rpc =
  One_way { msg = Bin_prot.Shape.eval_to_digest (Rpc.One_way.bin_msg rpc).shape }
;;

let streamable_plain_rpc rpc =
  Streamable_plain_rpc
    { query = Bin_prot.Shape.eval_to_digest (Streamable.Plain_rpc.bin_query_shape rpc)
    ; response =
        Bin_prot.Shape.eval_to_digest (Streamable.Plain_rpc.bin_response_shape rpc)
    }
;;

let streamable_pipe_rpc rpc =
  Streamable_pipe_rpc
    { query = Bin_prot.Shape.eval_to_digest (Streamable.Pipe_rpc.bin_query_shape rpc)
    ; response =
        Bin_prot.Shape.eval_to_digest (Streamable.Pipe_rpc.bin_response_shape rpc)
    }
;;

let streamable_state_rpc rpc =
  Streamable_state_rpc
    { query = Bin_prot.Shape.eval_to_digest (Streamable.State_rpc.bin_query_shape rpc)
    ; state = Bin_prot.Shape.eval_to_digest (Streamable.State_rpc.bin_state_shape rpc)
    ; update = Bin_prot.Shape.eval_to_digest (Streamable.State_rpc.bin_update_shape rpc)
    }
;;
