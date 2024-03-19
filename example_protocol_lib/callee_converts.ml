(* There is no need to try to preserve type equality between stable types. We will include
   any necessary conversion logic when we define the versioned protocol further below. *)
module Stable = struct
  open! Core.Core_stable

  module Query = struct
    module V1 = struct
      type t = { a : int } [@@deriving bin_io, compare, sexp]
    end

    module V2 = struct
      type t =
        { a : int
        ; b : int
        }
      [@@deriving bin_io, compare, sexp, stable_record ~version:V1.t ~remove:[ b ]]
    end
  end

  module Response = struct
    module V1 = struct
      type t = { z : int } [@@deriving bin_io, compare, sexp]
    end

    module V2 = struct
      type t =
        { y : int
        ; z : int
        }
      [@@deriving bin_io, compare, sexp, stable_record ~version:V1.t ~remove:[ y ]]
    end
  end
end

open! Core
open! Async_rpc_kernel

let rpc version bin_query bin_response include_in_error_count =
  Rpc.Rpc.create
    ~name:"my-callee-converts-rpc"
    ~version
    ~bin_query
    ~bin_response
    ~include_in_error_count
;;

(* First we explicitly define each rpc that we intend for the server to implement. *)

let v1 = rpc 1 Stable.Query.V1.bin_t Stable.Response.V1.bin_t Only_on_exn
and v2 = rpc 2 Stable.Query.V2.bin_t Stable.Response.V1.bin_t Only_on_exn
and v3 = rpc 3 Stable.Query.V2.bin_t Stable.Response.V2.bin_t Only_on_exn

(* Now we define a [Babel.Callee.t]. This permits combining multiple callees with the same
   query and response types. However, since our rpcs work on different types, we use
   functions like [Babel.Callee.Rpc.map_query] and [Babel.Callee.Rpc.map_response] to
   translate between them. *)
let callee =
  Babel.Callee.Rpc.singleton v1
  |> Babel.Callee.Rpc.map_query ~f:(Stable.Query.V2.of_V1_t ~b:0)
  |> Babel.Callee.Rpc.add ~rpc:v2
  |> Babel.Callee.Rpc.map_response ~f:Stable.Response.V2.to_V1_t
  |> Babel.Callee.Rpc.add ~rpc:v3
;;

(* The [Babel.Callee.print_shapes] function is useful for testing in one place that you
   are supporting all the rpcs you expect and that their bin_prot digests don't change. *)
let%expect_test _ =
  Babel.Callee.print_shapes callee;
  [%expect
    {|
    (Ok
     ((my-callee-converts-rpc
       ((1
         (Rpc (query fa9bd13df9b004418afde2225f5c7927)
          (response 0743bf7ccae7c4a9d44998836b0cb146)))
        (2
         (Rpc (query 94d3b785da460869144daff623f170df)
          (response 0743bf7ccae7c4a9d44998836b0cb146)))
        (3
         (Rpc (query 94d3b785da460869144daff623f170df)
          (response fe8c6d5d25e0c5ee905d672ed01b4a45)))))))
    |}]
;;

(* We can just use the normal [Rpc.Rpc.dispatch] function for the client side. *)
let dispatch = Rpc.Rpc.dispatch v2

(* [Babel.Callee.implement_multi_exn] generates all the rpc implementations that needs to
   be supported by the server from a single implementation function. It raises if any rpc
   name and version pair is duplicated in the callee. *)
let implement f = Babel.Callee.implement_multi_exn callee ~f

(* Most of the below is just structuring the modules as we want them exposed in the
   signature. *)

module Query = struct
  type t = Stable.Query.V2.t =
    { a : int
    ; b : int
    }
  [@@deriving sexp_of]
end

module Client = struct
  module Query = Query

  module Response = struct
    type t = Stable.Response.V1.t = { z : int } [@@deriving sexp_of]
  end
end

module Server = struct
  module Query = Query

  module Response = struct
    type t = Stable.Response.V2.t =
      { y : int
      ; z : int
      }
    [@@deriving sexp_of]
  end
end
