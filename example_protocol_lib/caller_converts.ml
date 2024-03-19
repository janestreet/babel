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
    ~name:"my-caller-converts-rpc"
    ~version
    ~bin_query
    ~bin_response
    ~include_in_error_count
;;

(* First we explicitly define each rpc that we intend for the client to implement. *)

let v1 = rpc 1 Stable.Query.V1.bin_t Stable.Response.V1.bin_t Only_on_exn
and v2 = rpc 2 Stable.Query.V2.bin_t Stable.Response.V1.bin_t Only_on_exn
and v3 = rpc 3 Stable.Query.V2.bin_t Stable.Response.V2.bin_t Only_on_exn

(* Now we define a [Babel.Caller.t]. This permits combining multiple callers with the same
   query and response types. However, since our rpcs work on different types, we use
   functions like [Babel.Caller.Rpc.map_query] and [Babel.Caller.Rpc.map_response] to
   translate between them.

   Unlike the definition of a callee, the ordering of the constituent callers is
   significant. When using [Babel.Caller.of_list_decreasing_preference], the first version
   will be the most preferred. [Babel.Caller.Rpc.add] effectively prepends to this list,
   meaning the last version added will be the one most preferred. *)
let caller =
  Babel.Caller.Rpc.singleton v1
  |> Babel.Caller.Rpc.map_query ~f:Stable.Query.V2.to_V1_t
  |> Babel.Caller.Rpc.add ~rpc:v2
  |> Babel.Caller.Rpc.map_response ~f:(Stable.Response.V2.of_V1_t ~y:0)
  |> Babel.Caller.Rpc.add ~rpc:v3
;;

(* The [Babel.Caller.print_shapes] function is useful for testing in one place that you
   are supporting all the rpcs you expect and that their bin_prot digests don't change.
   The ordering also reflect preference order; clients will prefer the versions appearing
   earlier over the ones appearing later. *)
let%expect_test _ =
  Babel.Caller.print_shapes caller;
  [%expect
    {|
    ((((name my-caller-converts-rpc) (version 3))
      (Rpc (query 94d3b785da460869144daff623f170df)
       (response fe8c6d5d25e0c5ee905d672ed01b4a45)))
     (((name my-caller-converts-rpc) (version 2))
      (Rpc (query 94d3b785da460869144daff623f170df)
       (response 0743bf7ccae7c4a9d44998836b0cb146)))
     (((name my-caller-converts-rpc) (version 1))
      (Rpc (query fa9bd13df9b004418afde2225f5c7927)
       (response 0743bf7ccae7c4a9d44998836b0cb146))))
    |}]
;;

(* [Babel.Caller.dispatch_multi] creates a dispatch function that knows how to select the
   most preferable rpc from a version menu and then use it. *)
let dispatch = Babel.Caller.Rpc.dispatch_multi caller

(* We can just use the normal [Rpc.Rpc.implement] function for the server side. *)
let implement f = Rpc.Rpc.implement v2 f

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
    type t = Stable.Response.V2.t =
      { y : int
      ; z : int
      }
    [@@deriving sexp_of]
  end
end

module Server = struct
  module Query = Query

  module Response = struct
    type t = Stable.Response.V1.t = { z : int } [@@deriving sexp_of]
  end
end
