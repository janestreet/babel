module Stable = struct
  open Core.Core_stable

  module Query = struct
    module V1 = struct
      type t =
        { waffles : string
        ; pancakes : int
        }
      [@@deriving bin_io, sexp, compare]
    end

    module V2 = struct
      type t =
        { waffles : string
        ; pancakes : int
        ; crepes : bool
        }
      [@@deriving bin_io, compare, sexp, stable_record ~version:V1.t ~remove:[ crepes ]]
    end

    module Bad_v2 = struct
      type t =
        { waffles : string
        ; crepes : bool
        ; pancakes : int
        }
      [@@deriving bin_io]
    end
  end

  module Response = struct
    module V1 = struct
      type t =
        | Already_exists
        | Created
      [@@deriving bin_io, sexp, compare]

      include
        Streamable.Remove_t
          (Streamable.Of_streamable
             (Streamable.Of_variant2
                (Streamable.Of_atomic (Unit.V1)) (Streamable.Of_atomic (Unit.V1)))
                (struct
                  type nonrec t = t

                  let to_streamable = function
                    | Already_exists -> `A ()
                    | Created -> `B ()
                  ;;

                  let of_streamable = function
                    | `A () -> Already_exists
                    | `B () -> Created
                  ;;
                end))
    end
  end
end

module type Q = sig
  type t [@@deriving bin_io]
end

module type R = sig
  type t [@@deriving bin_io]

  include Streamable.S with type t := t
end

open Core
open Async_kernel
open Async_rpc_kernel

module Setup (X : sig
    type ('q, 'r) rpc

    val create_rpc
      :  (module Q with type t = 'q)
      -> (module R with type t = 'r)
      -> name:string
      -> version:int
      -> ('q, 'r) rpc

    module Callee : sig
      module Monad : Monad.S

      val singleton : ('q, 'r) rpc -> ('q -> 'r Monad.t) Babel.Callee.t

      val add
        :  ('q -> 'r Monad.t) Babel.Callee.t
        -> rpc:('q, 'r) rpc
        -> ('q -> 'r Monad.t) Babel.Callee.t

      val map_query
        :  ('q1 -> 'r Monad.t) Babel.Callee.t
        -> f:('q1 -> 'q2)
        -> ('q2 -> 'r Monad.t) Babel.Callee.t
    end

    module Caller : sig
      module Monad : Monad.S

      val singleton : ('q, 'r) rpc -> ('q -> 'r Monad.t) Babel.Caller.t

      val add
        :  ('q -> 'r Monad.t) Babel.Caller.t
        -> rpc:('q, 'r) rpc
        -> ('q -> 'r Monad.t) Babel.Caller.t

      val map_query
        :  ('q1 -> 'r Monad.t) Babel.Caller.t
        -> f:('q2 -> 'q1)
        -> ('q2 -> 'r Monad.t) Babel.Caller.t
    end
  end) : sig
  val print_callee_shapes : unit -> unit
  val print_caller_shapes : unit -> unit
  val test_callee_converts : unit -> unit
  val test_caller_converts : unit -> unit
  val test_both_convert : unit -> unit
end = struct
  let name = "create-breakfast-buffet"

  let v1 =
    X.create_rpc (module Stable.Query.V1) (module Stable.Response.V1) ~name ~version:1
  ;;

  let v2 =
    X.create_rpc (module Stable.Query.V2) (module Stable.Response.V1) ~name ~version:2
  ;;

  let bad_v2 =
    X.create_rpc (module Stable.Query.Bad_v2) (module Stable.Response.V1) ~name ~version:2
  ;;

  let callees =
    let open List.Let_syntax in
    let maybe_add rpc = [ Fn.id; X.Callee.add ~rpc ] in
    let%map add_v1 = maybe_add v1
    and add_v2 = maybe_add v2 in
    Babel.Callee.of_list []
    |> add_v1
    |> X.Callee.map_query ~f:(Stable.Query.V2.of_V1_t ~crepes:false)
    |> add_v2
  ;;

  let callers =
    [ X.Caller.singleton v2
    ; X.Caller.singleton v1 |> X.Caller.map_query ~f:Stable.Query.V2.to_V1_t
    ; X.Caller.singleton v1
      |> X.Caller.map_query ~f:Stable.Query.V2.to_V1_t
      |> X.Caller.add ~rpc:v2
    ]
  ;;

  let banner = "-------------------------------------------------------------"

  let print_callee_shapes () =
    List.iter callees ~f:(fun callee ->
      print_endline banner;
      Babel.Callee.print_shapes callee)
  ;;

  let print_caller_shapes () =
    List.iter callers ~f:(fun caller ->
      print_endline banner;
      Babel.Caller.print_shapes caller)
  ;;

  let test_callee_converts () =
    List.iter callees ~f:(fun callee ->
      let callee_rpcs = Babel.Callee.descriptions callee in
      let caller x =
        Babel.check_compatibility ~callee ~caller:(X.Caller.singleton x)
        |> [%sexp_of: Rpc.Description.t Or_error.t]
      in
      print_endline banner;
      print_s
        [%message
          (callee_rpcs : Rpc.Description.t list Or_error.t)
            (caller v1 : Sexp.t)
            (caller v2 : Sexp.t)
            (caller bad_v2 : Sexp.t)])
  ;;

  let test_caller_converts () =
    List.iter callers ~f:(fun caller ->
      let caller_rpcs = Babel.Caller.descriptions caller in
      let callee x =
        Babel.check_compatibility ~caller ~callee:(X.Callee.singleton x)
        |> [%sexp_of: Rpc.Description.t Or_error.t]
      in
      print_endline banner;
      print_s
        [%message
          (caller_rpcs : Rpc.Description.t Nonempty_list.t)
            (callee v1 : Sexp.t)
            (callee v2 : Sexp.t)
            (callee bad_v2 : Sexp.t)])
  ;;

  let test_both_convert () =
    List.iter callers ~f:(fun caller ->
      let caller_rpcs = Babel.Caller.descriptions caller in
      List.iter callees ~f:(fun callee ->
        let callee_rpcs = Babel.Callee.descriptions callee in
        let selected_rpc = Babel.check_compatibility ~caller ~callee in
        print_endline banner;
        print_s
          [%message
            (caller_rpcs : Rpc.Description.t Nonempty_list.t)
              (callee_rpcs : Rpc.Description.t list Or_error.t)
              (selected_rpc : Rpc.Description.t Or_error.t)]))
  ;;
end

module%test Rpc = struct
  include Setup (struct
      type ('q, 'r) rpc = ('q, 'r) Rpc.Rpc.t

      let create_rpc
        (type q r)
        (module Q : Q with type t = q)
        (module R : R with type t = r)
        ~name
        ~version
        =
        Rpc.Rpc.create
          ~name
          ~version
          ~bin_query:Q.bin_t
          ~bin_response:R.bin_t
          ~include_in_error_count:Only_on_exn
      ;;

      module Callee = struct
        module Monad = Deferred
        include Babel.Callee.Rpc
      end

      module Caller = struct
        module Monad = Deferred.Or_error
        include Babel.Caller.Rpc
      end
    end)

  let%expect_test "callee shapes" =
    print_callee_shapes ();
    [%expect
      {|
      -------------------------------------------------------------
      (Ok ())
      -------------------------------------------------------------
      (Ok
       ((create-breakfast-buffet
         ((2
           (Rpc (query b3089c21b0f09f5502848bf675a1ac86)
            (response 4a65f33ff4f5558e046675409de91fad)))))))
      -------------------------------------------------------------
      (Ok
       ((create-breakfast-buffet
         ((1
           (Rpc (query 8b1cf33880547d84b741f11596b155d9)
            (response 4a65f33ff4f5558e046675409de91fad)))))))
      -------------------------------------------------------------
      (Ok
       ((create-breakfast-buffet
         ((1
           (Rpc (query 8b1cf33880547d84b741f11596b155d9)
            (response 4a65f33ff4f5558e046675409de91fad)))
          (2
           (Rpc (query b3089c21b0f09f5502848bf675a1ac86)
            (response 4a65f33ff4f5558e046675409de91fad)))))))
      |}]
  ;;

  let%expect_test "callee converts" =
    test_callee_converts ();
    [%expect
      {|
      -------------------------------------------------------------
      ((callee_rpcs (Ok ())) ("caller v1" (Error "Could not match any rpcs"))
       ("caller v2" (Error "Could not match any rpcs"))
       ("caller bad_v2" (Error "Could not match any rpcs")))
      -------------------------------------------------------------
      ((callee_rpcs (Ok (((name create-breakfast-buffet) (version 2)))))
       ("caller v1" (Error "Could not match any rpcs"))
       ("caller v2" (Ok ((name create-breakfast-buffet) (version 2))))
       ("caller bad_v2"
        (Error
         ("Corresponding rpcs have mismatching shapes"
          (caller_description ((name create-breakfast-buffet) (version 2)))
          (callee_description ((name create-breakfast-buffet) (version 2)))))))
      -------------------------------------------------------------
      ((callee_rpcs (Ok (((name create-breakfast-buffet) (version 1)))))
       ("caller v1" (Ok ((name create-breakfast-buffet) (version 1))))
       ("caller v2" (Error "Could not match any rpcs"))
       ("caller bad_v2" (Error "Could not match any rpcs")))
      -------------------------------------------------------------
      ((callee_rpcs
        (Ok
         (((name create-breakfast-buffet) (version 1))
          ((name create-breakfast-buffet) (version 2)))))
       ("caller v1" (Ok ((name create-breakfast-buffet) (version 1))))
       ("caller v2" (Ok ((name create-breakfast-buffet) (version 2))))
       ("caller bad_v2"
        (Error
         ("Corresponding rpcs have mismatching shapes"
          (caller_description ((name create-breakfast-buffet) (version 2)))
          (callee_description ((name create-breakfast-buffet) (version 2)))))))
      |}]
  ;;

  let%expect_test "caller shapes" =
    print_caller_shapes ();
    [%expect
      {|
      -------------------------------------------------------------
      ((((name create-breakfast-buffet) (version 2))
        (Rpc (query b3089c21b0f09f5502848bf675a1ac86)
         (response 4a65f33ff4f5558e046675409de91fad))))
      -------------------------------------------------------------
      ((((name create-breakfast-buffet) (version 1))
        (Rpc (query 8b1cf33880547d84b741f11596b155d9)
         (response 4a65f33ff4f5558e046675409de91fad))))
      -------------------------------------------------------------
      ((((name create-breakfast-buffet) (version 2))
        (Rpc (query b3089c21b0f09f5502848bf675a1ac86)
         (response 4a65f33ff4f5558e046675409de91fad)))
       (((name create-breakfast-buffet) (version 1))
        (Rpc (query 8b1cf33880547d84b741f11596b155d9)
         (response 4a65f33ff4f5558e046675409de91fad))))
      |}]
  ;;

  let%expect_test "caller converts" =
    test_caller_converts ();
    [%expect
      {|
      -------------------------------------------------------------
      ((caller_rpcs (((name create-breakfast-buffet) (version 2))))
       ("callee v1" (Error "Could not match any rpcs"))
       ("callee v2" (Ok ((name create-breakfast-buffet) (version 2))))
       ("callee bad_v2"
        (Error
         ("Corresponding rpcs have mismatching shapes"
          (caller_description ((name create-breakfast-buffet) (version 2)))
          (callee_description ((name create-breakfast-buffet) (version 2)))))))
      -------------------------------------------------------------
      ((caller_rpcs (((name create-breakfast-buffet) (version 1))))
       ("callee v1" (Ok ((name create-breakfast-buffet) (version 1))))
       ("callee v2" (Error "Could not match any rpcs"))
       ("callee bad_v2" (Error "Could not match any rpcs")))
      -------------------------------------------------------------
      ((caller_rpcs
        (((name create-breakfast-buffet) (version 2))
         ((name create-breakfast-buffet) (version 1))))
       ("callee v1" (Ok ((name create-breakfast-buffet) (version 1))))
       ("callee v2" (Ok ((name create-breakfast-buffet) (version 2))))
       ("callee bad_v2"
        (Error
         ("Corresponding rpcs have mismatching shapes"
          (caller_description ((name create-breakfast-buffet) (version 2)))
          (callee_description ((name create-breakfast-buffet) (version 2)))))))
      |}]
  ;;

  let%expect_test "both convert" =
    test_both_convert ();
    [%expect
      {|
      -------------------------------------------------------------
      ((caller_rpcs (((name create-breakfast-buffet) (version 2))))
       (callee_rpcs (Ok ())) (selected_rpc (Error "Could not match any rpcs")))
      -------------------------------------------------------------
      ((caller_rpcs (((name create-breakfast-buffet) (version 2))))
       (callee_rpcs (Ok (((name create-breakfast-buffet) (version 2)))))
       (selected_rpc (Ok ((name create-breakfast-buffet) (version 2)))))
      -------------------------------------------------------------
      ((caller_rpcs (((name create-breakfast-buffet) (version 2))))
       (callee_rpcs (Ok (((name create-breakfast-buffet) (version 1)))))
       (selected_rpc (Error "Could not match any rpcs")))
      -------------------------------------------------------------
      ((caller_rpcs (((name create-breakfast-buffet) (version 2))))
       (callee_rpcs
        (Ok
         (((name create-breakfast-buffet) (version 1))
          ((name create-breakfast-buffet) (version 2)))))
       (selected_rpc (Ok ((name create-breakfast-buffet) (version 2)))))
      -------------------------------------------------------------
      ((caller_rpcs (((name create-breakfast-buffet) (version 1))))
       (callee_rpcs (Ok ())) (selected_rpc (Error "Could not match any rpcs")))
      -------------------------------------------------------------
      ((caller_rpcs (((name create-breakfast-buffet) (version 1))))
       (callee_rpcs (Ok (((name create-breakfast-buffet) (version 2)))))
       (selected_rpc (Error "Could not match any rpcs")))
      -------------------------------------------------------------
      ((caller_rpcs (((name create-breakfast-buffet) (version 1))))
       (callee_rpcs (Ok (((name create-breakfast-buffet) (version 1)))))
       (selected_rpc (Ok ((name create-breakfast-buffet) (version 1)))))
      -------------------------------------------------------------
      ((caller_rpcs (((name create-breakfast-buffet) (version 1))))
       (callee_rpcs
        (Ok
         (((name create-breakfast-buffet) (version 1))
          ((name create-breakfast-buffet) (version 2)))))
       (selected_rpc (Ok ((name create-breakfast-buffet) (version 1)))))
      -------------------------------------------------------------
      ((caller_rpcs
        (((name create-breakfast-buffet) (version 2))
         ((name create-breakfast-buffet) (version 1))))
       (callee_rpcs (Ok ())) (selected_rpc (Error "Could not match any rpcs")))
      -------------------------------------------------------------
      ((caller_rpcs
        (((name create-breakfast-buffet) (version 2))
         ((name create-breakfast-buffet) (version 1))))
       (callee_rpcs (Ok (((name create-breakfast-buffet) (version 2)))))
       (selected_rpc (Ok ((name create-breakfast-buffet) (version 2)))))
      -------------------------------------------------------------
      ((caller_rpcs
        (((name create-breakfast-buffet) (version 2))
         ((name create-breakfast-buffet) (version 1))))
       (callee_rpcs (Ok (((name create-breakfast-buffet) (version 1)))))
       (selected_rpc (Ok ((name create-breakfast-buffet) (version 1)))))
      -------------------------------------------------------------
      ((caller_rpcs
        (((name create-breakfast-buffet) (version 2))
         ((name create-breakfast-buffet) (version 1))))
       (callee_rpcs
        (Ok
         (((name create-breakfast-buffet) (version 1))
          ((name create-breakfast-buffet) (version 2)))))
       (selected_rpc (Ok ((name create-breakfast-buffet) (version 2)))))
      |}]
  ;;
end

module%test Streamable_plain_rpc = struct
  include Setup (struct
      type ('q, 'r) rpc = ('q, 'r) Streamable.Plain_rpc.t

      module type P = sig
        type query
        type response

        val rpc : (query, response) Streamable.Plain_rpc.t
      end

      let create_rpc
        (type q r)
        (module Q : Q with type t = q)
        (module R : R with type t = r)
        ~name
        ~version
        =
        let (module P : P with type query = Q.t and type response = R.t) =
          (module struct
            type query = Q.t
            type response = R.t

            include Streamable.Plain_rpc.Make (struct
                let name = name
                let version = version

                type query = Q.t [@@deriving bin_io]
                type response = R.t

                module Response = R

                let client_pushes_back = false
              end)
          end)
        in
        P.rpc
      ;;

      module Callee = struct
        module Monad = Deferred.Or_error
        include Babel.Callee.Streamable_plain_rpc
      end

      module Caller = struct
        module Monad = struct
          type 'a t = 'a Or_error.t Or_error.t Deferred.t

          include Monad.Make (struct
              type nonrec 'a t = 'a t

              let bind t ~f =
                match%bind.Deferred.Or_error t with
                | Error _ as error -> Deferred.Or_error.return error
                | Ok x -> f x
              ;;

              let return x = Deferred.Or_error.return (Ok x)
              let map = `Define_using_bind
            end)
        end

        include Babel.Caller.Streamable_plain_rpc
      end
    end)

  let%expect_test "callee shapes" =
    print_callee_shapes ();
    [%expect
      {|
      -------------------------------------------------------------
      (Ok ())
      -------------------------------------------------------------
      (Ok
       ((create-breakfast-buffet
         ((2
           (Streamable_plain_rpc (query b3089c21b0f09f5502848bf675a1ac86)
            (response 4dcf125bc50b7b1690b2f33afcd521e1)))))))
      -------------------------------------------------------------
      (Ok
       ((create-breakfast-buffet
         ((1
           (Streamable_plain_rpc (query 8b1cf33880547d84b741f11596b155d9)
            (response 4dcf125bc50b7b1690b2f33afcd521e1)))))))
      -------------------------------------------------------------
      (Ok
       ((create-breakfast-buffet
         ((1
           (Streamable_plain_rpc (query 8b1cf33880547d84b741f11596b155d9)
            (response 4dcf125bc50b7b1690b2f33afcd521e1)))
          (2
           (Streamable_plain_rpc (query b3089c21b0f09f5502848bf675a1ac86)
            (response 4dcf125bc50b7b1690b2f33afcd521e1)))))))
      |}]
  ;;

  let%expect_test "callee converts" =
    test_callee_converts ();
    [%expect
      {|
      -------------------------------------------------------------
      ((callee_rpcs (Ok ())) ("caller v1" (Error "Could not match any rpcs"))
       ("caller v2" (Error "Could not match any rpcs"))
       ("caller bad_v2" (Error "Could not match any rpcs")))
      -------------------------------------------------------------
      ((callee_rpcs (Ok (((name create-breakfast-buffet) (version 2)))))
       ("caller v1" (Error "Could not match any rpcs"))
       ("caller v2" (Ok ((name create-breakfast-buffet) (version 2))))
       ("caller bad_v2"
        (Error
         ("Corresponding rpcs have mismatching shapes"
          (caller_description ((name create-breakfast-buffet) (version 2)))
          (callee_description ((name create-breakfast-buffet) (version 2)))))))
      -------------------------------------------------------------
      ((callee_rpcs (Ok (((name create-breakfast-buffet) (version 1)))))
       ("caller v1" (Ok ((name create-breakfast-buffet) (version 1))))
       ("caller v2" (Error "Could not match any rpcs"))
       ("caller bad_v2" (Error "Could not match any rpcs")))
      -------------------------------------------------------------
      ((callee_rpcs
        (Ok
         (((name create-breakfast-buffet) (version 1))
          ((name create-breakfast-buffet) (version 2)))))
       ("caller v1" (Ok ((name create-breakfast-buffet) (version 1))))
       ("caller v2" (Ok ((name create-breakfast-buffet) (version 2))))
       ("caller bad_v2"
        (Error
         ("Corresponding rpcs have mismatching shapes"
          (caller_description ((name create-breakfast-buffet) (version 2)))
          (callee_description ((name create-breakfast-buffet) (version 2)))))))
      |}]
  ;;

  let%expect_test "caller shapes" =
    print_caller_shapes ();
    [%expect
      {|
      -------------------------------------------------------------
      ((((name create-breakfast-buffet) (version 2))
        (Streamable_plain_rpc (query b3089c21b0f09f5502848bf675a1ac86)
         (response 4dcf125bc50b7b1690b2f33afcd521e1))))
      -------------------------------------------------------------
      ((((name create-breakfast-buffet) (version 1))
        (Streamable_plain_rpc (query 8b1cf33880547d84b741f11596b155d9)
         (response 4dcf125bc50b7b1690b2f33afcd521e1))))
      -------------------------------------------------------------
      ((((name create-breakfast-buffet) (version 2))
        (Streamable_plain_rpc (query b3089c21b0f09f5502848bf675a1ac86)
         (response 4dcf125bc50b7b1690b2f33afcd521e1)))
       (((name create-breakfast-buffet) (version 1))
        (Streamable_plain_rpc (query 8b1cf33880547d84b741f11596b155d9)
         (response 4dcf125bc50b7b1690b2f33afcd521e1))))
      |}]
  ;;

  let%expect_test "caller converts" =
    test_caller_converts ();
    [%expect
      {|
      -------------------------------------------------------------
      ((caller_rpcs (((name create-breakfast-buffet) (version 2))))
       ("callee v1" (Error "Could not match any rpcs"))
       ("callee v2" (Ok ((name create-breakfast-buffet) (version 2))))
       ("callee bad_v2"
        (Error
         ("Corresponding rpcs have mismatching shapes"
          (caller_description ((name create-breakfast-buffet) (version 2)))
          (callee_description ((name create-breakfast-buffet) (version 2)))))))
      -------------------------------------------------------------
      ((caller_rpcs (((name create-breakfast-buffet) (version 1))))
       ("callee v1" (Ok ((name create-breakfast-buffet) (version 1))))
       ("callee v2" (Error "Could not match any rpcs"))
       ("callee bad_v2" (Error "Could not match any rpcs")))
      -------------------------------------------------------------
      ((caller_rpcs
        (((name create-breakfast-buffet) (version 2))
         ((name create-breakfast-buffet) (version 1))))
       ("callee v1" (Ok ((name create-breakfast-buffet) (version 1))))
       ("callee v2" (Ok ((name create-breakfast-buffet) (version 2))))
       ("callee bad_v2"
        (Error
         ("Corresponding rpcs have mismatching shapes"
          (caller_description ((name create-breakfast-buffet) (version 2)))
          (callee_description ((name create-breakfast-buffet) (version 2)))))))
      |}]
  ;;

  let%expect_test "both convert" =
    test_both_convert ();
    [%expect
      {|
      -------------------------------------------------------------
      ((caller_rpcs (((name create-breakfast-buffet) (version 2))))
       (callee_rpcs (Ok ())) (selected_rpc (Error "Could not match any rpcs")))
      -------------------------------------------------------------
      ((caller_rpcs (((name create-breakfast-buffet) (version 2))))
       (callee_rpcs (Ok (((name create-breakfast-buffet) (version 2)))))
       (selected_rpc (Ok ((name create-breakfast-buffet) (version 2)))))
      -------------------------------------------------------------
      ((caller_rpcs (((name create-breakfast-buffet) (version 2))))
       (callee_rpcs (Ok (((name create-breakfast-buffet) (version 1)))))
       (selected_rpc (Error "Could not match any rpcs")))
      -------------------------------------------------------------
      ((caller_rpcs (((name create-breakfast-buffet) (version 2))))
       (callee_rpcs
        (Ok
         (((name create-breakfast-buffet) (version 1))
          ((name create-breakfast-buffet) (version 2)))))
       (selected_rpc (Ok ((name create-breakfast-buffet) (version 2)))))
      -------------------------------------------------------------
      ((caller_rpcs (((name create-breakfast-buffet) (version 1))))
       (callee_rpcs (Ok ())) (selected_rpc (Error "Could not match any rpcs")))
      -------------------------------------------------------------
      ((caller_rpcs (((name create-breakfast-buffet) (version 1))))
       (callee_rpcs (Ok (((name create-breakfast-buffet) (version 2)))))
       (selected_rpc (Error "Could not match any rpcs")))
      -------------------------------------------------------------
      ((caller_rpcs (((name create-breakfast-buffet) (version 1))))
       (callee_rpcs (Ok (((name create-breakfast-buffet) (version 1)))))
       (selected_rpc (Ok ((name create-breakfast-buffet) (version 1)))))
      -------------------------------------------------------------
      ((caller_rpcs (((name create-breakfast-buffet) (version 1))))
       (callee_rpcs
        (Ok
         (((name create-breakfast-buffet) (version 1))
          ((name create-breakfast-buffet) (version 2)))))
       (selected_rpc (Ok ((name create-breakfast-buffet) (version 1)))))
      -------------------------------------------------------------
      ((caller_rpcs
        (((name create-breakfast-buffet) (version 2))
         ((name create-breakfast-buffet) (version 1))))
       (callee_rpcs (Ok ())) (selected_rpc (Error "Could not match any rpcs")))
      -------------------------------------------------------------
      ((caller_rpcs
        (((name create-breakfast-buffet) (version 2))
         ((name create-breakfast-buffet) (version 1))))
       (callee_rpcs (Ok (((name create-breakfast-buffet) (version 2)))))
       (selected_rpc (Ok ((name create-breakfast-buffet) (version 2)))))
      -------------------------------------------------------------
      ((caller_rpcs
        (((name create-breakfast-buffet) (version 2))
         ((name create-breakfast-buffet) (version 1))))
       (callee_rpcs (Ok (((name create-breakfast-buffet) (version 1)))))
       (selected_rpc (Ok ((name create-breakfast-buffet) (version 1)))))
      -------------------------------------------------------------
      ((caller_rpcs
        (((name create-breakfast-buffet) (version 2))
         ((name create-breakfast-buffet) (version 1))))
       (callee_rpcs
        (Ok
         (((name create-breakfast-buffet) (version 1))
          ((name create-breakfast-buffet) (version 2)))))
       (selected_rpc (Ok ((name create-breakfast-buffet) (version 2)))))
      |}]
  ;;
end
