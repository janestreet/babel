open Core

(** Some extended functionality for [Fn] we use in the implementation of Babel that
    probably doesn't have any business being moved to [Base]. *)

include module type of Fn
include Applicative.S2 with type ('a, 'b) t := 'b -> 'a

val map_input : ('a1 -> 'b) -> f:('a2 -> 'a1) -> 'a2 -> 'b
