open Core
include Fn

include Applicative.Make2_using_map2 (struct
    type ('a, 'b) t = 'b -> 'a

    let return = const
    let map t ~f a = f (t a)
    let map2 t1 t2 ~f a = f (t1 a) (t2 a)
    let map = `Custom map
  end)

let map_input t ~f x = t (f x)
