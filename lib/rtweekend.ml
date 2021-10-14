
(* random float in [0, 1) *)
let random_f () : float =
  Random.float(Float.pred 1.)
;;

let random_float min max =
  min +. (max -. min) *. (random_f ())
;;
