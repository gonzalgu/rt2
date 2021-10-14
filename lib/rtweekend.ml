
(* random float in [0, 1) *)
let random_f () : float =
  Random.float(Float.pred 1.)
;;

let random_float min max =
  min +. (max -. min) *. (random_f ())
;;

let clamp (x:float) (min:float) (max:float) : float = 
  if x < min then min
  else if x > max then max
  else x
;;
