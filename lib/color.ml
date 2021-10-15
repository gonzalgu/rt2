
open Vec3

let get_clamped_color c =
  Int.of_float @@ 256.0 *. Rtweekend.clamp c 0. 0.999
;;

let write_color f color samples_per_pixel =
  let scale = 1.0 /. (Float.of_int samples_per_pixel)
  in
  Printf.fprintf f "%d %d %d\n"
    (get_clamped_color @@ Float.sqrt(scale *. color.x))
    (get_clamped_color @@ Float.sqrt(scale *. color.y))
    (get_clamped_color @@ Float.sqrt(scale *. color.z))
;;

    
      
