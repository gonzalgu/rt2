
open Vec3
let write_color f color =
  let pixel_value x = Float.to_int (255.999 *. x) in
  let _ = Printf.eprintf "p_x = %d; p_y = %d; p_z = %d\n"
      (pixel_value color.x) (pixel_value color.y) (pixel_value color.z)
  in
  Printf.fprintf f "%d %d %d\n"
    (pixel_value color.x) (pixel_value color.y) (pixel_value color.z)
    
      
