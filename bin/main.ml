(* open Modules.Vec3 *)
open Modules
open Vec3    


let print_vec label v =
  Printf.eprintf "%s=vec3{x=%F;y=%F;z=%F}\n"
    label v.x v.y v.z;;


let ray_color r =
  let open Ray in  
  let unit_direction = unit_vector r.direction in
  let _ = print_vec "unit_direction" unit_direction in
  let t = 0.5 *. (unit_direction.y +. 1.0) in
  let _ = Printf.eprintf "t=%F\n" t in
  let le = (1.0 -. t) *| (Vec3.create 1. 1. 1.) in
  let re = (t *| Vec3.create 0.5 0.7 1.0) in
  let _ = print_vec "le" le in
  let _ = print_vec "re" re in
  let result = le +: re in
  let _ = print_vec "result" result in
  result

(* image *)
let aspect_ratio = 16.0 /. 9.0;;
let image_width = 400;;
let image_height = Float.to_int ((Float.of_int image_width) /.  aspect_ratio);;

(* camera *)
let viewport_height = 2.0;;
let viewport_width = aspect_ratio *. viewport_height;;
let focal_length = 1.0;;

let origin = create 0. 0. 0.;;
let horizontal = create viewport_width 0. 0.;;
let vertical = create 0. viewport_height 0.;;
let lower_left_corner =
  origin -: ((horizontal /$ 2.) +: (vertical /$ 2.) +: create 0. 0. focal_length);;

let _ = Printf.eprintf "llc={x=%F;y=%F;z=%F}\n"
    lower_left_corner.x lower_left_corner.y lower_left_corner.z;;


(* render *)
Printf.printf "P3\n%d %d\n255\n" image_width image_height;
for j = (image_height-1) downto 0 do
  begin
    Printf.eprintf "\rScanlines remaining: %d\n" j;
    for i = 0 to (image_width-1) do
      let u = (Float.of_int i) /. Float.of_int (image_width-1) in
      let v = (Float.of_int j) /. Float.of_int (image_height-1) in
      let d = (lower_left_corner +:
           (u *| horizontal) +:
               ((v *| vertical) -: origin)) in
      let _ = print_vec "d" d in      
      let r = Ray.create origin d in
      let _ = print_vec "r.o" r.origin in
      let _ = print_vec "r.d" r.direction in 
      let pixel_color = ray_color r in
      let _ = print_vec "p" pixel_color in
      Color.write_color stdout pixel_color      
    done
  end
done;
Printf.eprintf "\nDone\n"
