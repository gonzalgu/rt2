(* open Modules.Vec3 *)
open Modules
open Vec3    


let print_vec (label:string) (v:Vec3.t) =
  Printf.eprintf "%s=vec3{x=%F;y=%F;z=%F}\n"
    label v.x v.y v.z;;

let hit_sphere (center:Vec3.t) (radius:float) (r:Ray.t) : float =
  let open Ray in
  let oc = r.origin -: center in
  let a = dot r.direction r.direction in
  let b = 2.0 *. dot oc r.direction in
  let c = dot oc oc -. radius*.radius in
  let discriminant = b *. b -. 4. *. a *. c in
  if discriminant < 0.
  then  -.1.0
  else ((-. b) -. Float.sqrt discriminant) /. (2.0 *. a)

           

let ray_color (r:Ray.t):Vec3.t =
  let open Ray in
  let t = hit_sphere (Vec3.create 0. 0. (-1.)) 0.5 r in
  if t > 0.0
  then
    let n = (Ray.at r t -: Vec3.create 0. 0. (-1.0)) |> unit_vector in
    0.5 *| Vec3.create (n.x +. 1.) (n.y +. 1.) (n.z +. 1.)
  else 
    let unit_direction = unit_vector r.direction in
    let t = 0.5 *. (unit_direction.y +. 1.0) in
    let le = (1.0 -. t) *| (Vec3.create 1. 1. 1.) in
    let re = (t *| Vec3.create 0.5 0.7 1.0) in
    let result = le +: re in
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
      let r = Ray.create origin d in
      let pixel_color = ray_color r in
      Color.write_color stdout pixel_color      
    done
  end
done;
Printf.eprintf "\nDone\n"
