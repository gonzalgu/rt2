(* open Modules.Vec3 *)
open Modules
open Vec3

let degrees_to_radians deg = deg *. Float.pi /. 180.0;;
                             
let print_vec (label:string) (v:Vec3.t) =
  Printf.eprintf "%s=vec3{x=%F;y=%F;z=%F}\n"
    label v.x v.y v.z;;

let rec ray_color (r:Ray.t) (world:Hittable.hittable) (depth:int):Vec3.t =
  if depth <= 0 then
    Vec3.create 0. 0. 0.
  else 
    match Hittable.hit world r 0.001 Float.infinity Hittable.empty_hit_rec with
    | Some(hrec') ->
      let target = hrec'.p +: Vec3.random_in_hemisphere hrec'.normal 
      in 0.5 *| ray_color (Ray.create hrec'.p  (target -: hrec'.p)) world (depth - 1) 
    | None ->
      let unit_direction = Vec3.unit_vector r.direction in
      let t = 0.5 *. (unit_direction.y +. 1.0) in
      ((1.0 -. t) *| Vec3.create 1.0 1.0 1.0) +: (t *| Vec3.create 0.5 0.7 1.0);;


(* image *)
let aspect_ratio = 16.0 /. 9.0;;
let image_width = 400;;
let image_height = Float.to_int ((Float.of_int image_width) /.  aspect_ratio);;
let samples_per_pixel = 100;;
let max_depth = 50;;

(* world *)
let world = Hittable.(
    Hit_list( 
      [
        Hittable.of_sphere { center = Vec3.create 0. 0. (0. -. 1.); radius = 0.5 };
        Hittable.of_sphere { center = Vec3.create 0. (0. -. 100.5) (0. -. 1.); radius = 100. }
      ])
  );;


(* camera *)
let camera = Camera.create
    ~aspect_ratio:aspect_ratio
    ~viewport_height:2.0
    ~focal_length:1.0
;;


(* render *)
Printf.printf "P3\n%d %d\n255\n" image_width image_height;
for j = (image_height-1) downto 0 do
  begin
    Printf.eprintf "\rScanlines remaining: %d\n" j;
    for i = 0 to (image_width-1) do
      let g x y = ((Float.of_int x) +. Rtweekend.random_f ()) /. ((Float.of_int y) -. 1.) in
      let seq = Seq.unfold (fun x -> if x > 0 then Some(x-1, x-1) else None) samples_per_pixel in
      let initial_color = Vec3.create 0. 0. 0. in
      let pixel_color = seq
                        |> Seq.fold_left (fun pix_col _ ->
                            let u = g i image_width in
                            let v = g j image_height in
                            let r = Camera.get_ray u v camera in
                            (pix_col +: ray_color r world max_depth)) initial_color
      in      
      Color.write_color stdout pixel_color samples_per_pixel      
    done
  end
done;
Printf.eprintf "\nDone\n"
