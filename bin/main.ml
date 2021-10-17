
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
      begin
        let zVec = Vec3.create 0. 0. 0. in 
        let scattered = Ray.create zVec zVec in
        let attenuation = Vec3.create 0. 0. 0. in 
        match Hittable.scatter r hrec' attenuation scattered hrec'.mat with
        | Some (attenuation', scattered') ->
          attenuation' *: ray_color scattered' world (depth-1)
        | None -> Vec3.create 0. 0. 0.
      end 
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
    let material_ground = Lambertian { albedo = Vec3.create 0.8 0.8 0.0 } in
    let material_center = Lambertian { albedo = Vec3.create 0.7 0.3 0.3 } in
    let material_left   = Metal { albedo = Vec3.create 0.8 0.8 0.8 } in
    let material_right = Metal { albedo = Vec3.create 0.8 0.6 0.2 }
    in
    Hit_list( 
      [
        Hittable.of_sphere {
          center = Vec3.create 0. (-. 100.5) (-. 1.);
          mat = material_ground;
          radius = 100.0
        };
        Hittable.of_sphere {
          center = Vec3.create 0. 0. (-. 1.);
          mat = material_center;          
          radius = 0.5 };
        Hittable.of_sphere {
          center = Vec3.create (-. 1.) 0. (-. 1.);
          mat = material_left;
          radius = 0.5
        };
        Hittable.of_sphere {
          center = Vec3.create 1. 0. (-. 1.);
          mat = material_right;
          radius = 0.5
        }
      ])
  );;


(* camera *)
let camera = Camera.create
    ~aspect_ratio:aspect_ratio
    ~viewport_height:2.0
    ~focal_length:1.0
;;



let sampled_pixel_color (i:int) (j:int) (samples_per_pixel:int) : Vec3.t =
  let initial_color = Vec3.create 0. 0. 0. in
  let get_rand_offset x y =
    (Float.of_int x +. Rtweekend.random_f ()) /. (Float.of_int y -. 1.) in

  let make_seq x =
    if x > 0
    then Some(x-1, x-1)
    else None
  in 

  let seq = Seq.unfold make_seq samples_per_pixel in

  let accumulate_sampled_pixel pix_color _ =
    let u = get_rand_offset i image_width
    and v = get_rand_offset j image_height in
    let r = Camera.get_ray u v camera in
    pix_color +: ray_color r world max_depth
  in  
  seq
  |> Seq.fold_left accumulate_sampled_pixel initial_color
;;



(* render *)
Printf.printf "P3\n%d %d\n255\n" image_width image_height;
for j = (image_height-1) downto 0 do
  begin
    Printf.eprintf "\rScanlines remaining: %d\n" j;
    for i = 0 to (image_width-1) do

      let pixel_color = sampled_pixel_color i j samples_per_pixel
      in
      Color.write_color stdout pixel_color samples_per_pixel
        
    done
  end
done;
Printf.eprintf "\nDone\n"
