
open Modules
open Vec3
open Domainslib

let usage_msg = "rt2 [-width] [-samples] [-tword] [-threads]"
let width = ref 600
let samples = ref 100
let use_test_world = ref false
let thread_pool_size = ref 1

let speclist = 
  [("-width", Arg.Set_int width, "Image width");
  ("-samples", Arg.Set_int samples, "pixel samples");
  ("-test_world", Arg.Set use_test_world, "use small test world");
  ("-threads", Arg.Set_int thread_pool_size, "render thread pool size") ]

let () = Arg.parse speclist (fun _ -> ()) usage_msg;;                             

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
let aspect_ratio = 3.0 /. 2.0;;
let image_width = !width;;
let image_height = Float.to_int ((Float.of_int image_width) /.  aspect_ratio);;
let samples_per_pixel = !samples;;
let max_depth = 50;;

(* world *)
let world = 
  if !use_test_world 
  then Scene.testWorld () 
  else Scene.random_scene()

(* camera *)
let camera =
  let lookfrom = Vec3.create 13. 2. 3. in
  let lookat = Vec3.create 0. 0. 0. in
  let vup = Vec3.create 0. 1. 0. in
  let dist_to_focus = 10.0 in
  let aperture = 0.1
  in
  Camera.create
    ~lookfrom:lookfrom
    ~lookat:lookat
    ~vup:vup
    ~vfov:20.0
    ~aspect_ratio:aspect_ratio
    ~aperture:aperture
    ~focus_dist:dist_to_focus
;;

let sampled_pixel_color (i:int) (j:int) (samples_per_pixel:int) : Vec3.t =
  let initial_color = Vec3.create 0. 0. 0. in
  let get_rand_offset x y = (Float.of_int x +. Rtweekend.random_f ()) /. (Float.of_int y -. 1.) in
  let make_seq x = if x > 0 then Some(x-1, x-1) else None
  in
  let accumulate_sampled_pixel pix_color _ =
    let u = get_rand_offset i image_width
    and v = get_rand_offset j image_height in
    let r = Camera.get_ray u v camera in
    pix_color +: ray_color r world max_depth
  in
  Seq.unfold make_seq samples_per_pixel 
  |> Seq.fold_left accumulate_sampled_pixel initial_color  
;;

let par_sampled_pixel_color pool (i:int) (j:int) (samples_per_pixel:int) : Vec3.t =
  let z = Vec3.create 0. 0. 0. in
  let sum = Vec3.( +: ) in  
  Task.parallel_for_reduce ~start:0 ~finish:samples_per_pixel ~body:(fun _ -> 
    let u = (Float.of_int i +. Rtweekend.random_f ()) /. (Float.of_int image_width -. 1.) in
    let v = (Float.of_int j +. Rtweekend.random_f ()) /. (Float.of_int image_height -. 1.) in
    let r = Camera.get_ray u v camera in 
    let pixel = ray_color r world max_depth in
    pixel
  ) pool sum z
;;

let pool = Task.setup_pool ~num_additional_domains:!thread_pool_size () ;;
let img = Array.make_matrix image_height image_width @@ Vec3.create 0. 0. 0. ;;


let t_start = Unix.gettimeofday();;
Task.parallel_for pool ~start:0 ~finish:(image_height-1) ~body:(fun k -> 
  for i = 0 to (image_width-1) do
    let j = image_height-1-k in
    let pixel_color = sampled_pixel_color i j samples_per_pixel 
    in img.(j).(i) <- pixel_color;
  done
);;
let t_end = Unix.gettimeofday();;

(* render image *)
Printf.printf "P3\n%d %d\n255\n" image_width image_height;
for j = (image_height-1) downto 0 do
  Printf.eprintf "\rScanlines remaining: %d\n%!" j;
  for i = 0 to (image_width-1) do
    let pixel_color = img.(j).(i) in
    Color.write_color stdout pixel_color samples_per_pixel;
  done;    
done;
Printf.eprintf "\nDone\n%!";
Printf.eprintf "\nrendering time: %F\n" (t_end -. t_start)
;;
