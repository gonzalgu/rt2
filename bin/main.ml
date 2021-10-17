
open Modules
open Vec3
                             
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
let rec range a b =
  if a > b
  then Seq.empty
  else Seq.cons a (range (a+1) b)

let ( -- ) a b = range a b

let cartesian s1 s2 =
  s1 |> Seq.flat_map (fun x ->
      s2 |> Seq.map (fun y -> (x,y)))
                         
let create_sphere  choose_mat center =
  if choose_mat < 0.8 then
    let albedo = Vec3.vec3_rand () *: Vec3.vec3_rand () in
    let sphere_material = Hittable.Lambertian { albedo } in
    Hittable.(of_sphere { center = center; radius = 0.2; mat = sphere_material })
  else if choose_mat < 0.95 then
    let albedo = Vec3.vec3_random 0.5 1.0 in
    let fuzz = Rtweekend.random_float 0. 0.5 in
    let sphere_material = Hittable.Metal { albedo; fuzz } in
    Hittable.(of_sphere { center = center; radius = 0.2; mat = sphere_material })
  else
    let sphere_material = Hittable.Dielectric{ ir = 1.5 } in
    Hittable.(of_sphere { center = center; radius = 0.2; mat = sphere_material })


let create_rand_sphere a b =
    let choose_mat = Rtweekend.random_f () in
    let x = Float.of_int a +. 0.9 *. Rtweekend.random_f () in
    let y = 0.2 in
    let z = Float.of_int b +. 0.9 *. Rtweekend.random_f () in      
    let center = Vec3.create x y z in
    if Vec3.length (center -: Vec3.create 4. 0.2 0.) > 0.9
    then
      let sphere = create_sphere choose_mat in
      Some(sphere)
    else
      None


let random_scene () : Hittable.Hit_list =
  let open Hittable in
  let ground_material = make_lambertian 0.5 0.5 0.5 in
  let s = cartesian (-11 -- 10) (-11 -- 10) in  
  in 
  let world = 
    s
    |> Seq.map create_rand_sphere
    |> Seq.filter Option.is_some
    |> Seq.map Option.get
    |> List.of_seq
  in
  let material1 = Hittable.make_dielectric 1.5 in
  let sphere1 = Hittable.of_sphere {
                    center = Vec3.create 0. 1. 0.;
                    radius = 1.0;
                    mat = material1}
  in
  let material2 = Hittable.make_lambertian 0.4 0.2 0.1 in
  let sphere2 = Hittable.of_sphere {
                    center = Vec3.create (-.4.0) 1. 0.; 
                    radius = 1.0;
                    mat = material2
                  }
  in
  let material3 = Hittable.make_metal 0.7 0.6 0.5 0.0 in
  let sphere3 = Hittable.of_sphere {
                    center = Vec3.create 4.0 1.0 0.0;
                    radius = 1.0;
                    mat = material3
                  }
  in
  Hittable.Hit_list( world @ [sphere1; sphere2; sphere3] )


let world = Hittable.(
    let material_ground = make_lambertian 0.8 0.8 0.0 in
    let material_center = make_lambertian 0.1 0.2 0.5 in 
    let material_left = make_dielectric 1.5 in 
    let material_right = make_metal 0.8 0.6 0.2 0.0 in 
    Hit_list( 
      [        
        of_sphere {
          center = Vec3.create 0. (-.100.5) (-.1.0);
          mat = material_ground;
          radius = 100.0
        };        
        of_sphere {
          center = Vec3.create 0. 0. (-.1.0);
          mat = material_center;
          radius = 0.5
        };
        of_sphere {
          center = Vec3.create (-.1.0) 0. (-.1.0);
          mat = material_left;
          radius = 0.5
        };
        of_sphere {
          center = Vec3.create (-.1.0) 0. (-.1.0);
          mat = material_left;
          radius = (-.0.45)
        };
        of_sphere {
          center = Vec3.create 1.0 0.0 (-.1.0);
          mat = material_right;
          radius = 0.5
        }
      ])       
  );;


(* camera *)
let camera =
  let lookfrom = Vec3.create 3. 3. 2. in
  let lookat = Vec3.create 0. 0. (-.1.) in
  let vup = Vec3.create 0. 1. 0. in
  let dist_to_focus = Vec3.length (lookfrom -: lookat) in
  let aperture = 2.0
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
