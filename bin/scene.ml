open Modules

let rec range a b =
  if a > b
  then Seq.empty
  else Seq.cons a (range (a+1) b)

let ( -- ) a b = range a b

let cartesian s1 s2 =
  s1 |> Seq.flat_map (fun x ->
      s2 |> Seq.map (fun y -> (x,y)))
                         
let create_sphere  choose_mat center =
    let open Vec3 in 
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


let create_rand_sphere (a, b) =
    let open Vec3 in
    let choose_mat = Rtweekend.random_f () in
    let x = Float.of_int a +. 0.9 *. Rtweekend.random_f () in
    let y = 0.2 in
    let z = Float.of_int b +. 0.9 *. Rtweekend.random_f () in      
    let center = Vec3.create x y z in
    if Vec3.length (center -: Vec3.create 4. 0.2 0.) > 0.9
    then
      let sphere = create_sphere choose_mat center in
      Some(sphere)
    else
      None


let random_scene () =
  let open Hittable in
  let ground_material = make_lambertian 0.5 0.5 0.5 in
  let ground_sphere = Hittable.of_sphere {
      center = Vec3.create 0. (-.1000.0) 0.;
      radius = 1000.0;
      mat = ground_material      
    }
  in
  let s = cartesian (-11 -- 10) (-11 -- 10) in    
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
  let additional = [sphere1; sphere2; sphere3] in
  Hittable.Hit_list( ground_sphere :: world @ additional )

  let testWorld () = 
    let open Hittable in
    let material_ground = make_lambertian 0.8 0.8 0.0 in
    let material_center = make_lambertian 0.1 0.2 0.5 in 
    let material_left   = make_dielectric 1.5 in 
    let material_right  = make_metal 0.8 0.6 0.2 0.0 in
    Hit_list  
        [
            (* ground *)
            of_sphere {
                center = Vec3.create 0.0 (-.100.5) (-.1.0);
                radius = 100.0;
                mat = material_ground
            };

            of_sphere {
                center = Vec3.create (3.5) 0.0 (2.0);
                radius = 0.5;
                mat = material_center
            };
            (* glass *)
            of_sphere {
                center = Vec3.create (5.0) (0.0) (1.0);
                radius = 0.5;
                mat = material_left
            };
            of_sphere {
                center = Vec3.create (5.0) (0.0) (1.0);
                radius = (-.0.45);
                mat = material_left
            };
            of_sphere {
                center = Vec3.create 6.0 (0.0) (0.0);
                radius = 0.5;
                mat = material_right
            }
        ]

