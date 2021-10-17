
type hit_record = {
  p:Vec3.t;
  normal:Vec3.t;
  mat:material;
  t:float;
  front_face:bool
}
and  material =
  | Lambertian of { albedo : Vec3.t }
  | Metal of { albedo : Vec3.t; fuzz : float }
  | Dielectric of { ir : float };;

let make_lambertian (x :float) (y:float) (z:float): material =
  Lambertian { albedo = Vec3.create x y z }

let make_metal (x:float)(y:float)(z:float)(f:float):material =
  Metal{ albedo = Vec3.create x y z; fuzz = if f < 1. then f else 1. }

let make_dielectric index_of_refraction =
  Dielectric { ir = index_of_refraction }





let empty_hit_rec = {
  p = Vec3.create 0. 0. 0.;
  normal = Vec3.create 0. 0. 0.;
  mat = Lambertian { albedo = Vec3.create 0. 0. 0.};
  t = 0.;
  front_face = false
};;

let set_face_normal (hr:hit_record) (r:Ray.t) (outward_normal:Vec3.t):hit_record =
  let front_face' = Vec3.dot r.direction outward_normal < 0.  in
  let normal'     =
    if front_face'
    then outward_normal
    else Vec3.neg outward_normal
  in  
  { hr with
    front_face = front_face';
    normal = normal'
  };;
  
  
type hittable =
  | Sphere of sphere
  | Hit_list of hittable list
and sphere = { center:Vec3.t; radius:float; mat:material }

let of_sphere (s:sphere):hittable = Sphere s    
;;

let of_list (hl:hittable list):hittable = Hit_list hl
;;

let rec hit (h:hittable) (r:Ray.t) (t_min:float) (t_max:float) (hrec:hit_record) : hit_record option =
  
  let hit_sphere (s:sphere) : hit_record option = 
    let open Vec3 in
    let oc = r.origin -: s.center
    and a = length_squared r.direction in
    let half_b = dot oc r.direction
    and c = (length_squared oc) -. (s.radius *. s.radius) in
    let discriminant = (half_b *. half_b) -. (a *. c) in
    
    if discriminant < 0.
    then None
    else(
      let sqrtd = Float.sqrt discriminant in
      let root0 = ((-. half_b) -. sqrtd) /. a in
      let root1 = ((-. half_b) +. sqrtd) /. a in

      let in_range x =
        t_min <= x && x <= t_max
      in

      [root0;root1]      
      |> List.find_opt in_range
      |> Option.map (fun root ->
          let hrec' = { hrec with t = root; p = Ray.at r root } in
          let outward_normal = (hrec'.p -: s.center) /$ s.radius in          
          let hrec'' = set_face_normal hrec' r outward_normal in
          { hrec'' with mat = s.mat }
        ))            
  in

  let hit_list (hl: hittable list) : hit_record option =
    let result =
      hl
      |> List.to_seq
      |> Seq.fold_left (fun (hit_anything, closest_so_far, tmp_rec) h ->
          match hit h r t_min closest_so_far tmp_rec with
          | None -> (hit_anything, closest_so_far, tmp_rec)
          | Some(tmp_rec') -> (true, tmp_rec'.t, tmp_rec'))
        (false, t_max, hrec)
    in
    match result with
    | (false, _, _) -> None
    | (true, _, hrec') -> Some(hrec')        
  in    
  
  match h with
  | Sphere(s) -> hit_sphere s
  | Hit_list(hl) -> hit_list hl
;;


let reflectance (cosine : float) (ref_idx : float) : float =
  let r0 = (1. -. ref_idx) /. (1. +. ref_idx) in
  let r0' = r0 *. r0 in
  r0' +. (1. -. r0') *. Float.pow (1. -. cosine) 5.


let scatter (r_in:Ray.t) (hr:hit_record) (_:Vec3.t) (_:Ray.t) (mat:material) =
  let open Vec3 in
  match mat with
  | Lambertian { albedo } -> 
    let scatter_direction = hr.normal +: Vec3.random_unit_vector () in
    let scatter_direction' =
      if Vec3.near_zero scatter_direction
      then hr.normal
      else scatter_direction
    in    
    let scattered' = Ray.create hr.p scatter_direction' in
    let attenuation' = albedo in
    Some (attenuation', scattered')
  | Metal { albedo; fuzz } ->
    let reflected = Vec3.reflect (unit_vector r_in.direction) hr.normal in
    let scattered' = Ray.create hr.p (reflected +: fuzz *| random_in_unit_sphere ()) in
    let attenuation' = albedo in
    if dot scattered'.direction hr.normal > 0.
    then Some(attenuation', scattered')
    else None
  | Dielectric { ir } ->
    let attenuation' = Vec3.create 1.0 1.0 1.0 in
    let refraction_ratio =
      if hr.front_face
      then (1.0 /. ir)
      else ir
    in
    let unit_direction = unit_vector r_in.direction in
    let cos_theta = Float.min (dot (neg unit_direction) hr.normal) 1.0 in
    let sin_theta = Float.sqrt(1.0 -. cos_theta *. cos_theta) in
    let cannot_refract = refraction_ratio *. sin_theta > 1.0 in
    let direction =
      if cannot_refract || (reflectance cos_theta refraction_ratio > Rtweekend.random_f ()) 
      then reflect unit_direction hr.normal
      else refract unit_direction hr.normal refraction_ratio
    in 
    let scattered' = Ray.create hr.p direction in
    Some(attenuation', scattered')
;;






(*
module HitRecord
 struct
   let hit_record = {}
   let set_face_normal ... = 
     ...
 end

module type Hittable 
  sig
    type t
    val hit : 

  end

module Sphere : Hittable = 
  struct 

  end

module HitList : Hittable = 
  struct 

  end
 

*)


                       
