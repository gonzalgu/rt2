
type hit_record = { p:Vec3.t; normal:Vec3.t; t:float; front_face:bool}

let empty_hit_rec = {
  p = Vec3.create 0. 0. 0.;
  normal = Vec3.create 0. 0. 0.;
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
and sphere = { center:Vec3.t; radius:float }

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
          set_face_normal hrec' r outward_normal
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

                       
