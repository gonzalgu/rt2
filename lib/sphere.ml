

type t = {center:Vec3.t; radius:float}

let create center radius = {center;radius}

type hit_record = { point:Vec3.t; normal:Vec3.t; t:float}
                 
let hit (sph:t) (r:Ray.t) (t_min:float) (t_max:float) (record:hit_record): hit_record option =
  let open Vec3 in
  let oc = r.origin -: sph.center in
  let a = length_squared r.direction in
  let half_b = dot oc r.direction in
  let c = length_squared oc -. sph.radius *. sph.radius in
  let discriminant = half_b *. half_b -. a *. c in
  
  if discriminant < 0.
  then None
  else
    let sqrtd = Float.sqrt discriminant in
    let root0 = ((-. half_b) -. sqrtd) /. a in
    let root1 = ((-. half_b) +. sqrtd) /. a in
  
    let in_range root min max =
      not (root < min || max < root)
    in

    match List.filter (fun e -> in_range e t_min t_max) [root0;root1] with
    | [] -> None
    | root::_ ->
      let result = {
        point = Ray.at r record.t;
        normal = record.point -: sph.center /$ sph.radius;
        t = root
      }
      in Some result      
;;      
      
