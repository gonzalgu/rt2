
type t = {x:float; y:float; z:float}

let create x y z = {x;y;z}
let create_with v1 v2 f =
  create (f v1.x v2.x) (f v1.y v2.y) (f v1.z v2.z)

let mapf f v = create (f v.x) (f v.y) (f v.z)

(* vec3 -> vec3 -> vec3 *)
let (+:) v1 v2 =  
  create_with v1 v2 ( +. )

(* vec3 -> vec3 *)
let neg {x;y;z} =
  create (-. x) (-. y) (-. z)

(* vec3 -> float -> vec3 *)
let ( *$ ) {x;y;z} t =
  create (t*.x)(t*.y)(t*.z)

(* ( $* ) : float -> vec3 -> vec3 *)
let ( *| ) t v       = v *$ t

(* vec3 -> float -> vec3 *)
let ( /$ ) v t =
  mapf (fun x -> x *. (1. /. t)) v


(* vec3 -> vec3 -> vec3 *)
let ( -: ) v1 v2 =
  create_with v1 v2 ( -. )
 
let ( *: ) v1 v2 =
  create_with v1 v2 ( *. )
    
let dot v1 v2 =
  v1.x *. v2.x +. v1.y *. v2.y +. v1.z *. v2.z

let length_squared v = dot v v

let length v = v |> length_squared |> Float.sqrt 

let cross v1 v2 =
  create
    (v1.y *. v2.z -. v1.z *. v2.y)
    (v1.z *. v2.x -. v1.x *. v2.z)
    (v1.x *. v2.y -. v1.y *. v2.x)

let unit_vector v = v /$ (length v)
  
let vec3_rand () =
  let open Rtweekend in
  create (random_f()) (random_f()) (random_f())
;;

let vec3_random min max =
  let open Rtweekend in
  create
    (random_float min max)
    (random_float min max)
    (random_float min max)
;;


let random_in_unit_sphere () =  
  let rec loop p =
    if length_squared p >= 1. then
      loop @@ vec3_random (-. 1.) 1.
    else
      p
  in
  loop @@ vec3_random (-. 1.) 1.
;;

let random_unit_vector () =
  unit_vector @@ random_in_unit_sphere ()
;;

let random_in_hemisphere normal =
  let in_unit_sphere = random_in_unit_sphere ()
  in
  if dot in_unit_sphere normal > 0.0
  then in_unit_sphere
  else neg @@ in_unit_sphere
;;

let near_zero v  =
  let s = 1e-8 in
  Float.abs v.x < s && Float.abs v.y < s && Float.abs v.z < s
    
let reflect v n =
  v -: (2. *. dot v n) *| n


let refract (uv : t) (n : t) (etai_over_etat : float) : t =
  let cos_theta = Float.min (dot (neg uv) n) 1.0 in
  let r_out_perp = etai_over_etat *| (uv +: cos_theta *| n) in
  let r_out_parallel = Float.sqrt (Float.abs 1.0 -. length_squared(r_out_perp)) *| n in
  r_out_perp +: (neg r_out_parallel)


let random_in_unit_disk () =
  let create_rand_p () =
    let open Rtweekend in
    create (random_float (-1.) 1.) (random_float (-1.) 1.) 0.
  in 
  let rec aux p =
    if length_squared p >= 1. then
      aux @@ create_rand_p ()
    else
      p
  in
  aux @@ create_rand_p ()


let print_vec (label:string) (v:t) =
  Printf.eprintf "%s=vec3{x=%F;y=%F;z=%F}\n"
    label v.x v.y v.z;;
  
  
  
    


