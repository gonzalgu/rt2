
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
  
    
