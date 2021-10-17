
type t = {
  origin:Vec3.t;
  lower_left_corner:Vec3.t;
  horizontal:Vec3.t;
  vertical:Vec3.t  
}

let degrees_to_radians deg = deg *. Float.pi /. 180.0



let create ~vfov ~aspect_ratio ~focal_length = 
  let open Vec3 in
  let theta = degrees_to_radians vfov in
  let h = Float.tan (theta /. 2.) in
  let viewport_height = 2.0 *. h in 
  let viewport_width = aspect_ratio *. viewport_height in
  let origin = Vec3.create 0. 0. 0. in
  let horizontal = Vec3.create viewport_width 0. 0. in
  let vertical = Vec3.create 0. viewport_height 0. in
  let lower_left_corner = origin -: horizontal /$ 2. -: vertical /$ 2. -: Vec3.create 0. 0. focal_length in
  {
    origin = origin;
    horizontal = horizontal;
    vertical = vertical;
    lower_left_corner = lower_left_corner    
  };;

let get_ray (u:float) (v:float) t : Ray.t =
  let open Vec3 in
  Ray.create t.origin (t.lower_left_corner +: u *| t.horizontal +: v *| t.vertical -: t.origin) 
    
