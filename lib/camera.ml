
type t = {
  origin:Vec3.t;
  lower_left_corner:Vec3.t;
  horizontal:Vec3.t;
  vertical:Vec3.t;
  u:Vec3.t;
  v:Vec3.t;
  w:Vec3.t;
  lens_radius:float             
}

let degrees_to_radians deg = deg *. Float.pi /. 180.0



let create
    ~lookfrom
    ~lookat
    ~vup
    ~vfov
    ~aspect_ratio
    ~aperture
    ~focus_dist = 
  let open Vec3 in
  let theta = degrees_to_radians vfov in
  let h = Float.tan (theta /. 2.) in
  let viewport_height = 2.0 *. h in 
  let viewport_width = aspect_ratio *. viewport_height in
  let w = unit_vector (lookfrom -: lookat) in
  let u = unit_vector (cross vup w) in
  let v = cross w u in 
  let origin = lookfrom in
  let horizontal = (focus_dist *. viewport_width) *| u in
  let vertical = (focus_dist *. viewport_height) *| v in 
  let lower_left_corner = origin -: (horizontal /$ 2.) -: (vertical /$ 2.) -: (focus_dist *| w) in
  let lens_radius = aperture /. 2.0
  in
  {
    origin = origin;
    horizontal = horizontal;
    vertical = vertical;
    lower_left_corner = lower_left_corner;
    u = u;
    v = v;
    w = w;
    lens_radius = lens_radius
  };;

let get_ray (s:float) (t:float) r : Ray.t =
  let open Vec3 in
  let rd = r.lens_radius *| random_in_unit_disk () in
  let offset = r.u *$ rd.x +: r.v *$ rd.y
  in
  Ray.create
    (r.origin +: offset)
    (r.lower_left_corner +: (s *| r.horizontal) +: (t *| r.vertical) -: r.origin -: offset) 
    
