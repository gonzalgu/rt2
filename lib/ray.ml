

type t = {origin: Vec3.t; direction: Vec3.t}

let create origin direction = {origin;direction}

let at r t =
  let open Vec3 in
  r.origin +: (t *| r.direction)
                                
