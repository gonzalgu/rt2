open Modules.Vec3

let num_tests = 10000;;

let test_create =
  QCheck.(
    Test.make ~count:num_tests ~name:"test_create" (triple float float float)
      (fun (x,y,z) ->
         let v = create x y z
         in v.x = x && v.y = y && v.z = z))

let vec_arb =
  let triple_float = QCheck.Gen.(triple float float float) in  
  let create3 (x,y,z) = create x y z in
  let print v = Printf.sprintf "vec3{x=%F;y=%F;z=%F}" v.x v.y v.z in
  let arb = QCheck.Gen.map create3 triple_float in
  QCheck.make ~print:print arb;;


let test_addition =
  QCheck.(
    Test.make ~count:num_tests ~name:"test_addition" (pair vec_arb vec_arb)
      (fun (v1, v2) ->       
         let result = v1 +: v2 in
         let expected = create (v1.x +. v2.x) (v1.y +. v2.y) (v1.z +. v2.z) in
         result = expected));;

let addition_comm =
  QCheck.(
    Test.make
      ~count:num_tests
      ~name:"addition_comm"
      (pair vec_arb vec_arb)
      (fun (a,b) ->
         let l = a +: b in
         let r = b +: a in
         l = r));;

let test_neg =
  QCheck.(
    Test.make ~count:num_tests ~name:"test_neg" vec_arb
      (fun v ->        
         let result = neg v in
         let expected = create (-. v.x) (-. v.y) (-. v.z) in
         result = expected));;



(* scalar mult *)             
let test_scalar_mult =
  QCheck.(
    Test.make ~count:num_tests ~name:"test_scalar_mult" (pair vec_arb float)
      (fun (v, t) ->
         let result = v *$ t in
         let expected = create (v.x *. t) (v.y *. t) (v.z *. t) in
         result = expected));;

let test_div =
  QCheck.(
    Test.make ~count:num_tests ~name:"test_div" (pair vec_arb pos_float)
      (fun (v, t) ->
         let result = v /$ t in
         let expected = v *$ (1./.t) in
         result = expected));;

let test_subtraction =
  QCheck.(
    Test.make ~count:num_tests ~name:"test_subtraction" (pair vec_arb vec_arb)
      (fun (v1, v2) ->
         let expected = create (v1.x -. v2.x) (v1.y -. v2.y) (v1.z -. v2.z) in
         expected = (v1 -: v2)));;

let test_multiplication =
  QCheck.(
    Test.make ~count:num_tests ~name:"test_multiplication" (pair vec_arb vec_arb)
      (fun (v1, v2) ->
         let expected = create (v1.x *. v2.x) (v1.y *. v2.y) (v1.z *. v2.z) in
         expected = (v1 *: v2)));;

let test_dot =
  QCheck.(
    Test.make ~count:num_tests ~name:"test_dot" (pair vec_arb vec_arb)
      (fun (v1, v2) ->
         let expected = v1.x *. v2.x +. v1.y *. v2.y +. v1.z *. v2.z
         in expected = dot v1 v2));;

let dot_comm =
  QCheck.(
    Test.make ~count:num_tests ~name:"dot_comm" (pair vec_arb vec_arb)
      (fun (a, b) ->
         let ab = dot a b in
         let ba = dot b a in
         ab = ba));;
             
                                                                  
let euclidean_magnitude =
  QCheck.(
    Test.make ~count:num_tests ~name:"euclidean_magnitude" vec_arb
      (fun v ->
         let square x = x *. x in
         let mag = length_squared v in
         mag = (square v.x) +. (square v.y) +. (square v.z)));; 

let cauchy_schwartz =
  QCheck.(
    Test.make ~count:num_tests ~name:"cauchy_schwartz" (pair vec_arb vec_arb)
      (fun (a,b) ->
         let l = dot a b in
         let r = (length a) *. (length b) in
         l <= r));;
             
let triangle_inequality =
  QCheck.(
    Test.make ~count:num_tests ~name:"triangle_inequality" (pair vec_arb vec_arb)
      (fun (a,b) ->
         let l = length (a +: b) in
         let r = length a +. length b in
         l <= r));;

let reverse_triangle_inequality =
  QCheck.(
    Test.make ~count:num_tests ~name:"reverse_triangle_inequality" (pair vec_arb vec_arb)
      (fun (a,b) ->
         let l = length (a -: b) in
         let r = abs_float(length a -. length b) in
         l >= r));;

let cross_anti_comm =
  QCheck.(
    Test.make ~count:num_tests ~name:"cross_anti_comm" (pair vec_arb vec_arb)
      (fun (a,b) ->
         let l = cross a b in
         let r = cross b a |> neg in
         l = r));;

let eq_within a b delta =
  let diff_small x y = Float.abs(x -. y) <= delta in
  diff_small a.x b.x && diff_small a.y b.y && diff_small a.z b.z

(* failing test *)
let scalar_mult_distr_add =
  QCheck.(
    Test.make
      ~count:num_tests
      ~name:"scalar_mult_distr_add"
      (triple vec_arb vec_arb float)
      (fun (a,b,t) ->
         let l = t *| (a +: b) in
         let r = (t *| a) +: (t *| b) in
         eq_within l r 1e-8))
    

let dot_distr_over_add =
  QCheck.(
    Test.make
      ~count:num_tests
      ~name:"dot_distr_over_add"
      (triple vec_arb vec_arb vec_arb)
      (fun (a,b,c) ->
         let lhs = dot (a +: b) c in
         let rhs = (dot a c) +. (dot b c) in
         let diff = Float.abs(lhs -. rhs) in
         diff < (100000. *. Float.epsilon)));;
         

let test_unit_vector =
  QCheck.(
    Test.make
      ~count:num_tests
      ~name:"test_unit_vector"
      vec_arb
      (fun v ->
         let luv = length (unit_vector v) in
         let diff = Float.abs(luv -. 1.0) in
         diff < (3. *. Float.epsilon)));;


(* Run tests *)
let _ = QCheck_runner.run_tests
          ~verbose:true
          [
            test_unit_vector;
            test_create;
            test_addition;
            addition_comm;
            test_neg;
            test_scalar_mult;
            test_div;
            test_subtraction;
            test_multiplication;
            test_dot;
            dot_comm;
            euclidean_magnitude;
            cauchy_schwartz;
            triangle_inequality;
            reverse_triangle_inequality;
            cross_anti_comm;
            scalar_mult_distr_add; 
            dot_distr_over_add;
            test_unit_vector
          ];;


(* sphere tests *)
let test_create =
  let open Modules.Sphere in
  QCheck.(
    Test.make
      ~name:"sphere_create"
      ~count:num_tests
      (pair vec_arb float)
      (fun (c,r) ->
         let s = create c r in
         s.center = c && s.radius = r))
;;

let _ = QCheck_runner.run_tests
          ~verbose:true
          [test_create]
          
