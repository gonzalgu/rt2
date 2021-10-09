open Vec3

let test_create =
  QCheck.(
    Test.make ~count:1000 ~name:"test_create" (triple float float float)
      (fun (x,y,z) ->
         let v = create x y z
         in v.x = x && v.y = y && v.z = z))

let vec_arb =
  let triple_float = QCheck.(triple float float float) in  
  let create3 (x,y,z) = Vec3.create x y z
  in QCheck.map create3 triple_float;;


let test_addition =
  QCheck.(
    Test.make ~count:1000 ~name:"test_addition" (pair vec_arb vec_arb)
      (fun (v1, v2) ->       
         let result = v1 +: v2 in
         let expected = create (v1.x +. v2.x) (v1.y +. v2.y) (v1.z +. v2.z) in
         result = expected));;

let test_neg =
  QCheck.(
    Test.make ~count:1000 ~name:"test_neg" vec_arb
      (fun v ->        
         let result = neg v in
         let expected = create (-. v.x) (-. v.y) (-. v.z) in
         result = expected));;



(* scalar mult *)             
let test_scalar_mult =
  QCheck.(
    Test.make ~count:1000 ~name:"test_scalar_mult" (pair vec_arb float)
      (fun (v, t) ->
         let result = v *$ t in
         let expected = create (v.x *. t) (v.y *. t) (v.z *. t) in
         result = expected));;

let test_div =
  QCheck.(
    Test.make ~count:1000 ~name:"test_div" (pair vec_arb pos_float)
      (fun (v, t) ->
         let result = v /$ t in
         let expected = v *$ (1./.t) in
         result = expected));;

let test_subtraction =
  QCheck.(
    Test.make ~count:1000 ~name:"test_subtraction" (pair vec_arb vec_arb)
      (fun (v1, v2) ->
         let expected = create (v1.x -. v2.x) (v1.y -. v2.y) (v1.z -. v2.z) in
         expected = (v1 -: v2)));;

let test_multiplication =
  QCheck.(
    Test.make ~count:1000 ~name:"test_multiplication" (pair vec_arb vec_arb)
      (fun (v1, v2) ->
         let expected = create (v1.x *. v2.x) (v1.y *. v2.y) (v1.z *. v2.z) in
         expected = (v1 *: v2)));;

let test_dot =
  QCheck.(
    Test.make ~count:1000 ~name:"test_dot" (pair vec_arb vec_arb)
      (fun (v1, v2) ->
         let expected = v1.x *. v2.x +. v1.y *. v2.y +. v1.z *. v2.z
         in expected = dot v1 v2));;
                                                                  

(* Run tests *)
let _ = QCheck_runner.run_tests_main [test_create;
                                          test_addition;
                                          test_neg;
                                      test_scalar_mult;
                                      test_div;
                                      test_subtraction;
                                      test_multiplication;
                                      test_dot
                                     ];;
