
(* image *)
let image_width = 256 
and image_height = 256
in
(* render *)
Printf.printf "P3\n%d %d\n255\n" image_width image_height;
for j = (image_height-1) downto 0 do
  begin
    Printf.eprintf "\rScanlines remaining: %d\n" j;
    for i = 0 to (image_width-1) do
      let r = float_of_int i /. float_of_int (image_width-1)
      and g = float_of_int j /. float_of_int (image_height-1)
      and b = 0.25
      and conv x = int_of_float (255.999 *. x)        
      in
      let (ir,ig,ib) = (conv r, conv g, conv b) in
      Printf.printf "%d %d %d\n" ir ig ib
    done
  end
done;
Printf.eprintf "\nDone\n"
