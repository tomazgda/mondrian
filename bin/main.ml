open Mondrian

let () =
  let open Table in

  let _test_table = 
    col [row [cell "a"; cell "b"];
         row [cell "c"; cell "d"]]
  in
  
  print_endline "Done!"
  
