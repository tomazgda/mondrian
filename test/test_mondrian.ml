(* Template *)

let practice () =
  Alcotest.(check int) "equal int" 5 (2 + 3)

let () =
  let open Alcotest in
  run "Utils" [ ("practice", [ test_case "Practice" `Quick practice ])]
