(* Lets start with trees *)

type table = 
  {
    tiling : cell list;
    row_shape : shape;
    col_shape : shape
  }

and cell =
  {
    index : table_index;
    value : string
  }

and table_index = int list * int list

and shape =
  | Leaf
  | Node of int * shape list

(* ------------------------------------------------------------------------------------ *)

(* let rec compare_shape s1 s2 = *)
(*   match s1, s2 with *)
(*   | Leaf, Leaf -> 0 *)
(*   | Leaf, _ -> -1 *)
(*   | _ , Leaf -> 1 *)
(*   | Node (_, t1), Node (_, t2) -> compare_shape t1 t2 *)

(* is a tree a prefix of another - less than or equal to *)
let rec shape_leq s1 s2 =
  match s1, s2 with
  | Leaf, _  -> true
  | _ , Leaf -> false
  | Node (_, trees1), Node (_, trees2) ->
     List.for_all2 shape_leq trees1 trees2

(* find the largest in a list of shapes *)
let shape_max shapes =
  let larger_shape s1 s2 =
    match shape_leq s1 s2 with
    | true -> s2
    | false -> s1
  in
  List.fold_left larger_shape Leaf shapes

let reindex_cells_of_tables f tables =
  let reindex_cells_of_table n t = List.map (fun c -> f c n) t.tiling in
  List.mapi reindex_cells_of_table tables |> List.concat

(* ------------------------------------------------------------------------------------ *)

module Table = struct

  type t = table
  
  let cell ?(rowspan = 0) ?(colspan = 0) value =
    let shape_span n = Node (n, List.init n (fun _ -> Leaf)) in
    let row_shape = shape_span rowspan in
    let col_shape = shape_span colspan in
    let tiling =
      let index = ([],[]) in
      [{index;value}]
    in
    {row_shape;col_shape;tiling}

  let row tables =
    let row_shape = List.map (fun t -> t.row_shape) tables |> shape_max in
    let col_shape =
      let col_shapes = List.map (fun t -> t.col_shape) tables in
      let label = List.length col_shapes in
      Node (label, col_shapes)
    in
    let tiling =
      let indexifier cell n =
        let (row,col) = cell.index in
        { cell with index = (row, n :: col)}
      in
      reindex_cells_of_tables indexifier tables
    in
    {row_shape;col_shape;tiling}

  let col tables =
    let row_shape =
      let row_shapes = List.map (fun t -> t.row_shape) tables in
      let label = List.length row_shapes in
      Node (label, row_shapes)
    in
    let col_shape = List.map (fun t -> t.row_shape) tables |> shape_max in
    let tiling =
      let indexifier cell n =
        let (row,col) = cell.index in
        { cell with index = (n :: row, col)}
      in
      reindex_cells_of_tables indexifier tables
    in
    {row_shape;col_shape;tiling}
end

