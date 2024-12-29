(* Mondrian - A library for creating and displaying tables *)

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

module Table : sig
  type t
  (* there are three ways to create a table *)

  (** cell - create a table consiting of a single cell of value v *)
  val cell : ?rowspan:int -> ?colspan:int -> string -> t

  (** row - create a table from multiple rowwise coherent tables *)
  val row : t list -> t

  (** col - create a table from multiple colwise coherent tables *)
  val col : t list -> t
end
