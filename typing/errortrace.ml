open Types

type position = First | Second
let swap_position = function
  | First -> Second
  | Second -> First

type desc = { t: type_expr; expanded: type_expr option }
type 'a diff = { got: 'a; expected: 'a}

let short t = { t; expanded = None }
let map_diff f r =
  (* ordering is often meaningful when dealing with type_expr *)
  let got = f r.got in
  let expected = f r.expected in
  { got; expected}

let flatten_desc f x = match x.expanded with
  | None -> f x.t x.t
  | Some expanded -> f x.t expanded

let swap_diff x = { got = x.expected; expected = x.got }

type 'a escape =
  | Constructor of Path.t
  | Univ of type_expr
  (* The type_expr argument of [Univ] is always a [Tunivar _],
     we keep a [type_expr] to track renaming in {!Printtyp} *)
  | Self
  | Module_type of Path.t
  | Equation of 'a
  | Constraint

module Unification = struct
  type variant =
    | No_intersection
    | No_tags of position * (Asttypes.label * row_field) list
    | Incompatible_types_for of string

  type obj =
    | Missing_field of position * string
    | Abstract_row of position
    | Self_cannot_be_closed

  type 'a elt =
    | Diff of 'a diff
    | Variant of variant
    | Obj of obj
    | Escape of {context:type_expr option; kind: 'a escape}
    | Incompatible_fields of {name:string; diff:type_expr diff }
    | Rec_occur of type_expr * type_expr

  type t = desc elt list

  let diff got expected = Diff (map_diff short {got;expected})

  let map_elt f = function
    | Diff x -> Diff (map_diff f x)
    | Escape {kind=Equation x; context} -> Escape {kind=Equation(f x); context}
    | Rec_occur (_,_)
    | Escape {kind=(Univ _ | Self|Constructor _ | Module_type _ |Constraint); _}
    | Variant _ | Obj _
    | Incompatible_fields _ as x -> x
  let map f = List.map (map_elt f)

  (* Convert desc to type_expr * type_expr *)
  let flatten f = map (flatten_desc f)

  (* Permute the expected and actual values *)
  let swap_elt = function
    | Diff x -> Diff (swap_diff x)
    | Incompatible_fields {name;diff} ->
        Incompatible_fields { name; diff = swap_diff diff}
    | Obj (Missing_field(pos,s)) -> Obj(Missing_field(swap_position pos,s))
    | Obj (Abstract_row pos) -> Obj(Abstract_row (swap_position pos))
    | x -> x
  let swap x = List.map swap_elt x

  exception Unify of t

  let rec_occur x y = Unify[Rec_occur(x, y)]
  let incompatible_fields name got expected =
    Incompatible_fields {name; diff={got; expected} }
end

module Equality = struct
    type variant =
      | Incompatible_types_for of string
      | Openness of position
      | Missing of (position * Asttypes.label)

    type obj =
      | Missing_field of position * string
      | Abstract_row of position

    type 'a elt =
      | Diff of 'a diff
      | Variant of variant
      | Obj of obj
      | Escape of {context:type_expr option; kind: 'a escape}
      | Incompatible_fields of {name:string; diff:type_expr diff }

    type t = desc elt list
    let diff got expected = Diff (map_diff short {got;expected})

    let map_elt f = function
      | Diff x -> Diff (map_diff f x)
      | Escape {kind=Equation x; context} -> Escape {kind=Equation(f x); context}
      | Variant _ | Obj _
      | Escape {kind=(Univ _ | Self|Constructor _ |Module_type _|Constraint); _}
      | Incompatible_fields _ as x -> x
    let map f = List.map (map_elt f)

    (* Convert desc to type_expr * type_expr *)
    let flatten f = map (flatten_desc f)

    exception Equality of t

    let incompatible_fields name got expected =
      Incompatible_fields {name; diff={got; expected} }
end

module Moregen = struct
    type variant =
      | Missing of position * Asttypes.label
      | Openness
      | Incompatible_types_for of string

    type obj =
      | Missing_field of position * string
      | Abstract_row of position

    type 'a elt =
      | Diff of 'a diff
      | Variant of variant
      | Obj of obj
      | Escape of {context:type_expr option; kind: 'a escape}
      | Incompatible_fields of {name:string; diff:type_expr diff }
      | Rec_occur of type_expr * type_expr

    type t = desc elt list
    let diff got expected = Diff (map_diff short {got;expected})

    let map_elt f = function
      | Diff x -> Diff (map_diff f x)
      | Escape {kind=Equation x; context} -> Escape {kind=Equation(f x); context}
      | Rec_occur (_,_)
      | Variant _
      | Obj _
      | Escape {kind=(Univ _ | Self|Constructor _ |Module_type _|Constraint); _}
      | Incompatible_fields _ as x -> x
    let map f = List.map (map_elt f)

    let flatten f = map (flatten_desc f)

    exception Moregen of t

    let rec_occur x y = Moregen [Rec_occur(x, y)]
    let incompatible_fields name got expected =
      Incompatible_fields {name; diff={got; expected} }
end

module Subtype = struct
  type 'a elt =
    | Diff of 'a diff

  type t = desc elt list

  let diff got expected = Diff (map_diff short {got;expected})

  let map_elt f = function
    | Diff x -> Diff (map_diff f x)
  let map f = List.map (map_elt f)

  let flatten f = map (flatten_desc f)

  exception Subtype of t * Unification.t
end
