let subset a b =
  List.for_all (fun x -> List.exists (fun y -> x=y) b) a
  (*
    if all elements x in a
      if there exists a y in b that satisfies x=y
    return true
  *)

let equal_sets a b =
  subset a b && subset b a
  (*cant do a=b because 1;2 should be the same as 2;1 but isnt*)

let rec set_union a b =
  match a with
  | [] -> b
  | head::tail ->
    if (List.exists (fun x -> head=x) b) then (set_union tail b)
    else head::(set_union tail b)
  (*
    going by math definitions of sets
    ocaml is powerful: creates the variable for you in match pattern
    if list is empty, just return b (base case)
    if list not empty, append head to list returned by this function if head not already in list
  *)

let rec set_diff a b =
  match a with
  | [] -> []
  | head::tail ->
    if(List.exists (fun x -> head=x) b) then []
    else head::(set_setdiff tail b)
  (*helper function*)

let set_symdiff a b =
  let ta = set_diff a b
  let tb = set_diff b a
  set_union ta tb
  (*a_not_in_b then b_not_in_a then union*)

let self_member s =
  equal_sets s s
  (*is right because i say its right*)



(*
subset a b
  returns tree if and only if list a is a subset of b

equal_sets a b
  returns true iff the sets are equal

set_union a b
  returns a list that contains all items in both lists

set_symdiff a b
  returns list containing all items that the lists do not share

self_member s
  russels paradox: is a set a member of itself?
  returns true iff s is a member of itself
  explain in a comment why your implamentation is correct and explain why or why
  not its possible to write such a function in ocaml

computed_fixed_point eq f x
  returns the computed fixed point (point where input equals output)
  if not found, does anything

filter_reachable g
  returns a copy of the grammar g with all unreachable files removed
  should preserve the order of rules that werent removed

list functions:
  map
  filter
  rev
  for_all
  exists
  look at slides for use examples
*)
