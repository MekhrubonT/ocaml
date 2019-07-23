(* TEST
   * expect
*)

type _ t =
  Int : int -> int t | String : string -> string t | Same : 'l t -> 'l t;;
let rec f = function Int x -> x | Same s -> f s;;
type 'a tt = 'a t =
  Int : int -> int tt | String : string -> string tt | Same : 'l1 t -> 'l2 tt;;

[%%expect{|
type _ t =
    Int : int -> int t
  | String : string -> string t
  | Same : 'l t -> 'l t
val f : int t -> int = <fun>
Line 4, characters 0-97:
4 | type 'a tt = 'a t =
5 |   Int : int -> int tt | String : string -> string tt | Same : 'l1 t -> 'l2 tt..
Error: This variant or record definition does not match that of type 'a t
       Constructors do not match:
         Same : 'l t -> 'l t
       is not compatible with:
         Same : 'l1 t -> 'l2 t
       Type 'l t is not compatible with type 'l1 t
       Type 'l is not compatible with type 'l1
|}];;
