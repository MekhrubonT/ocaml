(* TEST
 * expect
*)

module M1 : sig
  type t = {f0 : unit * unit * unit * int * unit * unit * unit;
            f1 : unit * unit * unit * int * unit * unit * unit}
end = struct
  type t = {f0 : unit * unit * unit * float* unit * unit * unit;
            f1 : unit * unit * unit * string * unit * unit * unit}
end;;
[%%expect{|
Line 4, characters 6-148:
4 | ......struct
5 |   type t = {f0 : unit * unit * unit * float* unit * unit * unit;
6 |             f1 : unit * unit * unit * string * unit * unit * unit}
7 | end..
Error: Signature mismatch:
       Modules do not match:
         sig
           type t = {
             f0 : unit * unit * unit * float * unit * unit * unit;
             f1 : unit * unit * unit * string * unit * unit * unit;
           }
         end
       is not included in
         sig
           type t = {
             f0 : unit * unit * unit * int * unit * unit * unit;
             f1 : unit * unit * unit * int * unit * unit * unit;
           }
         end
       Type declarations do not match:
         type t = {
           f0 : unit * unit * unit * float * unit * unit * unit;
           f1 : unit * unit * unit * string * unit * unit * unit;
         }
       is not included in
         type t = {
           f0 : unit * unit * unit * int * unit * unit * unit;
           f1 : unit * unit * unit * int * unit * unit * unit;
         }
       Fields do not match:
         f0 : unit * unit * unit * float * unit * unit * unit;
       is not equal to:
         f0 : unit * unit * unit * int * unit * unit * unit;
       Type unit * unit * unit * float * unit * unit * unit
       is not equal to type unit * unit * unit * int * unit * unit * unit
       Type float is not equal to type int
|}];;


module M2 : sig
  type t = {mutable f0 : unit * unit * unit * int * unit * unit * unit;
            f1 : unit * unit * unit * int * unit * unit * unit}
end = struct
  type t = {f0 : unit * unit * unit * float* unit * unit * unit;
            f1 : unit * unit * unit * string * unit * unit * unit}
end;;
[%%expect{|
Line 4, characters 6-148:
4 | ......struct
5 |   type t = {f0 : unit * unit * unit * float* unit * unit * unit;
6 |             f1 : unit * unit * unit * string * unit * unit * unit}
7 | end..
Error: Signature mismatch:
       Modules do not match:
         sig
           type t = {
             f0 : unit * unit * unit * float * unit * unit * unit;
             f1 : unit * unit * unit * string * unit * unit * unit;
           }
         end
       is not included in
         sig
           type t = {
             mutable f0 : unit * unit * unit * int * unit * unit * unit;
             f1 : unit * unit * unit * int * unit * unit * unit;
           }
         end
       Type declarations do not match:
         type t = {
           f0 : unit * unit * unit * float * unit * unit * unit;
           f1 : unit * unit * unit * string * unit * unit * unit;
         }
       is not included in
         type t = {
           mutable f0 : unit * unit * unit * int * unit * unit * unit;
           f1 : unit * unit * unit * int * unit * unit * unit;
         }
       Fields do not match:
         f0 : unit * unit * unit * float * unit * unit * unit;
       is not equal to:
         mutable f0 : unit * unit * unit * int * unit * unit * unit;
       The second is mutable and the first is not.
|}];;

module M3 : sig
  type t = {f0 : unit}
end = struct
  type t = {f1 : unit}
end;;
[%%expect{|
Line 3, characters 6-39:
3 | ......struct
4 |   type t = {f1 : unit}
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = { f1 : unit; } end
       is not included in
         sig type t = { f0 : unit; } end
       Type declarations do not match:
         type t = { f1 : unit; }
       is not included in
         type t = { f0 : unit; }
       1st fields have different names, f1 and f0.
|}];;

module M4 : sig
  type t = {f0 : unit; f1 : unit}
end = struct
  type t = {f0 : unit}
end;;
[%%expect{|
Line 3, characters 6-39:
3 | ......struct
4 |   type t = {f0 : unit}
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = { f0 : unit; } end
       is not included in
         sig type t = { f0 : unit; f1 : unit; } end
       Type declarations do not match:
         type t = { f0 : unit; }
       is not included in
         type t = { f0 : unit; f1 : unit; }
       The field f1 is only present in the second declaration.
|}];;
