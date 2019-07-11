(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Inclusion checks for the core language *)

open Typedtree
open Types

exception Dont_match

type label_mismatch =
    LType
  | LMutable

type record_mismatch =
  | Label_type of label_declaration * label_declaration * label_mismatch
  | Field_names of int * Ident.t * Ident.t
  | Field_missing of bool * Ident.t
  | Record_representation of bool   (* true means second one is unboxed float *)

type constructor_mismatch =
    CType
  | CArity
  | CRecord of record_mismatch
  | CKind
  | CExplicit_return_type

type variant_mismatch =
    Constructor_type of constructor_declaration * constructor_declaration * constructor_mismatch
  | Constructor_names of int * Ident.t * Ident.t
  | Constructor_missing of bool * Ident.t

type extension_constructor_mismatch =
    EPrivacy
  | EType of Ident.t *
           extension_constructor *
           extension_constructor *
           constructor_mismatch


type type_mismatch =
    Arity
  | Privacy
  | Kind
  | Constraint
  | Manifest
  | Variance
  | Record_error of record_mismatch
  | Constructor_error of variant_mismatch
  | Extension_constructor_error of extension_constructor_mismatch
  | Unboxed_representation of bool  (* true means second one is unboxed *)
  | Immediate of immediacy * immediacy

val value_descriptions:
  loc:Location.t -> Env.t -> string ->
  value_description -> value_description -> module_coercion

val type_declarations:
  ?equality:bool ->
  loc:Location.t ->
  Env.t -> mark:bool -> string ->
  type_declaration -> Path.t -> type_declaration -> type_mismatch option

val extension_constructors:
  loc:Location.t -> Env.t -> mark:bool -> Ident.t ->
  extension_constructor -> extension_constructor -> type_mismatch option
(*
val class_types:
        Env.t -> class_type -> class_type -> bool
*)

val report_type_mismatch:
    string -> string -> string -> Format.formatter -> type_mismatch -> unit
