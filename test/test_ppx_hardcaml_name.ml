open! Import

let%expect_test "Is hardcaml linked?" =
  if not !Ppx_hardcaml_runtime0.hardcaml_is_linked
  then failwith "ppx_hardcaml_runtime should link Hardcaml!"
;;

let iarray_of_list = Iarray.of_list

(* The naming PPX explicitly refers to Hardcaml in its expansion. Provide stubs for the
   functions and types it uses. *)
module Hardcaml = struct
  module Signal = struct
    type t = Dummy_signal [@@deriving sexp]

    let __ppx_auto_name thing_to_name prefix =
      Core.print_s
        [%message "Signal.__ppx_auto_name called" (prefix : string) (thing_to_name : t)];
      thing_to_name
    ;;
  end

  module Scope = struct
    type t = Dummy_scope [@@deriving sexp]

    let name scope name_for_signal =
      Core.print_s
        [%message "Hardcaml.Scope.name called" (scope : t) (name_for_signal : string)];
      name_for_signal
    ;;
  end

  module Always = struct
    module Variable = struct
      type t = { value : Signal.t } [@@deriving sexp]

      let __ppx_auto_name thing_to_name prefix =
        Core.print_s
          [%message
            "Variable.__ppx_auto_name called" (prefix : string) (thing_to_name : t)];
        thing_to_name
      ;;
    end
  end
end

(* By default we expect [Hardcaml] to be opened when using the naming ppx. *)
open Hardcaml

module Test_type = struct
  type t = { test_signal : Hardcaml.Signal.t } [@@deriving sexp]

  let __ppx_auto_name thing_to_name prefix =
    Core.print_s
      [%message "Test_type.__ppx_auto_name called" (prefix : string) (thing_to_name : t)];
    thing_to_name
  ;;
end

let%expect_test "naming a signal" =
  let scope = Hardcaml.Scope.Dummy_scope in
  let%hw _use_this_name_for_the_signal = Hardcaml.Signal.Dummy_signal in
  [%expect
    {|
    ("Hardcaml.Scope.name called" (scope Dummy_scope)
     (name_for_signal _use_this_name_for_the_signal))
    ("Signal.__ppx_auto_name called" (prefix _use_this_name_for_the_signal)
     (thing_to_name Dummy_signal))
    |}]
;;

let%expect_test "naming a type" =
  let scope = Hardcaml.Scope.Dummy_scope in
  let%hw.Test_type _use_this_name_for_the_signal_in_the_type =
    { Test_type.test_signal = Dummy_signal }
  in
  [%expect
    {|
    ("Hardcaml.Scope.name called" (scope Dummy_scope)
     (name_for_signal _use_this_name_for_the_signal_in_the_type))
    ("Test_type.__ppx_auto_name called"
     (prefix _use_this_name_for_the_signal_in_the_type)
     (thing_to_name ((test_signal Dummy_signal))))
    |}]
;;

let%expect_test "naming a variable" =
  let scope = Hardcaml.Scope.Dummy_scope in
  let%hw_var _use_this_name_for_the_signal_in_the_var =
    { Hardcaml.Always.Variable.value = Dummy_signal }
  in
  [%expect
    {|
    ("Hardcaml.Scope.name called" (scope Dummy_scope)
     (name_for_signal _use_this_name_for_the_signal_in_the_var))
    ("Variable.__ppx_auto_name called"
     (prefix _use_this_name_for_the_signal_in_the_var)
     (thing_to_name ((value Dummy_signal))))
    |}]
;;

let%expect_test "naming a list of signals" =
  let scope = Hardcaml.Scope.Dummy_scope in
  let%hw_list mylist = Hardcaml.Signal.[ Dummy_signal; Dummy_signal ] in
  let%hw_list.Test_type mylist_records =
    [ { Test_type.test_signal = Dummy_signal }; { Test_type.test_signal = Dummy_signal } ]
  in
  (* Check variables are accessible after renaming *)
  ignore (mylist : Hardcaml.Signal.t list);
  ignore (mylist_records : Test_type.t list);
  ();
  [%expect
    {|
    ("Hardcaml.Scope.name called" (scope Dummy_scope) (name_for_signal mylist$0))
    ("Signal.__ppx_auto_name called" (prefix mylist$0)
     (thing_to_name Dummy_signal))
    ("Hardcaml.Scope.name called" (scope Dummy_scope) (name_for_signal mylist$1))
    ("Signal.__ppx_auto_name called" (prefix mylist$1)
     (thing_to_name Dummy_signal))
    ("Hardcaml.Scope.name called" (scope Dummy_scope)
     (name_for_signal mylist_records$0))
    ("Test_type.__ppx_auto_name called" (prefix mylist_records$0)
     (thing_to_name ((test_signal Dummy_signal))))
    ("Hardcaml.Scope.name called" (scope Dummy_scope)
     (name_for_signal mylist_records$1))
    ("Test_type.__ppx_auto_name called" (prefix mylist_records$1)
     (thing_to_name ((test_signal Dummy_signal))))
    |}]
;;

let%expect_test "naming an array of signals" =
  let scope = Hardcaml.Scope.Dummy_scope in
  let%hw_array mylist = Hardcaml.Signal.[| Dummy_signal; Dummy_signal |] in
  let%hw_array.Test_type mylist_records =
    [| { Test_type.test_signal = Dummy_signal }
     ; { Test_type.test_signal = Dummy_signal }
    |]
  in
  (* Check variables are accessible after renaming *)
  ignore (mylist : Hardcaml.Signal.t array);
  ignore (mylist_records : Test_type.t array);
  ();
  [%expect
    {|
    ("Hardcaml.Scope.name called" (scope Dummy_scope) (name_for_signal mylist$0))
    ("Signal.__ppx_auto_name called" (prefix mylist$0)
     (thing_to_name Dummy_signal))
    ("Hardcaml.Scope.name called" (scope Dummy_scope) (name_for_signal mylist$1))
    ("Signal.__ppx_auto_name called" (prefix mylist$1)
     (thing_to_name Dummy_signal))
    ("Hardcaml.Scope.name called" (scope Dummy_scope)
     (name_for_signal mylist_records$0))
    ("Test_type.__ppx_auto_name called" (prefix mylist_records$0)
     (thing_to_name ((test_signal Dummy_signal))))
    ("Hardcaml.Scope.name called" (scope Dummy_scope)
     (name_for_signal mylist_records$1))
    ("Test_type.__ppx_auto_name called" (prefix mylist_records$1)
     (thing_to_name ((test_signal Dummy_signal))))
    |}]
;;

let%expect_test "naming an iarray of signals" =
  let scope = Hardcaml.Scope.Dummy_scope in
  let%hw_iarray mylist = iarray_of_list Hardcaml.Signal.[ Dummy_signal; Dummy_signal ] in
  let%hw_iarray.Test_type mylist_records =
    [ { Test_type.test_signal = Dummy_signal }; { Test_type.test_signal = Dummy_signal } ]
    |> iarray_of_list
  in
  (* Check variables are accessible after renaming *)
  ignore (mylist : Hardcaml.Signal.t iarray);
  ignore (mylist_records : Test_type.t iarray);
  ();
  [%expect
    {|
    ("Hardcaml.Scope.name called" (scope Dummy_scope) (name_for_signal mylist$0))
    ("Signal.__ppx_auto_name called" (prefix mylist$0)
     (thing_to_name Dummy_signal))
    ("Hardcaml.Scope.name called" (scope Dummy_scope) (name_for_signal mylist$1))
    ("Signal.__ppx_auto_name called" (prefix mylist$1)
     (thing_to_name Dummy_signal))
    ("Hardcaml.Scope.name called" (scope Dummy_scope)
     (name_for_signal mylist_records$0))
    ("Test_type.__ppx_auto_name called" (prefix mylist_records$0)
     (thing_to_name ((test_signal Dummy_signal))))
    ("Hardcaml.Scope.name called" (scope Dummy_scope)
     (name_for_signal mylist_records$1))
    ("Test_type.__ppx_auto_name called" (prefix mylist_records$1)
     (thing_to_name ((test_signal Dummy_signal))))
    |}]
;;

let%expect_test "naming a list/array of always variables" =
  let scope = Hardcaml.Scope.Dummy_scope in
  let%hw_var_list mylist = [ { value = Dummy_signal }; { value = Dummy_signal } ] in
  let%hw_var_array myarray = [| { value = Dummy_signal }; { value = Dummy_signal } |] in
  let%hw_var_iarray myiarray =
    [ { Hardcaml.Always.Variable.value = Dummy_signal }
    ; { Hardcaml.Always.Variable.value = Dummy_signal }
    ]
    |> iarray_of_list
  in
  (* Check variables are accessible after renaming *)
  ignore (mylist : Hardcaml.Always.Variable.t list);
  ignore (myarray : Hardcaml.Always.Variable.t array);
  ignore (myiarray : Hardcaml.Always.Variable.t iarray);
  ();
  [%expect
    {|
    ("Hardcaml.Scope.name called" (scope Dummy_scope) (name_for_signal mylist$0))
    ("Variable.__ppx_auto_name called" (prefix mylist$0)
     (thing_to_name ((value Dummy_signal))))
    ("Hardcaml.Scope.name called" (scope Dummy_scope) (name_for_signal mylist$1))
    ("Variable.__ppx_auto_name called" (prefix mylist$1)
     (thing_to_name ((value Dummy_signal))))
    ("Hardcaml.Scope.name called" (scope Dummy_scope)
     (name_for_signal myarray$0))
    ("Variable.__ppx_auto_name called" (prefix myarray$0)
     (thing_to_name ((value Dummy_signal))))
    ("Hardcaml.Scope.name called" (scope Dummy_scope)
     (name_for_signal myarray$1))
    ("Variable.__ppx_auto_name called" (prefix myarray$1)
     (thing_to_name ((value Dummy_signal))))
    ("Hardcaml.Scope.name called" (scope Dummy_scope)
     (name_for_signal myiarray$0))
    ("Variable.__ppx_auto_name called" (prefix myiarray$0)
     (thing_to_name ((value Dummy_signal))))
    ("Hardcaml.Scope.name called" (scope Dummy_scope)
     (name_for_signal myiarray$1))
    ("Variable.__ppx_auto_name called" (prefix myiarray$1)
     (thing_to_name ((value Dummy_signal))))
    |}]
;;

let%expect_test "naming a tuple, rhs constructs a tuple" =
  let scope = Hardcaml.Scope.Dummy_scope in
  let%hw a, b = Dummy_signal, Dummy_signal in
  (* Check variables are accessible after renaming *)
  ignore (a : Hardcaml.Signal.t);
  ignore (b : Hardcaml.Signal.t);
  ();
  [%expect
    {|
    ("Hardcaml.Scope.name called" (scope Dummy_scope) (name_for_signal b))
    ("Signal.__ppx_auto_name called" (prefix b) (thing_to_name Dummy_signal))
    ("Hardcaml.Scope.name called" (scope Dummy_scope) (name_for_signal a))
    ("Signal.__ppx_auto_name called" (prefix a) (thing_to_name Dummy_signal))
    |}]
;;

let%expect_test "naming a tuple, rhs is an expr that evaluates to a tuple" =
  let scope = Hardcaml.Scope.Dummy_scope in
  let tuple = Hardcaml.Signal.Dummy_signal, Hardcaml.Signal.Dummy_signal in
  let%hw a, b = tuple in
  (* Check variables are accessible after renaming *)
  ignore (a : Hardcaml.Signal.t);
  ignore (b : Hardcaml.Signal.t);
  ();
  [%expect
    {|
    ("Hardcaml.Scope.name called" (scope Dummy_scope) (name_for_signal b))
    ("Signal.__ppx_auto_name called" (prefix b) (thing_to_name Dummy_signal))
    ("Hardcaml.Scope.name called" (scope Dummy_scope) (name_for_signal a))
    ("Signal.__ppx_auto_name called" (prefix a) (thing_to_name Dummy_signal))
    |}]
;;

let%expect_test "naming a tuple of interfaces" =
  let scope = Hardcaml.Scope.Dummy_scope in
  let%hw.Test_type a, b =
    { Test_type.test_signal = Dummy_signal }, { Test_type.test_signal = Dummy_signal }
  in
  (* Check variables are accessible after renaming *)
  ignore (a : Test_type.t);
  ignore (b : Test_type.t);
  ();
  [%expect
    {|
    ("Hardcaml.Scope.name called" (scope Dummy_scope) (name_for_signal b))
    ("Test_type.__ppx_auto_name called" (prefix b)
     (thing_to_name ((test_signal Dummy_signal))))
    ("Hardcaml.Scope.name called" (scope Dummy_scope) (name_for_signal a))
    ("Test_type.__ppx_auto_name called" (prefix a)
     (thing_to_name ((test_signal Dummy_signal))))
    |}]
;;

let%expect_test "naming with type annotation" =
  let scope = Hardcaml.Scope.Dummy_scope in
  let%hw (a : Hardcaml.Signal.t) = Dummy_signal in
  let%hw.Test_type (b : Test_type.t) = { Test_type.test_signal = Dummy_signal } in
  (* Check variables are accessible after renaming *)
  ignore (a : Hardcaml.Signal.t);
  ignore (b : Test_type.t);
  ();
  [%expect
    {|
    ("Hardcaml.Scope.name called" (scope Dummy_scope) (name_for_signal a))
    ("Signal.__ppx_auto_name called" (prefix a) (thing_to_name Dummy_signal))
    ("Hardcaml.Scope.name called" (scope Dummy_scope) (name_for_signal b))
    ("Test_type.__ppx_auto_name called" (prefix b)
     (thing_to_name ((test_signal Dummy_signal))))
    |}]
;;
