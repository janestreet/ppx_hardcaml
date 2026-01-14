open! Import
open! Hardcaml

let%expect_test "Is hardcaml linked?" =
  if not !Ppx_hardcaml_runtime0.hardcaml_is_linked
  then failwith "ppx_hardcaml_runtime should link Hardcaml!"
;;

module%test Top_level_functions = struct
  let%subscope my_function _arg1 scope _arg2 =
    let%hw result = Signal.of_int_trunc ~width:8 10 in
    result
  ;;

  let%subscope my_outer_function arg1 scope arg2 = my_function arg1 scope arg2

  let%expect_test "top-level function" =
    let scope = Scope.create ~naming_scheme:Full_path () in
    let result = my_function 10 scope 20 in
    print_s [%message (Signal.names result : string list)];
    [%expect {| ("Signal.names result" (my_function$result)) |}]
  ;;

  let%expect_test "calling function twice" =
    let scope = Scope.create ~naming_scheme:Full_path () in
    let result0 = my_function 10 scope 20 in
    print_s [%message (Signal.names result0 : string list)];
    [%expect {| ("Signal.names result0" (my_function$result)) |}];
    let result1 = my_function 10 scope 20 in
    (* Since the my_function subscope was already created, the next result gets a
       my_function_1 subscope. *)
    print_s [%message (Signal.names result1 : string list)];
    [%expect {| ("Signal.names result1" (my_function_1$result)) |}]
  ;;

  let%expect_test "nested top-level functions" =
    let scope = Scope.create ~naming_scheme:Full_path () in
    let result = my_outer_function 10 scope 20 in
    print_s [%message (Signal.names result : string list)];
    [%expect {| ("Signal.names result" (my_outer_function$my_function$result)) |}]
  ;;

  let%subscope typed_and_name_arguments (_arg1 : int) (scope : Scope.t) ~arg2:_ =
    let%hw result = Signal.of_int_trunc ~width:8 10 in
    result
  ;;

  let%expect_test "with type annotations and named arguments" =
    let scope = Scope.create ~naming_scheme:Full_path () in
    let result = typed_and_name_arguments 5 scope ~arg2:10 in
    print_s [%message (Signal.names result : string list)];
    [%expect {| ("Signal.names result" (typed_and_name_arguments$result)) |}]
  ;;
end

module%test Inline_functions = struct
  let%expect_test "inline function captures scope from context" =
    let scope = Scope.create ~naming_scheme:Full_path () in
    let scope = Scope.sub_scope scope "outer" in
    let%subscope helper x =
      let%hw result = Signal.of_int_trunc ~width:8 x in
      result
    in
    let result = helper 10 in
    print_s [%message (Signal.names result : string list)];
    [%expect {| ("Signal.names result" (outer$helper$result)) |}]
  ;;

  let%expect_test "inline function captures scope from passed scope value" =
    let scope = Scope.create ~naming_scheme:Full_path () in
    let%subscope helper scope ~x =
      let%hw result = Signal.of_int_trunc ~width:8 x in
      result
    in
    let result = helper (Scope.sub_scope scope "passed") ~x:10 in
    print_s [%message (Signal.names result : string list)];
    [%expect {| ("Signal.names result" (passed$helper$result)) |}]
  ;;

  let%subscope my_function _arg1 scope _arg2 =
    let%subscope helper () =
      let%hw result = Signal.of_int_trunc ~width:8 10 in
      result
    in
    helper ()
  ;;

  let%expect_test "inline function in top-level function" =
    let scope = Scope.create ~naming_scheme:Full_path () in
    let result = my_function 10 scope 20 in
    print_s [%message (Signal.names result : string list)];
    [%expect {| ("Signal.names result" (my_function$helper$result)) |}]
  ;;
end

module%test Cases_functions = struct
  let%subscope my_function _arg1 scope _arg2 = function
    | `a ->
      let%hw result_a = Signal.of_int_trunc ~width:8 10 in
      result_a
    | `b ->
      let%hw result_b = Signal.of_int_trunc ~width:8 10 in
      result_b
  ;;

  let%expect_test "top-level function" =
    let scope = Scope.create ~naming_scheme:Full_path () in
    let result = my_function 10 scope 20 `a in
    print_s [%message (Signal.names result : string list)];
    [%expect {| ("Signal.names result" (my_function$result_a)) |}]
  ;;

  let%expect_test "inline function" =
    let scope = Scope.create ~naming_scheme:Full_path () in
    let scope = Scope.sub_scope scope "outer" in
    let%subscope helper _arg1 _arg2 = function
      | `a ->
        let%hw result_a = Signal.of_int_trunc ~width:8 10 in
        result_a
      | `b ->
        let%hw result_b = Signal.of_int_trunc ~width:8 10 in
        result_b
    in
    let result = helper 10 20 `b in
    print_s [%message (Signal.names result : string list)];
    [%expect {| ("Signal.names result" (outer$helper$result_b)) |}]
  ;;
end
