open! Import

let print_t_list t =
  List.iter t ~f:(fun (n, b) -> Printf.printf "%s %i\n" n b);
  printf "\n"
;;

module Simple = struct
  type 'a t = { a : 'a } [@@deriving hardcaml]

  let%expect_test "simple" =
    print_t_list (to_list port_names_and_widths);
    [%expect {| a 1 |}]
  ;;
end

module _ = struct
  type 'a t =
    { a : 'a [@bits 12]
    ; b : 'a [@bits 0]
    }
  [@@deriving hardcaml]

  let%expect_test "set bits" =
    print_t_list (to_list port_names_and_widths);
    [%expect {|
      a 12
      b 0
      |}]
  ;;

  let%expect_test "iter" =
    iter ~f:(fun si -> print_s [%sexp (si : string * int)]) port_names_and_widths;
    [%expect {|
      (a 12)
      (b 0)
      |}]
  ;;

  let%expect_test "iter2" =
    iter2
      ~f:(fun si i -> print_s [%sexp ((si, i) : (string * int) * int)])
      port_names_and_widths
      { a = 5; b = 3 };
    [%expect {|
      ((a 12) 5)
      ((b 0) 3)
      |}]
  ;;

  let%expect_test "map" =
    print_t_list (to_list @@ map ~f:(fun (n, b) -> n, b + 1) port_names_and_widths);
    [%expect {|
      a 13
      b 1
      |}]
  ;;

  let%expect_test "map2" =
    print_t_list
      (to_list
       @@ map2 ~f:(fun (n, b) c -> n, b + c) port_names_and_widths { a = 5; b = 3 });
    [%expect {|
      a 17
      b 3
      |}]
  ;;

  let%expect_test "[map] order" =
    ignore (map ~f:(fun si -> print_s [%sexp (si : string * int)]) port_names_and_widths);
    [%expect {|
      (a 12)
      (b 0)
      |}]
  ;;

  let%expect_test "[map2] order" =
    ignore
      (map2
         ~f:(fun si i -> print_s [%sexp ((si, i) : (string * int) * int)])
         port_names_and_widths
         { a = 5; b = 3 });
    [%expect {|
      ((a 12) 5)
      ((b 0) 3)
      |}]
  ;;
end

module Rtlname = struct
  type 'a t = { hello : 'a [@rtlname "WORLD"] } [@@deriving hardcaml]

  let%expect_test "rtlname" =
    print_t_list (to_list port_names_and_widths);
    [%expect {| WORLD 1 |}]
  ;;
end

module _ = struct
  type 'a t =
    { a : 'a [@bits 2]
    ; b : 'a Simple.t
    ; c : 'a Rtlname.t
    }
  [@@deriving hardcaml]

  let%expect_test "Nesting" =
    print_t_list (to_list port_names_and_widths);
    [%expect {|
      a 2
      a 1
      WORLD 1
      |}]
  ;;
end

module _ = struct
  type 'a t =
    { world : 'a [@rtlprefix "hello_"]
    ; foo : 'a [@rtlprefix "hello_"] [@rtlname "WORLD"]
    ; x : 'a Simple.t [@rtlprefix "i_"]
    ; y : 'a Simple.t [@rtlprefix "i_"] [@rtlmangle true]
    }
  [@@deriving hardcaml]

  let%expect_test "rtlprefix" =
    print_t_list (to_list port_names_and_widths);
    [%expect
      {|
      hello_world 1
      hello_WORLD 1
      i_a 1
      i_y_a 1
      |}]
  ;;
end

module _ = struct
  type 'a t =
    { hello : 'a [@rtlsuffix "_world"]
    ; foo : 'a [@rtlname "hello_"] [@rtlsuffix "WORLD"]
    ; x : 'a Simple.t [@rtlsuffix "_o"]
    ; y : 'a Simple.t [@rtlsuffix "_o"] [@rtlmangle true]
    }
  [@@deriving hardcaml]

  let%expect_test "rtlsuffix" =
    print_t_list (to_list port_names_and_widths);
    [%expect
      {|
      hello_world 1
      hello_WORLD 1
      a_o 1
      y_a_o 1
      |}]
  ;;
end

module _ = struct
  type 'a t =
    { x : 'a array [@length 1]
    ; y : 'a array [@length 3] [@bits 5]
    ; z : 'a array [@length 2] [@rtlname "Z"]
    }
  [@@deriving hardcaml]

  let%expect_test "arrays" =
    print_t_list (to_list port_names_and_widths);
    [%expect
      {|
      x0 1
      y0 5
      y1 5
      y2 5
      Z0 1
      Z1 1
      |}]
  ;;
end

module _ = struct
  module M = struct
    type 'a t = { foo : 'a } [@@deriving hardcaml]
  end

  type 'a t = { x : 'a M.t array [@length 1] } [@@deriving hardcaml]

  let%expect_test _ =
    print_t_list (to_list port_names_and_widths);
    [%expect {| foo0 1 |}]
  ;;
end

module _ = struct
  type 'a t =
    { x : 'a list [@length 1]
    ; y : 'a list [@length 3] [@bits 5]
    ; z : 'a list [@length 2] [@rtlname "Z"]
    }
  [@@deriving hardcaml]

  let%expect_test "lists" =
    print_t_list (to_list port_names_and_widths);
    [%expect
      {|
      x0 1
      y0 5
      y1 5
      y2 5
      Z0 1
      Z1 1
      |}]
  ;;
end

module _ = struct
  module M = struct
    type 'a t = { foo : 'a } [@@deriving hardcaml]
  end

  type 'a t = { x : 'a M.t list [@length 1] } [@@deriving hardcaml]

  let%expect_test _ =
    print_t_list (to_list port_names_and_widths);
    [%expect {| foo0 1 |}]
  ;;
end

module _ = struct
  type 'a t =
    { a : 'a [@rtlprefix "X"]
    ; b : 'a
    }
  [@@deriving hardcaml ~rtlprefix:"i_"]

  let%expect_test "rtlprefix_option" =
    print_t_list (to_list port_names_and_widths);
    [%expect {|
      Xa 1
      i_b 1
      |}]
  ;;
end

module _ = struct
  type 'a t =
    { a : 'a [@rtlsuffix "X"]
    ; b : 'a
    }
  [@@deriving hardcaml ~rtlsuffix:"_o"]

  let%expect_test "rtlsuffix_option" =
    print_t_list (to_list port_names_and_widths);
    [%expect {|
      aX 1
      b_o 1
      |}]
  ;;
end

module _ = struct
  type 'a t =
    { a : 'a [@bits 2]
    ; b : 'a Simple.t
    ; c : 'a Rtlname.t
    }
  [@@deriving hardcaml ~rtlmangle:true]

  let%expect_test "rtlmangle option" =
    print_t_list (to_list port_names_and_widths);
    [%expect {|
      a 2
      b_a 1
      c_WORLD 1
      |}]
  ;;
end

module _ = struct
  module N = struct
    type 'a t = { n : 'a } [@@deriving hardcaml]
  end

  type 'a t =
    { a : 'a [@rtlprefix "P"] [@rtlname "N"] [@rtlsuffix "S"]
    ; b : 'a [@rtlprefix "P"] [@rtlsuffix "S"]
    ; c : 'a [@rtlprefix "P"]
    ; d : 'a [@rtlprefix "P"] [@rtlname "N"]
    ; e : 'a [@rtlname "N"] [@rtlsuffix "S"]
    ; f : 'a [@rtlsuffix "S"]
    ; g : 'a
    ; h : 'a [@rtlname "N"]
    ; i : 'a N.t [@rtlprefix "P"] [@rtlsuffix "S"]
    ; j : 'a N.t [@rtlprefix "P"]
    ; k : 'a N.t [@rtlsuffix "S"]
    ; l : 'a N.t
    }
  [@@deriving hardcaml ~rtlmangle:true ~rtlprefix:"p" ~rtlsuffix:"s"]

  let%expect_test "options and overrides" =
    print_t_list (to_list port_names_and_widths);
    [%expect
      {|
      PNS 1
      PbS 1
      Pcs 1
      PNs 1
      pNS 1
      pfS 1
      pgs 1
      pNs 1
      Pi_nS 1
      Pj_ns 1
      pk_nS 1
      pl_ns 1
      |}]
  ;;
end

(* Testing optional fields *)
module _ = struct
  module type Arg = sig
    val exists : bool
  end

  module M = struct
    type 'a t = { x : 'a [@bits 42] } [@@deriving hardcaml]
  end

  module Make (A : Arg) = struct
    type 'a t =
      { clock : 'a
      ; optional_scalar : 'a option [@bits 12] [@exists A.exists] [@rtlname "os"]
      ; optional_array_of_scalar : 'a array option
           [@bits 12] [@length 2] [@exists A.exists] [@rtlname "oas$"]
      ; optional_module : 'a M.t option [@exists A.exists] [@rtlprefix "om$"]
      ; optional_list_of_modules : 'a M.t list option
           [@length 2] [@exists A.exists] [@rtlprefix "olm$"]
      }
    [@@deriving hardcaml]

    let test () =
      print_endline "Port names and widths:";
      printf
        "  %s\n"
        (Sexp.to_string_hum
           ~indent:3
           ([%sexp_of: (string * int) t] port_names_and_widths));
      print_endline "";
      print_endline "to_list";
      printf
        "  %s\n"
        (Sexp.to_string_hum ~indent:2 ([%sexp_of: string list] (to_list port_names)));
      print_endline "";
      print_endline "iter";
      iter port_names ~f:(printf "  - %s\n");
      print_endline "";
      print_endline "iter2";
      iter2 port_names port_widths ~f:(printf "  - %s: %d\n")
    ;;
  end

  module A = Make (struct
    let exists = true
  end)

  module B = Make (struct
    let exists = false
  end)

  let%expect_test "optional fields" =
    A.test ();
    [%expect
      {|
      Port names and widths:
        ((clock (clock 1)) (optional_scalar ((os 12)))
         (optional_array_of_scalar (((oas$0 12) (oas$1 12))))
         (optional_module (((x (om$x 42)))))
         (optional_list_of_modules ((((x (olm$x0 42))) ((x (olm$x1 42)))))))

      to_list
        (clock os oas$0 oas$1 om$x olm$x0 olm$x1)

      iter
        - clock
        - os
        - oas$0
        - oas$1
        - om$x
        - olm$x0
        - olm$x1

      iter2
        - clock: 1
        - os: 12
        - oas$0: 12
        - oas$1: 12
        - om$x: 42
        - olm$x0: 42
        - olm$x1: 42
      |}];
    B.test ();
    [%expect
      {|
      Port names and widths:
        ((clock (clock 1)) (optional_scalar ()) (optional_array_of_scalar ())
         (optional_module ()) (optional_list_of_modules ()))

      to_list
        (clock)

      iter
        - clock

      iter2
        - clock: 1
      |}]
  ;;
end

module Extended : sig
  type 'a t = { foo : 'a } [@@deriving hardcaml]
end = struct
  type 'a t = { foo : 'a } [@@deriving hardcaml]
end

let%expect_test "extended" =
  print_s [%sexp (Extended.port_widths : int Extended.t)];
  [%expect {| ((foo 1)) |}];
  print_s [%sexp (Extended.port_names : string Extended.t)];
  [%expect {| ((foo foo)) |}]
;;

module Bar : sig
  type 'a t = { bar : 'a } [@@deriving hardcaml ~ast]
end = struct
  type 'a t = { bar : 'a (** bar documentation *) } [@@deriving hardcaml ~ast]
end

module Foo : Hardcaml.Interface.S_with_ast = struct
  type 'a t =
    { foo : 'a (** foo documentation *)
    ; bar : 'a Bar.t
    ; lst : 'a list [@length 7]
    ; arr : 'a array [@length 0]
    ; lstm : 'a Bar.t list [@length 7] (** lstm documentation *)
    ; arrm : 'a Bar.t array [@length 0]
    }
  [@@deriving hardcaml ~ast]
end

let%expect_test "ast" =
  print_s [%sexp (Foo.ast : Hardcaml.Interface.Ast.t)];
  [%expect
    {|
    (((name foo)
      (type_ (
        Signal
        (bits    1)
        (rtlname foo)))
      (sequence ())
      (doc (" foo documentation ")))
     ((name bar)
      (type_ (
        Module
        (name Bar)
        (ast ((
          (name bar)
          (type_ (
            Signal
            (bits    1)
            (rtlname bar)))
          (sequence ())
          (doc (" bar documentation ")))))))
      (sequence ())
      (doc      ()))
     ((name lst)
      (type_ (
        Signal
        (bits    1)
        (rtlname lst)))
      (sequence ((
        (kind   List)
        (length 7))))
      (doc ()))
     ((name arr)
      (type_ (
        Signal
        (bits    1)
        (rtlname arr)))
      (sequence ((
        (kind   Array)
        (length 0))))
      (doc ()))
     ((name lstm)
      (type_ (
        Module
        (name Bar)
        (ast ((
          (name bar)
          (type_ (
            Signal
            (bits    1)
            (rtlname bar)))
          (sequence ())
          (doc (" bar documentation ")))))))
      (sequence ((
        (kind   List)
        (length 7))))
      (doc (" lstm documentation ")))
     ((name arrm)
      (type_ (
        Module
        (name Bar)
        (ast ((
          (name bar)
          (type_ (
            Signal
            (bits    1)
            (rtlname bar)))
          (sequence ())
          (doc (" bar documentation ")))))))
      (sequence ((
        (kind   Array)
        (length 0))))
      (doc ())))
    |}]
;;

(* This type signatures below demonstrates the PPX is more relaxed in mlis. Namely, even
   though the PPX cannot generate definitions for the below record fields, they are
   still allowed in the mli.
*)
module type Allow_nested_in_sig = sig
  type 'a t =
    { double_nested_field_type : 'a Bar.t Bar.t
    ; inline_functor_application : 'a Map.M(Int).t
    }
  [@@deriving hardcaml]
end

module Rtlmangle_with_seperator = struct
  type 'a t =
    { the_bar_field : 'a Bar.t [@rtlmangle "$"]
    ; the_simple_field : 'a Simple.t
    }
  [@@deriving hardcaml ~rtlmangle:"___"]
end

let%expect_test "rtlmangle with a non default seperator" =
  print_s
    ([%sexp_of: string Rtlmangle_with_seperator.t] Rtlmangle_with_seperator.port_names);
  [%expect
    {|
    ((the_bar_field    ((bar the_bar_field$bar)))
     (the_simple_field ((a   the_simple_field___a))))
    |}]
;;

(* Demonstrate naming functionality *)
let%test_module _ =
  (module struct
    (* The naming PPX explicitly refers to Hardcaml in its expansion. Provide stubs for
       the functions and types it uses. *)
    module Hardcaml = struct
      module Signal = struct
        type t = Dummy_signal [@@deriving sexp]
      end

      module Scope = struct
        type t = Dummy_scope [@@deriving sexp]

        let naming scope signal_to_name name_for_signal =
          Core.print_s
            [%message
              "Hardcaml.Scope.naming called"
                (scope : t)
                (signal_to_name : Signal.t)
                (name_for_signal : string)];
          signal_to_name
        ;;
      end

      module Always = struct
        module Variable = struct
          type t = { value : Signal.t }
        end
      end
    end

    module Test_type = struct
      type t = { test_signal : Hardcaml.Signal.t } [@@deriving sexp]

      let apply_names ~prefix ~naming_op thing_to_name =
        Core.print_s
          [%message "Test_type.apply_names called" (prefix : string) (thing_to_name : t)];
        ignore
          (naming_op thing_to_name.test_signal (prefix ^ "test_signal")
            : Hardcaml.Signal.t);
        thing_to_name
      ;;
    end

    let%expect_test "naming a signal" =
      let scope = Hardcaml.Scope.Dummy_scope in
      let%hw _use_this_name_for_the_signal = Hardcaml.Signal.Dummy_signal in
      [%expect
        {|
        ("Hardcaml.Scope.naming called" (scope Dummy_scope)
         (signal_to_name Dummy_signal)
         (name_for_signal _use_this_name_for_the_signal))
        |}]
    ;;

    let%expect_test "naming a type" =
      let scope = Hardcaml.Scope.Dummy_scope in
      let%hw.Test_type _use_this_name_for_the_signal_in_the_type =
        { Test_type.test_signal = Dummy_signal }
      in
      [%expect
        {|
        ("Test_type.apply_names called"
         (prefix _use_this_name_for_the_signal_in_the_type$)
         (thing_to_name ((test_signal Dummy_signal))))
        ("Hardcaml.Scope.naming called" (scope Dummy_scope)
         (signal_to_name Dummy_signal)
         (name_for_signal _use_this_name_for_the_signal_in_the_type$test_signal))
        |}]
    ;;

    let%expect_test "naming a variable" =
      let scope = Hardcaml.Scope.Dummy_scope in
      let%hw_var _use_this_name_for_the_signal_in_the_var =
        { Hardcaml.Always.Variable.value = Dummy_signal }
      in
      [%expect
        {|
        ("Hardcaml.Scope.naming called" (scope Dummy_scope)
         (signal_to_name Dummy_signal)
         (name_for_signal _use_this_name_for_the_signal_in_the_var))
        |}]
    ;;
  end)
;;
