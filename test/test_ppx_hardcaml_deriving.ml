open! Import

let%expect_test "Is hardcaml linked?" =
  if not !Ppx_hardcaml_runtime0.hardcaml_is_linked
  then failwith "ppx_hardcaml_runtime should link Hardcaml!"
;;

(* Stuff we need for the test code *)
let print_t_list t =
  List.iter t ~f:(fun (n, b) -> Printf.printf "%s %i\n" n b);
  printf "\n"
;;

module Base_int = Int

(* Alias the modules we export from the runtime. Dont allow them to be accessed directly.
*)
module Array = struct end
module List = struct end
module Iarray = struct end
module Int = struct end
module _ = Array
module _ = List
module _ = Iarray
module _ = Int

module Simple = struct
  type 'a t = { a : 'a } [@@deriving hardcaml]

  let%expect_test "simple" =
    print_t_list (to_list port_names_and_widths);
    [%expect {| a 1 |}]
  ;;
end

module%test Set_bits = struct
  type 'a t =
    { a : 'a [@bits 12]
    ; b : 'a [@bits 0]
    }
  [@@deriving hardcaml]

  let%expect_test "set bits" =
    print_t_list (to_list port_names_and_widths);
    [%expect
      {|
      a 12
      b 0
      |}]
  ;;

  let%expect_test "iter" =
    iter ~f:(fun si -> print_s [%sexp (si : string * int)]) port_names_and_widths;
    [%expect
      {|
      (a 12)
      (b 0)
      |}]
  ;;

  let%expect_test "iter2" =
    iter2
      ~f:(fun si i -> print_s [%sexp ((si, i) : (string * int) * int)])
      port_names_and_widths
      { a = 5; b = 3 };
    [%expect
      {|
      ((a 12) 5)
      ((b 0) 3)
      |}]
  ;;

  let%expect_test "map" =
    print_t_list (to_list @@ map ~f:(fun (n, b) -> n, b + 1) port_names_and_widths);
    [%expect
      {|
      a 13
      b 1
      |}]
  ;;

  let%expect_test "map2" =
    print_t_list
      (to_list
       @@ map2 ~f:(fun (n, b) c -> n, b + c) port_names_and_widths { a = 5; b = 3 });
    [%expect
      {|
      a 17
      b 3
      |}]
  ;;

  let%expect_test "[map] order" =
    ignore (map ~f:(fun si -> print_s [%sexp (si : string * int)]) port_names_and_widths);
    [%expect
      {|
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
    [%expect
      {|
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

module%test Nesting = struct
  type 'a t =
    { a : 'a [@bits 2]
    ; b : 'a Simple.t
    ; c : 'a Rtlname.t
    }
  [@@deriving hardcaml ~rtlmangle:false]

  let%expect_test "Nesting" =
    print_t_list (to_list port_names_and_widths);
    [%expect
      {|
      a 2
      a 1
      WORLD 1
      |}]
  ;;
end

module%test Rtlprefix = struct
  type 'a t =
    { world : 'a [@rtlprefix "hello_"]
    ; foo : 'a [@rtlprefix "hello_"] [@rtlname "WORLD"]
    ; x : 'a Simple.t [@rtlprefix "i_"]
    ; y : 'a Simple.t [@rtlprefix "i_"] [@rtlmangle "_"]
    }
  [@@deriving hardcaml ~rtlmangle:false]

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

module%test Rtlsuffix = struct
  type 'a t =
    { hello : 'a [@rtlsuffix "_world"]
    ; foo : 'a [@rtlname "hello_"] [@rtlsuffix "WORLD"]
    ; x : 'a Simple.t [@rtlsuffix "_o"]
    ; y : 'a Simple.t [@rtlsuffix "_o"] [@rtlmangle "_"]
    }
  [@@deriving hardcaml ~rtlmangle:false]

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

module%test Arrays = struct
  type 'a t =
    { x : 'a array [@length 1]
    ; y : 'a array [@length 3] [@bits 5]
    ; z : 'a array [@length 2] [@rtlname "Z"]
    }
  [@@deriving hardcaml ~rtlmangle:false]

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

module%test Array_with_module = struct
  module M = struct
    type 'a t = { foo : 'a } [@@deriving hardcaml]
  end

  type 'a t = { x : 'a M.t array [@length 1] } [@@deriving hardcaml ~rtlmangle:false]

  let%expect_test _ =
    print_t_list (to_list port_names_and_widths);
    [%expect {| foo0 1 |}]
  ;;
end

module%test Iarrays = struct
  type 'a t =
    { x : 'a iarray [@length 1]
    ; y : 'a iarray [@length 3] [@bits 5]
    ; z : 'a iarray [@length 2] [@rtlname "Z"]
    }
  [@@deriving hardcaml ~rtlmangle:false]

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

module%test Iarray_with_module = struct
  module M = struct
    type 'a t = { foo : 'a } [@@deriving hardcaml]
  end

  type 'a t = { x : 'a M.t iarray [@length 1] } [@@deriving hardcaml ~rtlmangle:false]

  let%expect_test _ =
    print_t_list (to_list port_names_and_widths);
    [%expect {| foo0 1 |}]
  ;;
end

module%test Lists = struct
  type 'a t =
    { x : 'a list [@length 1]
    ; y : 'a list [@length 3] [@bits 5]
    ; z : 'a list [@length 2] [@rtlname "Z"]
    }
  [@@deriving hardcaml ~rtlmangle:false]

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

module%test List_with_module = struct
  module M = struct
    type 'a t = { foo : 'a } [@@deriving hardcaml]
  end

  type 'a t = { x : 'a M.t list [@length 1] } [@@deriving hardcaml ~rtlmangle:false]

  let%expect_test _ =
    print_t_list (to_list port_names_and_widths);
    [%expect {| foo0 1 |}]
  ;;
end

module%test Rtlprefix_option = struct
  type 'a t =
    { a : 'a [@rtlprefix "X"]
    ; b : 'a
    }
  [@@deriving hardcaml ~rtlprefix:"i_"]

  let%expect_test "rtlprefix_option" =
    print_t_list (to_list port_names_and_widths);
    [%expect
      {|
      Xa 1
      i_b 1
      |}]
  ;;
end

module%test Rtlsuffix_option = struct
  type 'a t =
    { a : 'a [@rtlsuffix "X"]
    ; b : 'a
    }
  [@@deriving hardcaml ~rtlsuffix:"_o"]

  let%expect_test "rtlsuffix_option" =
    print_t_list (to_list port_names_and_widths);
    [%expect
      {|
      aX 1
      b_o 1
      |}]
  ;;
end

module%test Rtlmangle_option = struct
  type 'a t =
    { a : 'a [@bits 2]
    ; b : 'a Simple.t
    ; c : 'a Rtlname.t
    }
  [@@deriving hardcaml ~rtlmangle:"_"]

  let%expect_test "rtlmangle option" =
    print_t_list (to_list port_names_and_widths);
    [%expect
      {|
      a 2
      b_a 1
      c_WORLD 1
      |}]
  ;;
end

module%test With_valid_in_interface = struct
  type 'a t =
    { a : 'a [@bits 4]
    ; b : 'a Hardcaml.With_valid.t [@bits 1]
    ; c : 'a Hardcaml.With_valid.t [@bits 4]
    }
  [@@deriving hardcaml]

  let%expect_test "With_valid.t in interface" =
    print_t_list (to_list port_names_and_widths);
    [%expect
      {|
      a 4
      b$valid 1
      b$value 1
      c$valid 1
      c$value 4
      |}]
  ;;
end

module%test Options = struct
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
  [@@deriving hardcaml ~rtlmangle:"_" ~rtlprefix:"p" ~rtlsuffix:"s"]

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
      ; optional_iarray_of_scalar : 'a array option
           [@bits 12] [@length 2] [@exists A.exists] [@rtlname "ois$"]
      ; optional_module : 'a M.t option [@exists A.exists] [@rtlprefix "om$"]
      ; optional_list_of_modules : 'a M.t list option
           [@length 2] [@exists A.exists] [@rtlprefix "olm$"]
      }
    [@@deriving hardcaml ~rtlmangle:false]

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
         (optional_iarray_of_scalar (((ois$0 12) (ois$1 12))))
         (optional_module (((x (om$x 42)))))
         (optional_list_of_modules ((((x (olm$x0 42))) ((x (olm$x1 42)))))))

      to_list
        (clock os oas$0 oas$1 ois$0 ois$1 om$x olm$x0 olm$x1)

      iter
        - clock
        - os
        - oas$0
        - oas$1
        - ois$0
        - ois$1
        - om$x
        - olm$x0
        - olm$x1

      iter2
        - clock: 1
        - os: 12
        - oas$0: 12
        - oas$1: 12
        - ois$0: 12
        - ois$1: 12
        - om$x: 42
        - olm$x0: 42
        - olm$x1: 42
      |}];
    B.test ();
    [%expect
      {|
      Port names and widths:
        ((clock (clock 1)) (optional_scalar ()) (optional_array_of_scalar ())
         (optional_iarray_of_scalar ()) (optional_module ())
         (optional_list_of_modules ()))

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
  [@@deriving hardcaml ~rtlmangle:false ~ast]
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
   though the PPX cannot generate definitions for the below record fields, they are still
   allowed in the mli.
*)
module type Allow_nested_in_sig = sig
  type 'a t =
    { double_nested_field_type : 'a Bar.t Bar.t
    ; inline_functor_application : 'a Map.M(Base_int).t
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

(* [derive_from_map2] builds the interface from just the map2 function. Trades compilation
   time for runtime performance. *)
module _ : sig
  type 'a t =
    { a : 'a
    ; b : 'a
    }
  [@@deriving hardcaml]
end = struct
  type 'a t =
    { a : 'a [@bits 5]
    ; b : 'a [@bits 9]
    }
  [@@deriving hardcaml ~derive_from_map2]

  let%expect_test "to_list" =
    let names = to_list port_names in
    print_s [%message (names : string list)];
    [%expect {| (names (a b)) |}]
  ;;

  let%expect_test "map" =
    let t = map { a = 1; b = 2 } ~f:(( + ) 1) in
    print_s [%message (t : int t)];
    [%expect
      {|
      (t (
        (a 2)
        (b 3)))
      |}]
  ;;

  let%expect_test "iter" =
    iter port_names ~f:print_endline;
    [%expect
      {|
      a
      b
      |}]
  ;;

  let%expect_test "map2" =
    let t = map2 { a = 1; b = 2 } { a = 5; b = 7 } ~f:( + ) in
    print_s [%message (t : int t)];
    [%expect
      {|
      (t (
        (a 6)
        (b 9)))
      |}]
  ;;

  let%expect_test "iter2" =
    iter2 port_names port_widths ~f:(fun name width ->
      print_endline [%string "%{name} %{width#Base_int}"]);
    [%expect
      {|
      a 5
      b 9
      |}]
  ;;
end
