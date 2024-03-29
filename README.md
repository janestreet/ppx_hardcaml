ppx_deriving Plugin for Hardcaml
================================

This ppx supports grouping collections of signals using OCaml records. It
generates a set of functions that can be used to process the fields of the
record, associate bit witdths and manage generated RTL names.

We generally call a collection of signals an `interface`.

Such interfaces may be nested and include arrays and list typed fields.

It must be used in conjunction with `[@@deriving sexp_of]`, like this:

```ocaml
type 'a t =
  {
    ...
  }
[@@deriving sexp_of, hardcaml]
```

## Example

```ocaml
module S = struct
  type 'a t =
    { signal : 'a [@bits 6]
    ; signal_list : 'a list [@length 2]
    ; signal_array : 'a array [@length 3] [@bits 7]
    ; sub_interface : 'a T.t [@rtlmangle true] (* where [T.t] also derives hardcaml *)
    }
  [@@deriving sexp_of, hardcaml]
end
```

## The generated code

The type generated by the ppx is exactly as specified. It is a record with each
field of type `'a`. There may be only one polymorphic type and each field must
have this type. The ppx only generates related functions and values.

## Bit widths

Simple fields take an optional bit width specification.

```ocaml
foo : 'a [@bits 8]
```

These specifications do not have to be constants.

```ocaml
foo : 'a [@bits n + 1]
```

If not provided, a default bit width of 1 inferred.

## Lengths

When specifying list and array fields, a length _must_ be provided.

```ocaml
foo : 'a array [@length 6]
```

## Naming

By default the name of the field will be will be used by Hardcaml libraries to
generate an RTL signal identifier. Sometimes this is not appropriate (ie OCaml
names are not always valid Verilog or VHDL names) so a number of attributes are
available to rename fields.

### Naming field attributes

```
foo : 'a [@rtlname "bar"]
```

The field `foo` will be called `bar` in the RTL.

```ocaml
foo : 'a [@rtlprefix "bar_"]
foo : 'a [@rtlsuffix "_bar"]
```

The field name will be combined with the pre/suffix to produce `bar_foo` or `foo_bar`

```ocaml
foo : 'a Foo.t [@rtlprefix "foo_"]
foo : 'a Foo.t [@rtlmangle true]
```

Each field in [Foo.t] will be prefixed with `foo_`. The `rtlmangle` attribute does this
prefixing automatically based on the outer field name.

### Global naming options

The naming attributes can be applied to all fields at once as follows

```ocaml
[@@deriving sexp_of, hardcaml ~rtlprefix:"foo_"]
```

## Generated API

For the complete API see `Hardcaml.Interface.Pre` and `Hardcaml.Interface.S`.

The PPX generates the functions for `Hardcaml.Interface.Pre`. These include
`map` and `iter` functions for transforming and accessing each field in turn,
`map2` anmd `iter2` for combining interfaces, `to_list` to get a list of fields
and a value `t : (string * int) t` which includes each fields name and bit
width.

A more extensive API is built from these primitives.

## Examples

Assign a group of signals to some wires

```ocaml
X.map2 x_wires x_signals ~f:(<==)
```

Create an constant instance of an interface set to 0:

```ocaml
X.Of_signal.const 0
```

Define a hardware module via it's input and output ports, and an implementation
function. Convenience functors in Hardcaml generate RTL circuits, hierarchical
designs, simulators etc based on this pattern.

```ocaml
module Input_ports = struct
  type 'a t = {...}[@@deriving sexp_of, hardcaml]
end

module Output_ports = struct
  type 'a t = {...}[@@deriving sexp_of, hardcaml]
end

let create (i : _ Input_ports.t) = { Output_ports. ... }
```
