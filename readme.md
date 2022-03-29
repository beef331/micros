# Micros - macros are large, micros are small

Micros is a typesafe wrapping around the Nim macro API allowing more expressive creation of macros.

It presently does not wrap the entire Nim AST but does provide a decent amount of abstractions for what it does


The following is a small taste of what it offers:

```nim
macro outputParams(p: typed): untyped =
  let
    p = routineNode p # Convert `p` into a RoutineNode
    paramNames = collect:
      for iDef in p.params: # Iterate over all parameter ident defs
        for names in iDef.names: # Iterate over all parameter names
          $names
    genericNames = collect:
      for iDef in p.genericParams: # Iterate over all generic parameter
        $NimNode(iDef) # Typed AST has symbols here

  result = newStmtList()
  result.add varStmt(p.strName & "Params", paramNames)
  result.add varStmt(p.strName & "GenericParams", genericNames)

proc doThing[T: int, Y, Z](a, b: int, c: float) {.outputParams.} = discard
assert doThingParams == @["a", "b", "c"]
assert doThingGenericParams == @["T", "Y", "Z"]

proc doThingOther[T: int, Y, Z]() {.outputParams.} = discard
assert doThingOtherParams.len == 0
assert doThingOtherGenericParams == @["T", "Y", "Z"]
```

To use it you can `import micros` or selectively import specific modules you want.

The simple logic to extend is the following.
```nim
import micros
import micros/utils
type MyNewAST* = distinct NimNode

func isa*(n: NimNode, _: typedesc[MYNewAST]): bool =
  n.checkit {kindNShouldBe}
  n.checkit 0..^1 {kindChildrenShouldBe}

func myNewAst*(n: NimNode): MyNewAst = n.checkConv MyNewAST
```

`isa` should be implemented for all types to work with the case statement macro.
