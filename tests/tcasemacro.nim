import micros
import micros/utils/casemacro
import std/[macros, unittest]
{.experimental: "caseStmtMacros".}

suite "Case macro":
  test"Basic delim":
    type TestEnum = enum
      isRoutine, isCase, isFor, isWhile, isVar, isConst, isLet, isObject, isUnknown
    macro doTheThing(n: typed): untyped =
      let n =
        if n.kind == nnkTypeSection:
          n[0]
        else:
          n
      result = newLit:
        case n
        of RoutineNode:
          assert n is RoutineNode
          isRoutine
        of CaseStmt:
          assert n is CaseStmt
          isCase
        of ForLoop:
          assert n is ForLoop
          isFor
        of WhileLoop:
          assert n is WhileLoop
          isWhile
        of VarDef:
          assert n is VarDef
          isVar
        of ConstDef:
          assert n is ConstDef
          isConst
        of LetDef:
          assert n is LetDef
          isLet
        of ObjectDef:
          assert n is ObjectDef
          isObject
        else:
          isUnknown

    let
      routine = doTheThing:
        proc doThing() = discard
      casee = doTheThing:
        case true
        of false: discard
        else: discard
      forLoop = doTheThing:
        for x in 0..10: discard
      whileLoop = doTheThing:
        while true: discard
      varDef = doTheThing:
        var a: int
      constDef = doTheThing:
        const a = 30
      letDef = doTheThing:
        let a = 30
      objDef = doTheThing:
        type A = object
          a, b: int
    check routine == isRoutine
    check casee == isCase
    check forLoop == isFor
    check whileLoop == isWhile
    check varDef == isVar
    check constDef == isConst
    check letDef == isLet
    check objDef == isObject
