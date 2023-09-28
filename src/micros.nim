# Todo, make macro to automate this

import micros/[utils, pragmas, stmtlists, nimnodes, enumfields, nimnames]
import micros/definitions/[identdefs, objectdefs, routines, variabledefs, enumdefs, typedefs]
import micros/flowcontrol/[casestmts, elifbranches, elsebranches, ofbranches, forloops, whileloops, whenstmts, ifstmts]

export
  utils,
  pragmas,
  stmtlists,
  nimnodes,
  identdefs,
  objectdefs,
  routines,
  variabledefs,
  casestmts,
  elifbranches,
  elsebranches,
  ofbranches,
  forloops,
  whileloops,
  enumfields,
  enumdefs,
  typedefs,
  whenstmts,
  ifstmts,
  nimnames

when defined(nimDoc):
  import std/macros

## This package implements a static typed high level macro API.
## That means that instead of tedious manual node traversal one uses iterators and types to traverse the AST.

## A comparison between macros, micros
runnableExamples:
  import std/macros
  macro changeRetType(to: typedesc, p: typed): untyped =
    result = p.copyNimTree()
    result.params[0] = to
  proc doThing {.changeRetType: float.} = discard

runnableExamples:
  import std/macros
  import micros
  macro changeRetType(to: typedesc, p: typed): untyped =
    let prc = routineNode(p).copy()
    prc.returnType = to
    result = NimNode(prc)
  proc doThing {.changeRetType: float.} = discard


