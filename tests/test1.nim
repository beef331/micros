# This is just an example to get you started. You may wish to put all of your
# tests into a single file, or separate them into multiple `test1`, `test2`
# etc. files (better names are recommended, just make sure the name starts with
# the letter 't').
#
# To run these tests, simply execute `nimble test`.

import unittest

import micros
import std/[macros, sugar, genasts]

test "collecting vals":
  macro outputParams(p: typed): untyped =
    let
      p = routineNode p
      paramNames = collect:
        for iDef in p.params:
          for names in iDef.names:
            $names
      genericNames = collect:
        for iDef in p.genericParams:
          $NimNode(iDef)

    result = newStmtList()
    result.add varStmt(p.strName & "Params", paramNames)
    result.add varStmt(p.strName & "GenericParams", genericNames)

  proc doThing[T: int, Y, Z](a, b: int, c: float) {.outputParams.} = discard
  check doThingParams == @["a", "b", "c"]
  check doThingGenericParams == @["T", "Y", "Z"]

  proc doThingOther[T: int, Y, Z]() {.outputParams.} = discard
  check doThingOtherParams.len == 0
  check doThingOtherGenericParams == @["T", "Y", "Z"]

test "Rtos fun example":
  macro rtosProc(p: untyped): untyped =
    let
      p = routineNode(p)
      retT = p.param(0)
    p.setParamType 0, ident"pointer"
    p.insert 0, letStmt(retT.name, retT.name.castTo(ptr int))
    p.add pragma("cdecl")
    result = NimNode p

  proc doThing(a: int) {.rtosProc.} =
    echo a[]

  check doThing is proc(a: pointer) {.cdecl.}


test "Procedure from scratch":
  macro makeProc(): untyped =
    let p = routineNode("hello")
    p.addToBody newCall("echo", ident"message")
    p.addParam identDef("message", int)
    result = NimNode p
  makeProc()
  hello(10)
  hello(20)


suite "Flow Control Api":
  test "Generate case statement":
    macro makeCase(cond: static int): untyped =
      result = newStmtList()
      result.add letStmt("a", cond)
      let
        stmt = caseStmt(NimName ident"a")
      stmt.add:
        ofBranch(cond):
          genAst: a * 20
      stmt.add:
        elseBranch newLit 10
      stmt.add:
        elifBranch:
          genAst: a mod 2 == 0
        do:
          genAst: a * 3

      result.add stmt

    check makeCase(15) == 300
    check makeCase(10) == 200
    check makeCase(3) == 60

  test "Generate For Loop":
    macro makeLoop(varName: untyped, countTo: static int) =
      result = newStmtList()
      result.add:
        varStmt(NimName varName, 0)
      result.add:
        forLoop(ident"_"):
          genAst(countTo):
            0..<countTo
        do:
          genAst(varName):
            inc varName
    makeLoop(test1, 30)
    makeLoop(test2, 10)
    makeLoop(test3, 50)
    check test1 == 30
    check test2 == 10
    check test3 == 50

  test "Generate While Loop":
    macro makeLoop(varName: untyped, countTo: static int) =
      result = newStmtList()
      result.add:
        varStmt(NimName varName, 0)
      result.add:
        whileLoop():
          genAst(varName, countTo):
            varName < countTo
        do:
          genAst(varName):
            inc varName
    makeLoop(test1, 30)
    makeLoop(test2, 10)
    makeLoop(test3, 50)
    check test1 == 30
    check test2 == 10
    check test3 == 50


suite "Object Api":
  test "From Source":
    macro test(a: typed, name: static string) =
      let obj = objectDef(a).copy
      obj.name = name
      obj.addField identDef("newField", float)
      result = nnkTypeSection.newNimNode()
      result.add obj
      obj.delField("a")
      let fieldNames = collect:
        for idef in obj.fields:
          for name in idef.names:
            $name
      result = newStmtList(result)
      result.add letStmt(name & "Fields", fieldNames)

    type
      Test = ref object of RootObj
        a: int
      Test2 = object
        foo: int
        bar: string
      Test3 = object
        case isThing: bool
        of true:
          foo: int
        else:
          bar: int

    test(Test(), "Hello")
    check Hello().newField is float
    check HelloFields == @["newField"]
    test(Test2(), "Hmm")
    check Hmm().newField is float
    check HmmFields == @["foo", "bar", "newField"]
    test(Test3(), "Huh")
    check HuhFields == @["isThing", "foo", "bar", "newField"]
    let a = Huh(isThing: true)
    check a.foo == 0
    check a.newField == 0

  test "From raw":
    macro emitObject(name: static string, isRef: static bool): untyped =
      let
        obj = objectDef(name, isRef)
      obj.addField identDef("hello", int)
      obj.addField identDef("bye", float)
      result = nnkTypeSection.newTree NimNode obj

    emitObject("MyObj", false)
    check MyObj() is object
    check MyObj().bye is float
    check MyObj().hello is int

    emitObject("MyOtherObj", true)
    check MyOtherObj() is ref
    check MyOtherObj().bye is float
    check MyOtherObj().hello is int


  test "Check Parent Fields":
    macro fieldNames(a: typed): untyped =
      let fields = collect:
        for x in objectDef(a).fields:
          for name in x.names:
            $name
      newLit(fields)
    type
      A = ref object of RootObj
        foo: int
      B = ref object of A
        bar: string
        baz: float
      C = ref object of B
        bungo: int
    check fieldNames(A()) == @["foo"]
    check fieldNames(B()) == @["bar", "baz", "foo"]
    check fieldNames(C()) == @["bungo", "bar", "baz", "foo"]

  test "FieldAccessible":
    type MyObject = object
      case a: uint8
      of 0u8..3u8, 10u8, 20u8:
        b, c : int
      of {40u8, 50 .. 60}:
        d: int
      else:
        e: float

    macro isAccessible(a: typed, name: static string): untyped =
      result = fieldConditions(a, name)
      if result.kind == nnkNilLit:
        result = newLit false

    var a = MyObject()
    check a.isAccessible("a")
    check a.isAccessible("b")
    check a.isAccessible("c")
    check not a.isAccessible("d")
    check not a.isAccessible("e")

    a = MyObject(a: 100)
    check a.isAccessible("e")
    check a.isAccessible("a")
    check not a.isAccessible("b")
    check not a.isAccessible("c")
    check not a.isAccessible("d")



suite  "EnumDefs":
  test "prestige inspired macro":

    macro generateMySafeEnum(n: typedesc[enum], typeName: untyped): untyped =
      result = newStmtList()
      result.add:
        genast(typeName, n):
          type typeName = distinct n
      for field in enumDef(n).fields:
        let newPrc = routineNode($field.name, rtTemplate)
        newPrc.addParam:
          identDefTyp("_", makeTypeDesc(typeName))
        newPrc.returnType = typeName
        newPrc.addToBody:
          genast(field = NimNode field, typeName):
            typeName field
        result.add newPrc

    type MyInnerEnum = enum
      left, right, up, down
    generateMySafeEnum(MyInnerEnum, MyOuter)
    let
      a = MyOuter.right
      b = MyOuter.up

  test "Get enum type":
    type
      MyEnum = enum
        a, b, c
      MyObj = object
       aField: MyEnum
    macro getEType(a: typed): untyped =
      let
        a =
          if a.kind == nnkDotExpr:
            a[^1]
          else:
            a
      let def = a.enumDef
      result = NimNode def.name

    check a.getEType is MyEnum

    var myVar = a
    let mylet = a
    const myConst = a

    check myVar.getEType is MyEnum
    check myLet.getEType is MyEnum
    check myConst.getEType is MyEnum

    var myObj = MyObj()

    check myObj.aField.getEType is MyEnum
