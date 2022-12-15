import micros
import std/[macros, unittest, genasts]

suite "TS Utillity Types":
  test"Pick":
    macro pick(t: typedesc[object], toKeep: static openarray[string]): untyped =
      let
        objDef = objectDef(t)
        name = genNimName("PickedType", nskType)
        newDef = objectDef(name)
      for field in objDef.fields:
        for name in field.names:
          for keeper in toKeep:
            if NimNode(name).eqIdent keeper:
              newDef.addField identDef(name, field.typ)
              break
      result = newStmtList(nnkTypeSection.newTree(NimNode newDef), NimNode name)

    type
      MyType = object
        a, b, c: int
        d, e: string
      MyOtherType = MyType.pick(["a", "b", "e"])
    check $MyOtherType() == """(a: 0, b: 0, e: "")"""

  test"Omit":
    macro omit(t: typedesc[object], toKeep: static openarray[string]): untyped =
      let
        objDef = objectDef(t)
        name = genNimName("OmitedType", nskType)
        newDef = objectDef(name)
      for field in objDef.fields:
        for name in field.names:
          block search:
            for keeper in toKeep:
              if NimNode(name).eqIdent keeper:
                break search
            newDef.addField identDef(name, field.typ)
      result = newStmtList(nnkTypeSection.newTree(NimNode newDef), NimNode name)

    type
      MyType = object
        a, b, c: int
        d, e: string
      MyOtherType = MyType.omit(["a", "b", "e"])
    check $MyOtherType() == """(c: 0, d: "")"""
