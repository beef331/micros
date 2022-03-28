import unittest
import micros
suite "VarDef":
  test"Adding pragma to macro":
    macro rtcNoInit(lh, rh, ex: untyped): untyped =
      let
        n =
          if rh.kind == nnkNilLit:
            newEmptyNode()
          else:
            rh
        vrDef = VarDef nnkVarSection.newTree(nnkIdentDefs.newTree(lh, n, ex))
      vrDef.add pragmaVal("codegendecl", "RTC_NOINIT_ATTR $# $#")
    var a {.rtcNoInit.} = 10
