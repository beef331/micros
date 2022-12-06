import unittest
import micros
suite "VarDef":
  test"Adding pragma to macro":
    when (NimMajor, NimMinor) > (1, 6):
      macro rtcNoInit(def: untyped): untyped =
        let vrDef = varDef(def)
        vrDef.add pragmaVal("codegendecl", "RTC_NOINIT_ATTR $# $#")
        assert repr(NimNode vrDef) == """var a {.codegendecl: "RTC_NOINIT_ATTR $# $#".} = 10"""
    else:
      macro rtcNoInit(lh, rh, ex: untyped): untyped =
        let
          n =
            if rh.kind == nnkNilLit:
              newEmptyNode()
            else:
              rh
          vrDef = VarDef nnkVarSection.newTree(nnkIdentDefs.newTree(lh, n, ex))
        vrDef.add pragmaVal("codegendecl", "RTC_NOINIT_ATTR $# $#")
        assert repr(NimNode vrDef) == """var a {.codegendecl: "RTC_NOINIT_ATTR $# $#".} = 10"""
    var a {.rtcNoInit.} = 10
