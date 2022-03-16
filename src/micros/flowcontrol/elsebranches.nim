import micros/[nimnodes, utils]
import std/macros
func isa*(n: NimNode, _: typedesc[ElseBranch]): bool =
  n.checkit {nnkElse}
  if n.len != 1:
    return false

func elseBranch*(n: NimNode): ElseBranch =
  if n.kind != nnkElse:
    ElseBranch nnkElse.newTree(n)
  else:
    n.checkConv ElseBranch

func stmtList*(elseBranch: ElseBranch): NimNode = elseBranch.NimNode[0]
