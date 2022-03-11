import nimnodes, utils
import std/macros
func `of`*(n: NimNode, _: typedesc[ElseBranch]): bool =
  n.checkit {nnkElse}
  n.checkit 0, {nnkStmtList}

func elseBranch*(n: NimNode): ElseBranch =
  if n.kind != nnkElse:
    ElseBranch nnkElse.newTree(n)
  else:
    n.checkConv ElseBranch

func stmtList*(elseBranch: ElseBranch): NimNode = elseBranch.NimNode[0]
