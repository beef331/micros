import nimnodes, utils
import std/[macros, genasts]

func `of`*(n: NimNode, _: typedesc[ElseBranch]): bool =
  n.checkit {nnkElifBranch}
  n.checkit 0, {nnkStmtList}

func elseBranch*(n: NimNode): ElseBranch = n.checkConv ElseBranch

macro elsBranch*(body: untyped): untyped =
  let body = body.astGenRepr.parseExpr
  result = newCall(bindSym"elseBranch"):
    genast(body):
      nnkElse.newTree(body)


func stmtList*( elifBranch: ElseBranch): NimNode = elifBranch.NimNode[0]
