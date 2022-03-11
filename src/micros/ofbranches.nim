import nimnodes, utils, stmtlists
import std/[macros, genasts]

func `of`(n: NimNode, _: typedesc[OfBranch]): bool =
  n.checkit {nnkOfBranch}
  n.checkit ^1, {nnkStmtList}

func ofBranch*(n: NimNode): OfBranch = n.checkConv OfBranch

macro ofBranch*(cond, body: untyped): untyped =
  let
    cond =
      if cond.kind == nnkSym:
        cond
      else:
        cond.astGenRepr.parseExpr
    body = body.astGenRepr.parseExpr
  result = newCall(bindSym"ofBranch"):
    genast(cond, body):
      nnkOfBranch.newTree(cond, body)

func stmtList*(ofBranch: OfBranch): StmtList = stmtList ofBranch.NimNode[^1]

func addCondition*(ofBranch: OfBranch, cond: NimNode) =
  ofBranch.NimNode.insert ofBranch.NimNode.len - 1, cond

iterator conditions*(ofBranch: OfBranch): NimNode =
  let n = NimNode ofBranch
  for i in 1 ..< n.len - 1:
    yield n[i]
