import nimnodes, utils, stmtlists
import std/macros

func `of`(n: NimNode, _: typedesc[OfBranch]): bool =
  n.checkit {nnkOfBranch}
  n.checkit ^1, {nnkStmtList}

func ofBranch*(n: NimNode): OfBranch = n.checkConv OfBranch

func ofBranch*(cond: not NimNode, body: NimNode): OfBranch =
  ofBranch nnkOfBranch.newTree(newLit cond, body)

func ofBranch*(cond: NimNode, body: NimNode): OfBranch =
  ofBranch nnkOfBranch.newTree(cond, body)

func stmtList*(ofBranch: OfBranch): StmtList = stmtList ofBranch.NimNode[^1]

func addCondition*(ofBranch: OfBranch, cond: NimNode) =
  ofBranch.NimNode.insert ofBranch.NimNode.len - 1, cond

iterator conditions*(ofBranch: OfBranch): NimNode =
  let n = NimNode ofBranch
  for i in 1 ..< n.len - 1:
    yield n[i]
