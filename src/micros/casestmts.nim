import utils, nimnodes

func `of`*(n: NimNode, _: typedesc[CaseStmt]): bool =
  n.checkIt {nnkCaseStmt}
  n.checkIt 1..^1, {nnkOfBranch, nnkElifBranch, nnkElse}

func caseStmt*(n: NimNode): CaseStmt = n.checkConv CaseStmt

func caseStmt*(delim: NimName): CaseStmt =
  CaseStmt nnkCaseStmt.newTree(NimNode delim)

func add*(casStmt: CaseStmt, ofBranch: OfBranch) =
  let n = NimNode casStmt
  var toInsert = 1
  for i, x in n:
    if x.kind in {nnkElifBranch, nnkElse}:
      toInsert = i - 1
      break
  n.insert toInsert, NimNode ofBranch

func add*(casStmt: CaseStmt, elifBranch: ElifBranch) =
  let n = NimNode casStmt
  var toInsert = n.len
  for i, x in n:
    if x.kind == nnkElse:
      toInsert = i
      break
  n.insert toInsert, NimNode elifBranch

func add*(casStmt: CaseStmt, elseBranch: ElseBranch) =
  let n = NimNode casStmt
  assert n[^1].kind != nnkElse
  n.add NimNode elseBranch

iterator branches*(caseStmt: CaseStmt): NimNode =
  let n = NimNode caseStmt
  if n.len > 1:
    for i in 1..<n.len:
      yield n[i]
