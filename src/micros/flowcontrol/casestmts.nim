import micros/[utils, nimnodes]

func isa*(n: NimNode, _: typedesc[CaseStmt]): bool =
  n.checkIt {nnkCaseStmt}
  n.checkIt 1..^1, {nnkOfBranch, nnkElifBranch, nnkElse}

func caseStmt*(n: NimNode): CaseStmt =
  ## Ensures `n` isa `CaseStmt` and then converts to it.
  n.checkConv CaseStmt

func caseStmt*(delim: NimName): CaseStmt =
  ## Starts a `CaseStmt` discriminated by `delim`.
  CaseStmt nnkCaseStmt.newTree(NimNode delim)

func discriminator*(casStmt: CaseStmt): NimNode =
  ## Retrieves the descriminator of `casStmt`.
  casStmt.NimNode[0]

func add*(casStmt: CaseStmt, ofBranch: OfBranch) =
  ## Adds the new `ofBranch` to `casStmt`.
  ## This does add it before `ElifBranch` or `ElseBranch`
  let n = NimNode casStmt
  var toInsert = 1
  for i, x in n:
    if x.kind in {nnkElifBranch, nnkElse}:
      toInsert = i - 1
      break
  n.insert toInsert, NimNode ofBranch

func add*(casStmt: CaseStmt, elifBranch: ElifBranch) =
  ## Adds the new `ofBranch` to `casStmt`.
  ## This adds it right before the `ElseBranch`
  let n = NimNode casStmt
  var toInsert = n.len
  for i, x in n:
    if x.kind == nnkElse:
      toInsert = i
      break
  n.insert toInsert, NimNode elifBranch

func add*(casStmt: CaseStmt, elseBranch: ElseBranch) =
  ## Adds the new `elseBranch` to `casStmt`
  ## This raises an assertion if there is already one there.
  let n = NimNode casStmt
  assert n[^1].kind != nnkElse
  n.add NimNode elseBranch

iterator branches*(caseStmt: CaseStmt): NimNode =
  ## Iterates all the branches of `caseStmt`
  ## Returns NimNode since they are not homogeneous types,
  ## use `isa` and conversions to get around this.
  let n = NimNode caseStmt
  if n.len > 1:
    for i in 1..<n.len:
      yield n[i]
