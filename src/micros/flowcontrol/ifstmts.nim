import ../[nimnodes, utils]
import std/macros

func isa*(n: NimNode, _: typedesc[IfStmt]): bool =
  n.checkit {nnkIfStmt}
  for child in n:
    child.checkit {nnkElifBranch, nnkElse}

func ifStmt*(n: NimNode): IfStmt =
  ## Ensures `n` isa `IfStmt` and then converts to it.
  n.checkConv IfStmt

func ifStmt*(): IfStmt = IfStmt newNimNode(nnkIfStmt)

func add*(ifStmt: IfStmt, elifBranch: ElifBranch) =
  ## Adds an `elifBranch` to `ifStmt`, will position before `else`.
  let node = Nimnode ifStmt
  if node.len > 0 and node[^1].kind == nnkElse:
    node.insert node.len - 1, NimNode elifBranch
  else:
    node.add NimNode elifBranch

func add*(ifStmt: IfStmt, elseBranch: ElseBranch) =
  ## Adds `elseBranch` to `ifStmt`, will raise assertion if there is one.
  assert ifStmt.NimNode[^1].kind != nnkElse
  ifStmt.NimNode.add NimNode elseBranch

func elseBranch*(ifStmt: IfStmt): ElseBranch =
  ## Retrieves the `elseBranch` from `ifStmt`.
  if ifStmt.NimNode[^1].kind == nnkElse:
    ElseBranch ifStmt.NimNode[^1]
  else:
    ElseBranch nil

iterator elifBranches*(ifStmt: IfStmt): ElIfBranch =
  ## Iterates all `elifBranches` of `ifStmt`.
  for x in ifStmt.NimNode:
    if x.kind == nnkElifBranch:
      yield ElifBranch x

iterator branches*(ifStmt: IfStmt): NimNode =
  ## Iterates all branches of `ifStmt` yielding `NimNode`.
  for x in ifStmt.NimNode:
    yield x
