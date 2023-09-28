import ../[nimnodes, utils]
import std/macros

func isa*(n: NimNode, _: typedesc[WhenStmt]): bool =
  n.checkit {nnkWhenStmt}
  for child in n:
    child.checkit {nnkElifBranch, nnkElse}

func whenStmt*(n: NimNode): WhenStmt =
  ## Ensures `n` isa `WhenStmt` and then converts to it.
  n.checkConv WhenStmt

func whenStmt*(): WhenStmt = WhenStmt newNimNode(nnkWhenStmt)

func add*(wStmt: WhenStmt, elifBranch: ElifBranch) =
  ## Adds an `elifBranch` to `wStmt`, will position before `else`.
  let node = Nimnode wStmt
  if node.len > 0 and node[^1].kind == nnkElse:
    node.insert node.len - 1, NimNode elifBranch
  else:
    node.add NimNode elifBranch

func add*(wStmt: WhenStmt, elseBranch: ElseBranch) =
  ## Adds `elseBranch` to `wStmt`, will raise assertion if there is one.
  assert wStmt.NimNode[^1].kind != nnkElse
  wStmt.NimNode.add NimNode elseBranch

func elseBranch*(wStmt: WhenStmt): ElseBranch =
  ## Retrieves the `elseBranch` from `wStmt`.
  if wStmt.NimNode[^1].kind == nnkElse:
    ElseBranch wStmt.NimNode[^1]
  else:
    ElseBranch nil

iterator elifBranches*(wStmt: WhenStmt): ElIfBranch =
  ## Iterates all `elifBranches` of `wStmt`.
  for x in wstmt.NimNode:
    if x.kind == nnkElifBranch:
      yield ElifBranch x

iterator branches*(wStmt: WhenStmt): NimNode =
  ## Iterates all branches of `wStmt` yielding `NimNode`.
  for x in wStmt.NimNode:
    yield x
