import micros/[nimnodes, utils, stmtlists]
import std/macros

func isa*(n: NimNode, _: typedesc[OfBranch]): bool =
  n.checkit {nnkOfBranch}

func ofBranch*(n: NimNode): OfBranch =
  ## Ensures `n` isa `OfBranch` and then converts to it.
  n.checkConv OfBranch

func ofBranch*(cond: not NimNode, body: NimNode): OfBranch =
  ## Generates a new `OfBranch` as `of cond: body`, but takes any values.
  ofBranch nnkOfBranch.newTree(newLit cond, body)

func ofBranch*(cond: NimNode, body: NimNode): OfBranch =
  ## Generates a new `ofBranch` as `of cond, body`, but takes NimNodes.
  ofBranch nnkOfBranch.newTree(cond, body)

func stmtList*(ofBranch: OfBranch): StmtList =
  ## Retrieves the `stmtList` of `ofBranch`
  stmtList ofBranch.NimNode[^1]

func addCondition*(ofBranch: OfBranch, cond: NimNode) =
  ## Adds a condition value to `ofBranch`
  ofBranch.NimNode.insert ofBranch.NimNode.len - 1, cond

iterator conditions*(ofBranch: OfBranch): NimNode =
  ## Yields all condition statements of `ofBranch`.
  let n = NimNode ofBranch
  for cond in n[0..^2]:
    yield cond
