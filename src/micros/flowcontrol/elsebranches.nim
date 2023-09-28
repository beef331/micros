import ../[nimnodes, utils]
import std/macros
func isa*(n: NimNode, _: typedesc[ElseBranch]): bool =
  n.checkit {nnkElse}
  if n.len != 1:
    return false

func elseBranch*(n: NimNode): ElseBranch =
  ## Ensures `n` isa `ElseBranch` and then converts to it.
  if n.kind != nnkElse:
    ElseBranch nnkElse.newTree(n)
  else:
    n.checkConv ElseBranch

func stmtList*(elseBranch: ElseBranch): NimNode =
  ## Retrieves the body of `elseBranch`
  elseBranch.NimNode[0]
