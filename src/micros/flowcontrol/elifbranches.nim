import micros/[nimnodes, utils]
import std/macros

func isa*(n: NimNode, _: typedesc[ElifBranch]): bool =
  n.checkit {nnkElifBranch}

func elifBranch*(n: NimNode): ElifBranch =
  ## Ensures `n` isa `ElifBranch` and then converts to it.
  n.checkConv ElifBranch

func elifBranch*(cond, body: NimNode): ElifBranch =
  ## Generates an `ElifBranch` from two `NimNode`s.
  elifBranch nnkElifBranch.newTree(cond, body)

func condition*(elifBranch: ElifBranch): NimNode =
  ## Retrieves the condition from `elifBranch`
  elifBranch.NimNode[0]

func `condition=`*(elifBranch: ElifBranch, newCond: NimNode) =
  ## sets the condition on `elifBranch`
  elifBranch.NimNode[0] = newCond

func stmtList*(elifBranch: ElifBranch): NimNode =
  ## Retrieves the body of the `elifBranch`
  elifBranch.NimNode[1]
