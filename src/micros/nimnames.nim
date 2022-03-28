import micros/[utils, nimnodes]

func isa*(n: NimNode, _: typedesc[NimName]): bool =
  n.checkIt {nnkPragmaExpr, nnkSym, nnkIdent}

func nimName*(n: NimNode): NimName = n.checkConv(NimName)

func add*(n: NimName, p: PragmaVal) =
  ## Adds the pragma if this `n` is a `PragmaExpr`
  assert n.NimNode.kind in {nnkPragmaExpr}
  n.NimNode[^1].add p

func copyWithPragma*(name: NimName, prag: PragmaVal): NimName =
  let n = name.NimNode.copyNimTree()
  if n.kind == nnkPragmaExpr:
    n[^1].add prag
    result = nimName n
  else:
    result =
      nimname:
        nnkPragmaExpr.newTree(n, nnkPragma.newTree(NimNode(prag)))
