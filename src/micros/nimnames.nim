import utils, nimnodes

func isa*(n: NimNode, _: typedesc[NimName]): bool =
  n.checkIt {nnkPragmaExpr, nnkSym, nnkIdent}

func nimName*(n: NimNode): NimName =
  ## Ensures `n` isa `NimName` and then converts to it.
  n.checkConv(NimName)

func genNimName*(s: string, symKind: NimSymKind): NimName =
  ## Gensym's a `NimName` with name `s` and kind `symKind`
  NimName(genSym(symKind, s))

func nimName*(s: string): NimName =
  ## Converts string to NimName.
  NimName ident(s)

func add*(n: NimName, p: PragmaVal) =
  ## Adds the pragma if this `n` is a `PragmaExpr`
  assert n.NimNode.kind in {nnkPragmaExpr}
  n.NimNode[^1].add p

func copyWithPragma*(name: NimName, prag: PragmaVal): NimName =
  ## Copies `name` adding `prag` to the copy.
  let n = name.NimNode.copyNimTree()
  if n.kind == nnkPragmaExpr:
    n[^1].add prag
    result = nimName n
  else:
    result =
      nimname:
        nnkPragmaExpr.newTree(n, nnkPragma.newTree(NimNode(prag)))

iterator pragmas*(name: NimName): PragmaVal =
  let n = NimNode name
  if n.kind == nnkPragmaExpr:
    for x in n[^1]:
      yield PragmaVal x
