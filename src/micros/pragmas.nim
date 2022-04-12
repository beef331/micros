import nimnodes, utils

func pragmaVal*(name: string, val: auto): PragmaVal =
  ## Constructs a `PragmaVal` with `name: val`
  PragmaVal newColonExpr(ident name, newLit val)

func isa*(n: NimNode, _: typedesc[PragmaVal]): bool = n.checkit {nnkExprColonExpr, nnkIdent, nnkSym}

func pragmaVal*(n: NimNode): PragmaVal =
  ## Ensures `n` isa `PragmaVal` and then converts to it.
  n.checkConv PragmaVal

func pragma*(name: string): PragmaVal =
  ## Converts a string to a `PragmaVal`
  PragmaVal ident name

func pragma*(name: NimName): PragmaVal =
  ## Converts a NimName to PragmaVal
  pragmaVal NimNode name

func val*(p: PragmaVal): NimNode =
  ## Gets the value of a `PragmaVal`
  if NimNode(p).kind == nnkExprColonExpr:
    NimNode(p)[^1]
  else:
    newEmptyNode()

func `val=`*(p: var PragmaVal, val: NimNode) =
  ## Sets the value of a `PragmaVal`
  if NimNode(p).kind == nnkExprColonExpr:
    NimNode(p)[^1] = val
  else:
    p = PragmaVal(nnkExprColonExpr.newTree(NimNode p, val))

func hasVal*(p: PragmaVal): bool = NimNode(p).kind == nnkExprColonExpr

func isNamed*(p: PragmaVal, name: string or NimName): bool =
  if p.hasVal:
    NimNode(p)[0].eqIdent name
  else:
    NimNode(p).eqIdent(name)

