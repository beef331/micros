import nimnodes, utils

func pragmaVal*(name: string, val: auto): PragmaVal =
  PragmaVal newColonExpr(ident name, newLit val)

func isa*(n: NimNode, _: typedesc[PragmaVal]): bool = n.checkit {nnkExprColonExpr, nnkIdent, nnkSym}

func pragmaVal(n: NimNode): PragmaVal = n.checkConv PragmaVal

func pragma*(name: string): PragmaVal = PragmaVal ident name

func val*(p: PragmaVal): NimNode =
  if NimNode(p).kind == nnkExprColonExpr:
    NimNode(p)[^1]
  else:
    newEmptyNode()

func `val=`*(p: var PragmaVal, val: NimNode) =
  if NimNode(p).kind == nnkExprColonExpr:
    NimNode(p)[^1] = val
  else:
    p = PragmaVal(nnkExprColonExpr.newTree(NimNode p, val))
