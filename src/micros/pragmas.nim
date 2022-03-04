import nimnodes

func pragmaVal(name: string, val: auto): PragmaVal =
  PragmaVal newColonExpr(ident name, newLit val)

func pragmaVal(n: NimNode): PragmaVal =
  assert n.kind in {nnkExprColonExpr, nnkIdent}
  PragmaVal n

func pragma*(name: string): PragmaVal = PragmaVal ident name

template pragma*(name: untyped, val: untyped): PragmaVal = pragmaVal(astToStr(name), val)

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
