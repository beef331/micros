import nimnodes


iterator `[]`*(n: NimNodes, slice: Slice[int]): NimNode =
  when n is DistinctNimNode:
    let n = NimNode n

  for i in slice:
    yield n[i]

iterator `[]`*(n: NimNodes, slice: HSlice[int, BackwardsIndex]): NimNode =
  when n is DistinctNimNode:
    let n = NimNode n

  for i in slice.a .. n.len - int slice.b:
    yield n[i]

iterator `[]`*(n: NimNodes, slice: Slice[BackwardsIndex]): NimNode =
  when n is DistinctNimNode:
    let n = NimNode n

  for i in n.len - int slice.a .. n.len - int slice.b:
    yield n[i]

func castTo*(node: NimNodes, typ: typedesc): NimNode =
  nnkCast.newTree(typ.getType[^1], NimNode node)

func makeTypeDesc*(n: NimNode): NimNode = nnkBracketExpr.newTree(ident"typedesc", n)

template checkIt*(node: NimNode, op: HSlice, toCheck: NimNodeKinds) =
  result = true
  for child in node[op]:
    if child.kind notin toCheck:
      return false

template checkIt*(node: NimNode, op: int or BackwardsIndex, toCheck: NimNodeKinds) =
  result = true
  if node[op].kind notin toCheck:
    return false

template checkIt*(node: NimNode, toCheck: NimNodeKinds) =
  result = true
  if node.kind notin toCheck:
    return false

template checkConv*(node: NimNode, typ: typedesc): auto =
  assert isa(node, typ)
  typ(node)

proc desym*(n: NimNode) =
  for i, x in n:
    case x.kind
    of nnkSym:
      n[i] = ident($x)
    of nnkOpenSymChoice, nnkClosedSymChoice:
      n[i] = ident($x[0])
    else:
      n[i].desym()

proc skipAddrs*(n: NimNode): NimNode =
  case n.kind
  of nnkHiddenAddr:
    n[0]
  of nnkHiddenStdConv:
    n[1]
  else:
    n
