import nimnodes
func castTo*(node: NimNodes, typ: typedesc): NimNode =
  nnkCast.newTree(typ.getType[^1], NimNode node)

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
  assert `of`(node, typ)
  typ(node)
