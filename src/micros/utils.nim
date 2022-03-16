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

