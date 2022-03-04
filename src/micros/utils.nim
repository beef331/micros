import nimnodes
func castTo*(node: NimNodes, typ: typedesc): NimNode =
  nnkCast.newTree(typ.getType[^1], NimNode node)
