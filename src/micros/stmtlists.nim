import nimnodes

func stmtList*(n: NimNode): StmtList =
  assert n.kind == nnkStmtList
  StmtList n

func add*(s: StmtList, n: StmtSubTypes) =
  s.NimNode.add NimNode n
