import nimnodes, utils

func `of`*(n: NimNode, _: typedesc[ElifBranch]): bool =
  n.checkit {nnkElifBranch}
  n.checkit 0..^1, {nnkStmtList}

func elifBranch*(n: NimNode): ElifBranch = n.checkConv ElifBranch

func condition*(elifBranch: ElifBranch): NimNode = elifBranch.NimNode[0]

func stmtList*( elifBranch: ElifBranch): NimNode = elifBranch.NimNode[1]
