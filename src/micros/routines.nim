import nimnodes, identdefs

func routineNode*(n: NimNode): RoutineNode =
  assert n.kind in RoutineNodes
  RoutineNode n

func strName*(r: RoutineNode): string = $NimNode(r)[0]

func returnType*(r: RoutineNode): NimNode = NimNode(r)[3][0]

func param*(r: RoutineNode, toGetInd: int): IdentDef =
  var ind = 0
  for i, iDef in NimNode(r)[3]:
    if i > 0:
      for idefInd in 0 ..< idef.len - 1:
        if toGetInd == ind:
          return identDef(newIdentDefs(NimNode(r)[3][i][idefInd], NimNode(r)[3][i][^2]))
        inc ind

func setParamType*(r: RoutineNode, index: int, newType: NimNode, replaceOne = true) =
  ## Replaces type of paramamter `index`, will split the parameter list at that point.
  ## If `replaceOne` is false it'll change the entire identdef of that parameter.
  var ind = 0
  for i, iDef in NimNode(r)[3]:
    if i > 0:
      for idefInd in 0 ..< idef.len - 1:
        if index == ind:
          if replaceOne:
            let
              thisNode = NimNode(r)[3][i]
              typ = thisNode[^2]
              newDef = newIdentDefs(thisNode[idefInd].copyNimTree, newType)
              rightNames = thisNode[iDefInd + 1 .. ^3]
            if thisNode.len > 3:
              # We have more than a single definition handle the sides
              thisNode.del(iDefInd, thisNode.len - iDefInd - 2)
              NimNode(r)[3].insert(i + 1):
                nnkIdentDefs.newTree(rightNames & thisNode[^2..^1])
              NimNode(r)[3].insert(i + 1, newDef)
              if thisNode.len <= 2:
                NimNode(r)[3].del(i)
            else:
              # Single definition assign the type
              NimNode(r)[3][i][^2] = newType
          else:
            NimNode(r)[3][i][^2] = newType
          return
        inc ind

func addGeneric*(r: RoutineNode, generic: IdentDef) =
  assert NimNode(generic)[2].kind == nnkEmpty
  if NimNode(r)[2].kind == nnkEmpty:
    NimNode(r)[2] = nnkGenericParams.newTree(NimNode generic)
  else:
    NimNode(r)[2].add NimNode generic

func addParam*(r: RoutineNode, param: IdentDef) =
  NimNode(r)[3].add NimNode(param)

func add*(r: RoutineNode, pragma: PragmaVal) =
  if NimNode(r)[4].kind == nnkEmpty:
     NimNode(r)[4] = nnkPragma.newTree(NimNode pragma)
  else:
    NimNode(r)[4].add NimNode pragma

func insert*(r: RoutineNode, i: int, val: StmtSubTypes) =
 NimNode(r)[^1].insert i, NimNode val

func body*(r: RoutineNode): StmtList = StmtList NimNode(r)[^1]

iterator params*(r: RoutineNode): IdentDef =
  for i, x in NimNode(r).params:
    if i > 0:
      yield IdentDef(x)

iterator genericParams*(r: RoutineNode): IdentDef =
  for i in NimNode(r)[2]:
    yield IdentDef(i)

iterator pragmas*(r: RoutineNode): PragmaVal =
  for p in NimNode(r)[4]:
    yield PragmaVal(p)
