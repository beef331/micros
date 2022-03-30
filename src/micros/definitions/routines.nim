import micros/[nimnodes, utils]
import micros/definitions/identdefs
import std/genasts

type
  RoutineType* = enum ## Used for instantiation of types
    rtProc
    rtFunc
    rtIter
    rtMacro
    rtTemplate

func isa*(n: NimNode, _: typedesc[RoutineNode]): bool =
  n.checkit RoutineNodes
  n.checkit 0, {nnkPragmaExpr, nnkPostfix, nnkIdent, nnkSym} # name
  n.checkit 1, {nnkEmpty, nnkStmtList} # constraint
  n.checkit 2, {nnkGenericParams, nnkEmpty} # generic params
  n.checkit 3, {nnkFormalParams} # formal Params
  n.checkit 4, {nnkEmpty, nnkPragma} # pragmas
  #n.checkit ^1, {nnkStmtList} # Perhaps bad

func routineNode*(n: NimNode): RoutineNode =
  ## Ensures `n` isa `RoutineNode` and then converts to it.
  n.checkConv RoutineNode

func routineNode*(name: NimName or string, typ = rtProc): RoutineNode =
  ## Constructs a RoutineNode with the given `name` and of `typ`
  let name = name.toName.NimNode
  result =
    routineNode:
      case typ
      of rtProc:
        genast(name):
          proc name()
      of rtFunc:
        genast(name):
          funcname()
      of rtIter:
        genast(name):
          iterator name()
      of rtMacro:
        genast(name):
          macro name()
      of rtTemplate:
        genast(name):
          template name()

func strName*(r: RoutineNode): string =
  ## Retrieves the string name of `r`
  let n = NimNode(r)
  case n[0].kind
  of nnkSym, nnkIdent:
    result = $n[0]
  of nnkPostfix:
    result = $n[0][1]
  else:
    assert false, $n[0].kind

func returnType*(r: RoutineNode): NimNode =
  ## Gets the return type of `r`
  NimNode(r)[3][0]

func `returnType=`*(r: RoutineNode, newType: NimNode) =
  ## Sets the return type of `r` to `newType`
  NimNode(r)[3][0] = newType


func param*(r: RoutineNode, toGetInd: int): IdentDef =
  ## Returns the parameter at `toGetIndex`, only a single paramter is returned.
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

func insertIdentDef*(r: RoutineNode, index: int, param: IdentDef)=
  ## Inserts `param` into the formal params of `r`.
  ## Index `0` is the first param not the return type.
  r.NimNode.params.insert(index + 1, NimNode param)

func deleteIdentDef*(r: RoutineNode, index: int) =
  ## Deletes the `index` from `r`'s formal params.
  ## Index `0` is the first param not the return type.
  r.NimNode.params.del(index)

func addGeneric*(r: RoutineNode, generic: IdentDef) =
  ## Adds generic parameter `generric` to `r`
  assert NimNode(generic)[2].kind == nnkEmpty
  if NimNode(r)[2].kind == nnkEmpty:
    NimNode(r)[2] = nnkGenericParams.newTree(NimNode generic)
  else:
    NimNode(r)[2].add NimNode generic

func addParam*(r: RoutineNode, param: IdentDef) =
  ## Adds a normal parameter to `RoutineNode`.
  NimNode(r)[3].add:
    if NimNode(r)[3].len == 0:
      @[newEmptyNode(), NimNode param]
    else:
      @[NimNode(param)]

func add*(r: RoutineNode, pragma: PragmaVal) =
  ## Adds pragma `pragma` to `r`
  if NimNode(r)[4].kind == nnkEmpty:
     NimNode(r)[4] = nnkPragma.newTree(NimNode pragma)
  else:
    NimNode(r)[4].add NimNode pragma

func insert*(r: RoutineNode, i: int, val: StmtSubTypes) =
  ## Inserts a value into the body of `r`
  {.warning: "Use on a typed Procedure may result in accessing the wrong Stmtlist".}
  NimNode(r)[^1].insert i, NimNode val

func body*(r: RoutineNode): StmtList =
  ## Retrieves the body of `r`
  {.warning: "Use on a typed Procedure may result in accessing the wrong Stmtlist".}
  StmtList NimNode(r)[^1]

func addToBody*(r: RoutineNode, toAdd: StmtSubTypes) =
  ## Adds a statement to the end of `r`'s `body`
  {.warning: "Use on a typed Procedure may result in accessing the wrong Stmtlist".}
  case NimNode(r)[^1].kind
  of nnkStmtList:
    r.body.NimNode.add toAdd
  else:
    NimNode(r)[^1] = newStmtList(NimNode r.body, NimNode toAdd)

iterator params*(r: RoutineNode): IdentDef =
  ## Iterates the parameters of `r`, not yielding the return type.
  for i, x in NimNode(r).params:
    if i > 0:
      yield IdentDef(x)

iterator genericParams*(r: RoutineNode): IdentDef =
  ## Iterates the generic parameters of `r`.
  for i in NimNode(r)[2]:
    yield IdentDef(i)

iterator pragmas*(r: RoutineNode): PragmaVal =
  ## Iterates the pragmas of `r`.
  for p in NimNode(r)[4]:
    yield PragmaVal(p)
