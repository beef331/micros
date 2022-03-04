import nimnodes, identdefs
import std/[enumerate, options]
export options
func objectDef*(n: NimNode): ObjectDef =
  let n =
    case n.kind
    of nnkSym:
      n.getTypeInst.getImpl
    of nnkObjConstr:
      n[0].getImpl
    else:
      n
  assert n.kind == nnkTypeDef
  ObjectDef n

proc recList*(obj: ObjectDef): auto =
  if obj.NimNode[^1].kind == nnkRefTy:
    obj.NimNode[^1][0][2]
  else:
    obj.NimNode[^1][2]

proc `recList=`*(obj: ObjectDef, val: NimNode) =
  if obj.NimNode[^1].kind == nnkRefTy:
    obj.NimNode[^1][0][2] = val
  else:
    obj.NimNode[^1][2] = val

func inheritObj*(obj: ObjectDef): Option[ObjectDef] =
  let inherit =
    if obj.NimNode[^1].kind == nnkRefTy:
      obj.NimNode[^1][0][1]
    else:
      obj.NimNode[^1][1]

  if inherit.kind == nnkOfInherit and not inherit[0].eqIdent("RootObj"):
    result = some objectDef(inherit[0])

func `name=`*(obj: ObjectDef, newName: NimName or string) =
  let
    newName =
      when newName is NimName:
        NimNode newName
      else:
        ident newName
    obj = NimNode(obj)
  case obj[0].kind
  of nnkIdent, nnkSym:
    obj[0] = newName
  of nnkPragmaExpr:
    case obj[0][0].kind
    of nnkIdent, nnkSym:
      obj[0][0] = newName
    else: # Postfix pragma
      obj[0][0][1] = newName
  else: # Postfix
    obj[0][1] = newName

func `name`*(obj: ObjectDef): NimName =
  let obj = NimNode obj
  NimName(
    case obj[0].kind
    of nnkIdent, nnkSym:
      obj[0]
    of nnkPragmaExpr:
      case (obj)[0][0].kind
      of nnkIdent, nnkSym:
        obj[0][0]
      else: # Postfix pragma
        obj[0][0][1]
    else: # Postfix
      obj[0][1]
  )


## Todo handle fields inside recCase and reclist

func addField*(obj: ObjectDef, field: IdentDef) =
  let
    n = NimNode obj
  case obj.recList.kind
  of nnkRecCase, nnkRecWhen:
    obj.recList = nnkRecList.newTree(obj.recList, NimNode field)
  else:
    obj.recList.add NimNode(field)

func delField*(obj: ObjectDef, fieldName: string) =
  for i, idef in obj.recList:
    if idef.kind == nnkIdentDefs:
      let idef = identDef(idef)
      if idef.isSingle:
        if idef.name.NimNode.eqIdent(fieldName):
          obj.recList.del(i, 1)
          break
      else:
        for ind, name in enumerate idef.names:
          if name.NimNode.eqident(fieldName):
            idef.NimNode.del(ind, 1)
            return

proc collectBranchFields(n: NimNode): seq[IdentDef] =
  case n.kind:
  of nnkIdentDefs:
    result.add identDef n
  of nnkRecCase:
    result.add identDef n[0]
    for i in 1..<n.len:
      result.add collectBranchFields(n[i][^1]) # Get reclist of branch
  of nnkRecList:
    for child in n:
      result.add collectBranchFields(child)
  else: discard

iterator fields*(obj: ObjectDef): IdentDef =
  for def in obj.recList:
    case def.kind
    of nnkIdentDefs:
      yield IdentDef def
    of nnkRecCase:
      for fields in collectBranchFields(def):
        yield fields
    else: discard
  var parent = obj.inheritObj
  while parent.isSome:
    for iDef in collectBranchFields(parent.get.reclist):
      yield iDef
    parent = parent.get.inheritObj

iterator pragmas*(obj: ObjectDef): PragmaVal =
  case obj.NimNode[0].kind
  of nnkPragmaExpr:
    for i in 1..<obj.NimNode[0].len:
      yield PragmaVal obj.NimNode[0][i]
  else: discard

iterator genericParams*(obj: ObjectDef): IdentDef =
  for def in NimNode(obj)[1]:
    yield IdentDef def
