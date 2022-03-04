import nimnodes, identdefs
import std/enumerate

func objectDef*(n: NimNode): ObjectDef =
  let n =
    case n.kind
    of nnkSym:
      n.getTypeInst.getImpl
    of nnkObjConstr:
      n[0].getImpl
    else:
      n
  assert n[0].kind in {nnkSym, nnkIdent}
  assert n[^1].kind in {nnkRefTy, nnkObjectTy}
  if n[^1].kind == nnkRefTy:
    assert n[^1][0].kind == nnkObjectTy
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

func `name=`*(obj: ObjectDef, newName: NimName or string) =
  NimNode(obj)[0] =
    when newName is NimName:
      NimNode newName
    else:
      ident newName

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
