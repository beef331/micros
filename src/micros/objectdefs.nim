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

func recList*(obj: ObjectDef): NimNode =
  let
    n = NimNode obj
  if n[^1].kind == nnkRefTy:
    n[^1][0][2]
  else:
    n[^1][2]

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

iterator fields*(obj: ObjectDef): IdentDef =
  for def in obj.recList:
    if def.kind == nnkIdentDefs:
      yield IdentDef def
