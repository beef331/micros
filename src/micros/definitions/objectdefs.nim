import micros/[nimnodes, utils]
import identdefs
import std/[enumerate, options, genasts]
export options

func isa*(n: NimNode, _: typedesc[ObjectDef]): bool =
  n.checkIt {nnkTypedef}
  n[^1].checkIt {nnkRefTy, nnkObjectTy}
  if n[^1].kind == nnkRefTy:
    n[^1][0].checkit {nnkObjectTy}

func objectDef*(n: NimNode): ObjectDef =
  ## Ensures `n` isa `ObjectDef` and then converts to it.
  let n =
    case n.kind
    of nnkSym:
      let typ = n.getType()
      case typ.typeKind
      of ntyTypeDesc:
        n.getImpl
      else:
        n.getTypeInst.getImpl
    of nnkObjConstr:
      n[0].getImpl
    else:
      n
  n.checkConv ObjectDef

func objectDef*(name: string or NimName, isRef = false, parent: NimName or NimNode = newEmptyNode()): ObjectDef =
  ## Makes a new object definition.
  ## `isRef` makes it a reference.
  ## `parent` not being empty is written as `object of parent`.
  let
    name =
      when name is string:
        ident name
      else:
        name
    parent =
      when parent is NimName:
        NimNode parent
      else:
        parent
    res =
      if isRef:
        if parent.kind == nnkEmpty:
          genAst(name):
            type name = ref object
        else:
          genAst(name, parent):
            type name = ref object of parent
      else:
        if parent.kind == nnkEmpty:
          genAst(name):
            type name = object
        else:
          genAst(name, parent):
            type name = object of parent
  ObjectDef res[0]

proc recList*(obj: ObjectDef): auto =
  ## Retrieves the reclist of `obj`
  if obj.NimNode[^1].kind == nnkRefTy:
    obj.NimNode[^1][0][2]
  else:
    obj.NimNode[^1][2]

proc `recList=`*(obj: ObjectDef, val: NimNode) =
  ## Set the rectlist of `obj` to val.
  if obj.NimNode[^1].kind == nnkRefTy:
    obj.NimNode[^1][0][2] = val
  else:
    obj.NimNode[^1][2] = val

func inheritObj*(obj: ObjectDef): Option[ObjectDef] =
  ## Returns `some(ObjectDef)` if `obj` inherits,
  ## otherwise `none(ObjectDef)`
  let inherit =
    if obj.NimNode[^1].kind == nnkRefTy:
      obj.NimNode[^1][0][1]
    else:
      obj.NimNode[^1][1]

  if inherit.kind == nnkOfInherit and not inherit[0].eqIdent("RootObj"):
    result = some objectDef(inherit[0])

func addGeneric*(obj: ObjectDef, newGenParam: IdentDef) =
  ## Adds generic paramater `newGenParam` to `obj`.
  let gParams = NimNode(obj)[1]
  case gParams.kind
  of nnkGenericParams:
    gParams.add NimNode newGenParam
  else:
    NimNode(obj)[1] = nnkGenericParams.newTree(NimNode newGenParam)

func add*(obj: ObjectDef, pragma: PragmaVal) =
  ## Adds the pragma `pragma` to `obj`.
  case obj.Nimnode[0].kind
  of nnkPragmaExpr:
    obj.NimNode[0].add NimNode pragma
  else:
    obj.NimNode[0] = nnkPragmaExpr.newTree(obj.NimNode[0], NimNode pragma)

func makeAccessCond(n, obj, field: NimNode): NimNode =
  ## Helper for fieldConditions, generating the access logic for a condition.
  template setResult(to: NimNode) =
    if result.kind == nnkNilLit:
      result = to
    else:
      result = infix(result, "or", to)
  let fieldAccess = newDotExpr(obj, field)
  for i, cond in n:
    case cond.kind
    of nnkCurly, nnkInfix:
      setResult(fieldAccess.infix("in", cond))
    elif i < n.len - 1:
      setResult(fieldAccess.infix("==", cond))

  if result.kind != nnkNilLit:
    result = nnkPar.newTree(result)


func fieldConditions(obj, list: NimNode, field: NimName or string, cond: NimNode): NimNode =
  ## Retrieves conditions that must be true to access `field`
  if list.kind == nnkIdentDefs:
    for name in list.identDef.names:
      if name == field:
        return cond
  for n in list:
    case n.kind:
    of nnkIdentDefs:
      for name in n.identDef.names:
        if name == field:
          return cond
    of nnkRecCase:
      let fieldToCheck = n[0][0]
      var elseCond: NimNode
      for branch in n:
        var newCond = copyNimTree(cond)
        case branch.kind
        of nnkIdentDefs:
          let val = fieldConditions(obj, branch, field, newCond)
          if val.kind != nnkNilLit:
            return val
        of nnkOfBranch:
          newCond = infix(newCond, "and", branch.makeAccessCond(obj, fieldToCheck))
          if elseCond.kind == nnkNilLit:
            elseCond = prefix(newCond, "not")
          else:
            elseCond = infix(elseCond, "and", newCall("not", newCond))
        of nnkElse:
          newCond = infix(cond, "and", elseCond)
        else: discard
        if branch.kind != nnkIdentDefs:
          let val = fieldConditions(obj, branch[^1], field, newCond)
          if val.kind != nnkNilLit:
            return val

    else: discard

func fieldConditions*(obj: NimNode, field: NimName or string): NimNode =
  ## Retrieves conditions that must be true to access `field`
  fieldConditions(obj, obj.objectDef.recList, field, newLit(true))

## Todo handle fields inside recCase and reclist

func addField*(obj: ObjectDef, field: IdentDef) =
  ## Adds identdef `field` to `obj`'s top level reclist
  let
    n = NimNode obj
  case obj.recList.kind
  of nnkRecCase, nnkRecWhen:
    obj.recList = nnkRecList.newTree(obj.recList, NimNode field)
  of nnkEmpty:
    obj.recList = nnkRecList.newTree(NimNode field)
  else:
    obj.recList.add NimNode(field)

func delField*(obj: ObjectDef, fieldName: string) =
  ## Deletes field named `fieldName`
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
  ## Gets all the branch fields in a def
  ## should do it iteratively instead of storing in a seq
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

iterator inheritObjs*(obj: ObjectDef): ObjectDef =
  ## Walks up the `obj`'s inheritance tree at the object that inherits from RootObj`
  var parent = obj.inheritObj
  while parent.isSome:
    yield parent.get
    parent = parent.get.inheritObj

iterator fields*(obj: ObjectDef): IdentDef =
  ## Iterates all fields including parent fields of `obj`
  for def in obj.recList:
    case def.kind
    of nnkIdentDefs:
      yield IdentDef def
    of nnkRecCase:
      for fields in collectBranchFields(def):
        yield fields
    else: discard
  for parent in obj.inheritObjs:
    for iDef in collectBranchFields(parent.reclist):
      yield iDef

iterator pragmas*(obj: ObjectDef): PragmaVal =
  ## Iterates all pragmas applied to `obj`
  case obj.NimNode[0].kind
  of nnkPragmaExpr:
    for i in 1..<obj.NimNode[0].len:
      yield PragmaVal obj.NimNode[0][i]
  else: discard

iterator genericParams*(obj: ObjectDef): IdentDef =
  ## Iterates all generic parameters of `obj`
  for def in NimNode(obj)[1]:
    yield IdentDef def
