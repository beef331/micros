import nimnodes

func identDef*(n: NimNode): IdentDef =
  assert n.kind == nnkIdentDefs
  IdentDef n

func identDefTyp(name: string, typ: typedesc): IdentDef =
  IdentDef newIdentDefs(ident name, typ.getType)

func identDefVal(name: string, val: auto): IdentDef =
  IdentDef newIdentDefs(ident name, newEmptyNode(), newLit(val))

func isSingle*(iDef: IdentDef): bool = NimNode(iDef).len == 3

template identDef*(name: string, typ: typedesc): untyped = identDefTyp(name, typ)

template identDef*(name: string, val: not typedesc): untyped = identDefVal(name, val)

func typ*(n: IdentDef): NimNode = NimNode(n)[^2]

func `typ=`*(n: IdentDef, newTyp: NimNode): NimNode = NimNode(n)[^2] = newTyp

func val*(n: IdentDef): NimNode = NimNode(n)[^1]

func name*(n: IdentDef, ind = 0): NimName = NimName NimNode(n)[ind]

func addVar*(n: IdentDef, name: NimNode or string) =
  when name is NimNode:
    NimNode(n).insert(n.len - 2, name)
  else:
    NimNode(n).insert(n.len - 2, ident name)

iterator names*(idef: IdentDef): NimName =
  let n = NimNode(idef)
  for x in 0..<n.len - 2:
    yield NimName n[x]
