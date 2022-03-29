import micros/[nimnodes, utils]

func isa*(node: NimNode, _: typedesc[IdentDef]): bool =
  node.checkit({nnkIdentDefs})
  node.checkit(0..^3, {nnkIdent, nnkSym})

func identDef*(n: NimNode): IdentDef =
  ## Ensures `n` isa `IdentDef` and then converts to it.
  n.checkConv IdentDef

func identDefTyp(name: string, typ: typedesc): IdentDef =
  ## Generates an `IdentDef` of `name: typ`
  IdentDef newIdentDefs(ident name, typ.getType)

func identDefVal(name: string, val: not typedesc and not NimNode): IdentDef =
  ## generates a `IdentDef` of `name = val`
  IdentDef newIdentDefs(ident name, newEmptyNode(), newLit(val))

func isSingle*(iDef: IdentDef): bool =
  ## Is this `IdentDef` a single declaration
  NimNode(iDef).len == 3

# Some reason these exist i'll remember eventuall
template identDef*(name: string, typ: typedesc and not NimNode): untyped = identDefTyp(name, typ)

template identDef*(name: string, val: not typedesc and not NimNode): untyped = identDefVal(name, val)

func identDefTyp*(name: string, typ: NimNode): IdentDef =
  ## Generates an `IdentDef`with a type of `typ`
  identDef nnkIdentDefs.newTree(ident(name),newEmptyNode(), typ)

func identDefVal*(name: string, val: NimNode): IdentDef =
  ## Generates an `IdentDef` with a value of `val`
  identDef nnkIdentDefs.newTree(ident(name), val, newEmptyNode())

func typ*(n: IdentDef): NimNode =
  ## Retrieves the type of `n`
  NimNode(n)[^2]

func `typ=`*(n: IdentDef, newTyp: NimNode): NimNode =
  ## Sets the `type` of `n`
  NimNode(n)[^2] = newTyp

func val*(n: IdentDef): NimNode =
  ## Retrieves the value of `n`
  NimNode(n)[^1]

func name*(n: IdentDef, ind = 0): NimName =
  ## Returns the first name of `n`, should be used with `isSingle`
  NimName NimNode(n)[ind]

func addVar*(n: IdentDef, name: NimNode or string) =
  ## Adds another declaration named `name` to `n`
  when name is NimNode:
    NimNode(n).insert(n.len - 2, name)
  else:
    NimNode(n).insert(n.len - 2, ident name)

iterator names*(idef: IdentDef): NimName =
  ## Iterates all names inside `idef`.
  let n = NimNode(idef)
  for x in 0..<n.len - 2:
    yield NimName n[x]
