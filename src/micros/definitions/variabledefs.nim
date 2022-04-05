import micros/[nimnodes, utils, nimnames]
import micros/definitions/identdefs
import std/enumerate

func isa*(n: NimNode, T: typedesc[VarDefs]): bool =
  n.checkit:
    when T is VarDef:
      {nnkVarSection}
    elif T is LetDef:
      {nnkLetSection}
    else:
      {nnkConstSection}
  n.checkit 0..^1,
    when T is ConstDef:
      {nnkConstdef}
    else:
      {nnkIdentdefs}

func varDef*(n: NimNode): VarDef =
  ## Ensures `n` isa `VarDef` and then converts to it.
  n.checkConv VarDef

func letDef*(n: NimNode): LetDef =
  ## Ensures `n` isa `LetDef` and then converts to it.
  n.checkConv LetDef

func constDef*(n: NimNode): ConstDef =
  ## Ensures `n` isa `ConstDef` and then converts to it.
  n.checkConv ConstDef


func varStmt*(name: string or NimName, val: auto): VarDef =
  ## Generates a `VarDef` as `var name = val`.
  let name =
    when name is NimName:
      NimNode name
    else:
      ident name

  when val is NimNode:
    VarDef newVarStmt(name, val)
  else:
    VarDef newVarStmt(name, newLit val)

func letStmt*(name: string or NimName, val: auto): LetDef =
  ## Generates a `LetDef` as `let name = val`.
  let name =
    when name is NimName:
      NimNode name
    else:
      ident name

  when val is NimNode:
    LetDef newLetStmt(name, val)
  else:
    LetDef newLetStmt(name, newLit val)

iterator identdefs*(def: LetDef or VarDef): IdentDef =
  ## Iterates all identdefs inside the `def`.
  for idef in NimNode def:
    yield identDef(idef)

iterator names*(def: LetDef or VarDef): NimName =
  ## Iterates all name declarations inside the `def`.
  for idef in NimNode def:
    for name in idef.names:
      yield name

func add*(def: VarDefs, prag: PragmaVal) =
  ## Adds `prag` to all variables declared inside `def`.
  for idef in def.identdefs:
    for i, name in enumerate idef.names:
      idef.NimNode[i] = NimNode name.copyWithPragma prag

