import micros/[nimnodes, utils]
import micros/definitions/identdefs
import micros/[pragmas, nimnames]
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

func varDef*(n: NimNode): VarDef = n.checkConv VarDef

func letDef*(n: NimNode): LetDef = n.checkConv LetDef

func constDef*(n: NimNode): ConstDef = n.checkConv ConstDef


func varStmt*(name: string or NimName, val: auto): VarDef =
  let name =
    when name is NimName:
      assert NimNode(name).kind in {nnkIdent, nnkSym}
      NimNode name
    else:
      ident name

  when val is NimNode:
    VarDef newVarStmt(name, val)
  else:
    VarDef newVarStmt(name, newLit val)

func letStmt*(name: string or NimName, val: auto): LetDef =
  let name =
    when name is NimName:
      assert NimNode(name).kind in {nnkIdent, nnkSym}
      NimNode name
    else:
      ident name

  when val is NimNode:
    LetDef newLetStmt(name, val)
  else:
    LetDef newLetStmt(name, newLit val)

iterator identdefs*(def: VarDefs): IdentDef =
  for idef in NimNode def:
    yield identDef(idef)

func add*(def: VarDefs, prag: PragmaVal) =
  for idef in def.identdefs:
    for i, name in enumerate idef.names:
      idef.NimNode[i] = NimNode name.copyWithPragma prag

