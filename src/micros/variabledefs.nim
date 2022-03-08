import nimnodes, utils

func `of`*(n: NimNode, T: typedesc[VarDefs]): bool =
  n.checkit:
    when T is VarDef:
      {nnkVarSection}
    elif T is LetDef:
      {nnkLetSection}
    else:
      {nnkConstSection}
  n.checkit 0..^1, {nnkIdentdefs}



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

## Todo implemenet const method and support tuples
