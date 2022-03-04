import nimnodes

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
