import micros/[utils, nimnodes]

func `name=`*(obj: TypeDefs, newName: NimName or string) =
  ## Sets the `name` of the `obj` typedef
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

func `name`*(obj: TypeDefs): NimName =
  ## Retrieves the `name` of the `obj` typedef
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
