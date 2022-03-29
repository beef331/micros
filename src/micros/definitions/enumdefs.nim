import micros/[utils, nimnodes, enumfields]
import std/enumerate

func isa*(n: NimNode, _: typedesc[EnumDef]): bool =
  n.checkit {nnkTypeDef, nnkEnumTy}
  n.checkIt ^1, {nnkEnumTy}

func enumdef*(n: NimNode): EnumDef =
  ## Ensures `n` isa `EnumDef` and then converts to it.
  let n =
    case n.kind
    of nnkSym:
      case n.symKind
      of nskType:
        n.getImpl()
      of VariableSyms, nskEnumField:
        n.getTypeInst().getImpl
      else:
        error("Invalid Enum Symbol: '" & $n.symKind & "'.", n)
        return
    else:
      n
  n.checkConv EnumDef

func enumDef*(name: string or NimName, fields: seq[NimNode]): EnumDef =
  ## Generates a new `enumDef` with `name` and `fields`
  let name =
    when name is string:
      ident name
    else:
      NimNode name

  EnumDef newEnum(name, fields, false, false)

iterator fields*(enmDf: EnumDef): EnumField =
  ## Iterates all the fields of `enmDf`
  let n = NimNode enmDf
  for i, x in enumerate n[^1]:
    if i > 0:
      yield enumField x
