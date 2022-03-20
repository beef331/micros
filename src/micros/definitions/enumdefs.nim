import micros/[utils, nimnodes, enumfields]
import std/enumerate
func isa*(n: NimNode, _: typedesc[EnumDef]): bool =
  n.checkit {nnkTypeDef, nnkEnumTy}
  n.checkIt ^1, {nnkEnumTy}

func enumdef*(n: NimNode): EnumDef =
  let n =
    case n.kind
    of nnkSym:
      case n.symKind
      of nskType:
        n.getImpl()
      else:
        n.getTypeInst.getImpl
    else:
      n
  n.checkConv EnumDef

func enumDef*(name: string or NimName, fields: seq[NimNode]): EnumDef =
  let name =
    when name is string:
      ident name
    else:
      NimNode name

  EnumDef newEnum(name, fields, false, false)


iterator fields*(enmDf: EnumDef): EnumField =
  let n = NimNode enmDf
  for i, x in enumerate n[^1]:
    if i > 0:
      yield enumField x
