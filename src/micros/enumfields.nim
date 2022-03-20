import micros/[utils, nimnodes]
import std/options

func isa*(n: NimNode, _: typedesc[EnumField]): bool =
  n.checkIt {nnkEnumFieldDef, nnkIdent, nnkSym}

func enumField*(n: NimNode): EnumField = n.checkConv EnumField

func enumField*(s: string, val: Option[SomeOrdinal]): EnumField =
  let name = ident s
  if val.isSome:
    enumField nnkEnumFieldDef.newTree(name, newLit val)
  else:
    enumField name

func val*(ef: EnumField): NimNode =
  assert NimNode(ef).kind == nnkEnumFieldDef
  NimNode(ef)[^1]

func name*(ef: EnumField): NimName =
  let n = NimNode ef
  assert n.kind in {nnkEnumFieldDef, nnkSym, nnkIdent}
  if n.kind == nnkEnumFieldDef:
    NimName n[0]
  else:
    NimName n
