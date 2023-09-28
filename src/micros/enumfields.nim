import utils, nimnodes
import std/options

func isa*(n: NimNode, _: typedesc[EnumField]): bool =
  n.checkIt {nnkEnumFieldDef, nnkIdent, nnkSym}

func enumField*(n: NimNode): EnumField =
  ## Ensures `n` isa `EnumField` and then converts to it.
  n.checkConv EnumField

func enumField*(s: string, val: Option[SomeOrdinal]): EnumField =
  ## Generates a new `EnumField` named `s` with value `val`.
  let name = ident s
  if val.isSome:
    enumField nnkEnumFieldDef.newTree(name, newLit val)
  else:
    enumField name

func val*(ef: EnumField): NimNode =
  ## Retrieves the value of `ef`,
  ## assertion is raised if it does not have one.
  assert NimNode(ef).kind == nnkEnumFieldDef
  NimNode(ef)[^1]

func name*(ef: EnumField): NimName =
  ## Retrieves the name of `ef`.
  let n = NimNode ef
  assert n.kind in {nnkEnumFieldDef, nnkSym, nnkIdent}
  if n.kind == nnkEnumFieldDef:
    NimName n[0]
  else:
    NimName n
