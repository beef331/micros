import std/[macros, typetraits]
type
  RoutineNode* = distinct NimNode
  IdentDef* = distinct NimNode
  VarDef* = distinct NimNode
  LetDef* = distinct NimNode
  ConstDef* = distinct NimNode
  ObjectDef* = distinct NimNode
  EnumDef* = distinct NimNode
  PragmaVal* = distinct NimNode

  VarDefs* = VarDef or LetDef or ConstDef

  NimName* = distinct NimNode

  StmtList* = distinct NimNode
  StmtSubTypes* = VarDefs or StmtList or RoutineNode
  DistinctNimNode* = concept d
    d.distinctBase is NimNode
  NimNodes* = DistinctNimNode or NimNode

func exported*(name: string or NimNode): NimName =
  let name =
    when name is string:
      ident name
    else:
      name
  NimName postFix(name, "*")

export macros

func repr*(n: DistinctNimNode): string = system.repr(NimNode(n))

func `$`*(n: NimName): string {.borrow.}

func `==`*(n: Nimname, name: string): bool = n.NimNode.eqIdent name
func `==`*(n, name: Nimname): bool = n.NimNode.eqIdent name.NimNode

func add*(n: NimNode, d: DistinctNimNode) = n.add NimNode d

func copy*(n: DistinctNimNode): typeof(n) = typeOf(n) NimNode(n).copyNimTree
