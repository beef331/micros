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
  CaseStmt* = distinct NimNode
  IfStmt* = distinct NimNode
  ElIfBranch* = distinct NimNode
  ElseBranch* = distinct NimNode
  OfBranch* = distinct NimNode
  WhileLoop* = distinct NimNode
  ForLoop* = distinct NimNode
  EnumField* = distinct NimNode

  VarDefs* = VarDef or LetDef or ConstDef

  NimName* = distinct NimNode

  StmtList* = distinct NimNode
  StmtSubTypes* = NimNode or DistinctNimNode
  TypeDefs* = EnumDef or ObjectDef
  DistinctNimNode* = concept d
    d.distinctBase is NimNode
  NimNodes* = DistinctNimNode or NimNode

const
  VariableSyms* = {nskForVar, nskParam, nskVar, nskConst, nskLet, nskResult, nskTemp, nskField}

func exported*(name: string or NimNode): NimName =
  ## Creates an exported NimName
  let name =
    when name is string:
      ident name
    else:
      name
  NimName postFix(name, "*")

func toName*(n: NimName or string): NimName =
  when n is string:
    NimName ident(n)
  else:
    n

export macros

func repr*(n: DistinctNimNode): string = system.repr(NimNode(n))

func `$`*(n: NimName): string {.borrow.}

func `==`*(n: Nimname, name: string): bool = n.NimNode.eqIdent name
func `==`*(n, name: Nimname): bool = n.NimNode.eqIdent name.NimNode

func add*(n: NimNode, d: DistinctNimNode) = n.add NimNode d

func copy*(n: DistinctNimNode): typeof(n) = typeOf(n) NimNode(n).copyNimTree
