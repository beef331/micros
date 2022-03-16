import micros/nimnodes
import micros/flowcontrol/[casestmts, ofbranches, elifbranches, elsebranches]
import std/[macros, sugar, genasts]
{.experimental: "caseStmtMacros".}

macro `case`*(n: NimNode): untyped =
  let
    myStmt = caseStmt(n)
    discrim = myStmt.discriminator
  if myStmt.discriminator.kind notin {nnkSym, nnkIdent}:
    error("Expected symbol or identifier for discriminator", discrim)
  result = nnkIfStmt.newTree()
  for branch in myStmt.branches:
    if branch.isa OfBranch:
      let branch = ofBranch branch
      for cond in branch.conditions:
        result.add:
          elifBranch:
            genAst(cond, discrim):
              isa(discrim,cond)
          do:
            genast(typ = cond, body = branch.stmtList.NimNode, name = ident $discrim):
              static: assert typ is DistinctNimNode
              let name = typ name
              body
    elif branch.isa(ElseBranch) or branch.isa(ElifBranch):
      result.add branch
