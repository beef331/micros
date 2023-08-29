## This is a module that implements easy to use abstractions over macros
## If reflection happens at runtime, introspection happens at compile time
import std/macros

macro returnType*(p: proc): untyped =
  ## Gets the return type of a procedure
  p.getTypeInst[0][0]

macro paramTypeAt*(p: proc, ind: static int): untyped =
  ## Gets a parameter at a specific index, returns void if not found
  let params = p.getTypeInst[0]
  if ind in 0..<params.len:
    params[ind + 1][^2]
  else:
    ident"void"

macro paramsAsTuple*(p: proc): untyped =
  ## Creates a tuple typedesc for the parameters of a given procedure
  result = nnkTupleConstr.newTree()
  for x in p.getTypeInst()[0][1..^1]:
    result.add x[^2]

macro unpackTuple*(p: proc, t: tuple): untyped =
  ## Attempts to invoke a procedure with a given tuple, akin to macros.unpackVarargs
  result = newCall(p)
  for i, _ in t.getTypeInst():
    result.add nnkBracketExpr.newTree(t, newLit i)

