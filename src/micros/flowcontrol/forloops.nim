import micros/[utils, nimnodes]
import std/[macros, enumerate]

const forParamKinds = {nnkIdent, nnkSym, nnkVarTuple}

func isa*(n: NimNode, _: typedesc[ForLoop]): bool =
  n.checkit {nnkForStmt}
  n.checkit 0..^3, forParamKinds

func forloop*(n: NimNode): ForLoop =
  ## Ensures `n` isa `FoorLoop` and then converts to it.
  n.checkConv ForLoop

func forLoop*(params, iter, body: Nimnode): ForLoop =
  ForLoop nnkForStmt.newTree(params, iter, body)

func add*(forLoop: ForLoop, name: NimName) =
  ## Adds a for loop variable named `name` to `forLoop`.
  let n = NimNode forLoop
  n.insert n.len - 3, NimNode name # Place this after all other parameters

func delete*(forLoop: ForLoop, name: NimName) =
  ## Removes a forloop variable named `name` from `forLoop`.
  for i, node in enumerate forLoop[0..^3]:
    if node.eqIdent NimNode name:
      forLoop.NimNode.del(i, 1)
      break

func `body=`*(forLoop: ForLoop, body: NimNode) =
  ## Sets the body of `forLoop` to `body`.
  forLoop.NimNode[^1] = body

iterator variables*(forLoop: ForLoop): NimName =
  ## Yields all for loop variables of `forLoop`
  for i, node in enumerate forLoop[0..^3]:
    case node.kind
    of nnkIdent, nnkSym:
      yield NimName node
    of nnkVarTuple:
      for j, name in enumerate node[0..^2]:
        yield NimName name
    else:
      error("Invalid for loop parameter.", node)
