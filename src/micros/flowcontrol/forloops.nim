import micros/[utils, nimnodes]
import std/[macros, enumerate]

const forParamKinds = {nnkIdent, nnkSym, nnkVarTuple}

func `of`*(n: NimNode, _: typedesc[ForLoop]): bool =
  n.checkit {nnkForStmt}
  n.checkit 0..^2, forParamKinds
  n.checkIt ^1, {nnkStmtList}

func forloop*(n: NimNode): ForLoop = n.checkConv ForLoop

func forLoop*(params, iter, body: Nimnode): ForLoop =
  ForLoop nnkForStmt.newTree(params, iter, body)

func add*(forLoop: ForLoop, name: NimName) =
  let n = NimNode forLoop
  n.insert n.len - 3, NimNode name # Place this after all other parameters

func delete*(forLoop: ForLoop, name: NimName) =
  for i, node in enumerate forLoop[0..^3]:
    if node.eqIdent NimNode name:
      forLoop.NimNode.del(i, 1)
      break

func `body=`(forLoop: ForLoop, body: NimNode) = forLoop.NimNode[^1] = body

iterator parameters*(forLoop: ForLoop): NimName =
  for i, node in enumerate forLoop[0..^3]:
    case node.kind
    of nnkIdent, nnkSym:
      yield NimName node
    of nnkVarTuple:
      for j, name in enumerate node[0..^2]:
        yield NimName name
    else:
      error("Invalid for loop parameter.", node)
