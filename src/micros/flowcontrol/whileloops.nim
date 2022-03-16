import micros/[utils, nimnodes]
import std/macros


func isa*(n: NimNode, _: typedesc[WhileLoop]): bool =
  n.checkit {nnkWhileStmt}
  if n.len != 2:
    return false

func whileLoop*(n: NimNode): WhileLoop = n.checkConv WhileLoop

func whileLoop*(cond, body: Nimnode): WhileLoop =
  WhileLoop nnkWhileStmt.newTree(cond, body)

func cond*(whileLoop: WhileLoop): NimNode = whileLoop.NimNode[0]

func `cond=`*(whileLoop: WhileLoop, cond: NimNode) = whileLoop.NimNode[0] = cond

func body*(whileLoop: WhileLoop): NimNode = whileLoop.NimNode[1]

func `body=`*(whileLoop: WhileLoop, body: NimNode) = whileLoop.NimNode[1] = body
