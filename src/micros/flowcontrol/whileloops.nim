import micros/[utils, nimnodes]
import std/macros


func `of`*(n: NimNode, _: typedesc[WhileLoop]): bool =
  n.checkit {nnkWhileStmt}
  n.checkit 0..1, {nnkStmtList}

func whileLoop*(n: NimNode): WhileLoop = n.checkConv WhileLoop

func whileLoop*(cond, body: Nimnode): WhileLoop =
  WhileLoop nnkWhileStmt.newTree(cond, body)

func cond*(whileLoop: WhileLoop): NimNode = whileLoop.NimNode[0]

func `cond=`*(whileLoop: WhileLoop, cond: NimNode) = whileLoop.NimNode[0] = cond

func body*(whileLoop: WhileLoop): NimNode = whileLoop.NimNode[1]

func `body=`*(whileLoop: WhileLoop, body: NimNode) = whileLoop.NimNode[1] = body
