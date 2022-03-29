import micros/[utils, nimnodes]
import std/macros


func isa*(n: NimNode, _: typedesc[WhileLoop]): bool =
  n.checkit {nnkWhileStmt}
  if n.len != 2:
    return false

func whileLoop*(n: NimNode): WhileLoop =
  ## Ensures `n` isa `RoutineNode` and then converts to it.
  n.checkConv WhileLoop

func whileLoop*(cond, body: Nimnode): WhileLoop =
  ## Generates a new `WhileLoop` with like `while cond: body`.
  WhileLoop nnkWhileStmt.newTree(cond, body)

func cond*(whileLoop: WhileLoop): NimNode =
  ## Retrieves the condition of `whileLoop`.
  whileLoop.NimNode[0]

func `cond=`*(whileLoop: WhileLoop, cond: NimNode) =
  ## Sets the condition of the `whileLoop` to `cond`.
  whileLoop.NimNode[0] = cond

func body*(whileLoop: WhileLoop): NimNode =
  ## Retrieves the body of `whileLoop`.
  whileLoop.NimNode[1]

func `body=`*(whileLoop: WhileLoop, body: NimNode) =
  ## Sets the body of `whileLoop` to `body`.
  whileLoop.NimNode[1] = body
