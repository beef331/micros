import micros/introspection
import std/unittest

proc doThing(a, b: int): string = discard
proc generic[T](a: T): seq[T] = discard

suite "Introspection":
  test "doThing":
    check doThing.paramTypeAt(3) is void
    check doThing.paramTypeAt(0) is int
    check doThing.paramTypeAt(0) is int
    check doThing.returnType is string
    check doThing.paramsAsTuple() is (int, int)
    check doThing.unpackTuple((10, 20)) == ""

  test "generic":
    check generic[int].paramTypeAt(0) is typeof int
    check generic[int].returnType is seq[int]
    check generic[int].paramsAsTuple is (int, )
    check generic[int].unpackTuple((1,)) == newSeq[int]()
