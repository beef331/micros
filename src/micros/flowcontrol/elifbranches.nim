import micros/[nimnodes, utils]
import std/macros

func isa*(n: NimNode, _: typedesc[ElifBranch]): bool =
  n.checkit {nnkElifBranch}

func elifBranch*(n: NimNode): ElifBranch = n.checkConv ElifBranch

func elifBranch*(n, body: NimNode): ElifBranch =
  elifBranch nnkElifBranch.newTree(n, body)

func condition*(elifBranch: ElifBranch): NimNode = elifBranch.NimNode[0]

func stmtList*( elifBranch: ElifBranch): NimNode = elifBranch.NimNode[1]
