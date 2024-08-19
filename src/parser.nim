import std/strutils
import std/tables
import std/sequtils
import defs

proc isWhite(x: char): bool = " \b\r\n\t".contains(x)
proc isDigit(x: char): bool =
  let o = x.ord
  '0'.ord <= o and o <= '9'.ord

type
  ParseState = ref object
    x: string
    i: int
    lenx: int
    filename: string
    line: int
    col: int

proc `$`(x: ParseState): string =
  "ParseState(" & x.x & "," & $x.i & ")"

proc mkParseState*(x: string): ParseState = ParseState(x: x, i: 0, lenx: x.len, filename: "")
proc withFilename*(ps: ParseState, fn: string): ParseState =
  ps.filename = fn
  return ps

proc errorWithReason*(ps: ParseState, x: string): void =
  raise newException(ValueError, ps.filename & "(" & $ps.line & ":" & $ps.col & "): " & x)

proc skipWhite(x: ParseState): ParseState =
  var i = x.i
  let lenx = x.lenx
  while i < lenx and x.x[i].isWhite:
    if x.x[i] == '\n':
      x.line += 1
      x.col = 0
    else:
      x.line += 1
    i += 1
  x.i = i
  return x

proc textEnded(x: ParseState): bool = x.i >= x.lenx

proc skipComment(x: ParseState): ParseState =
  let x = x.skipWhite
  if x.textEnded: return x
  if x.x[x.i] != ';': return x
  var i = x.i
  while i < x.lenx and x.x[i] != '\n':
    i += 1
  if i < x.lenx: i += 1
  x.i = i
  return x

proc nextChar(x: ParseState): void =
  if x.textEnded: return
  if x.x[x.i] == '\n':
    x.line += 1
    x.col = 0
  else:
    x.col += 1
  x.i += 1

proc hasWhiteOrComment(x: ParseState): bool =
  not x.textEnded and (x.x[x.i].isWhite or x.x[x.i] == ';')

proc skipWhiteAndComment(x: ParseState): ParseState =
  var x = x.skipWhite
  while true:
    x = x.skipComment.skipWhite
    if x.textEnded or x.x[x.i] != ';': return x

proc takeInteger(x: ParseState): int =
  var x = x.skipWhiteAndComment
  var i = x.i
  while i < x.lenx and x.x[i].isDigit:
    i += 1
    x.col += 1
  let iVal = x.x[x.i..<i]
  x.i = i
  return iVal.parseInt

proc takeWord(x: ParseState): string =
  var x = x.skipWhiteAndComment
  var i = x.i
  while i < x.lenx and (x.x[i].isAlphaAscii() or x.x[i] == '#'):
    i += 1
    x.col += 1
  let iVal = x.x[x.i..<i]
  x.i = i
  return iVal
  
# NOTE: these two modifies the argument.
proc parseMultiNode*(x: var ParseState): seq[Node]
proc parseSingleNode*(x: var ParseState): Node =
  var x = x.skipWhiteAndComment
  if x.textEnded: return nil
  let line = x.line
  let col = x.col
  let firstChar = x.x[x.i]
  if firstChar == '(':
    x.nextChar
    let lVal = parseMultiNode(x)
    var tail: Node = nil
    if x.hasWhiteOrComment:
      discard x.skipWhiteAndComment
    if not x.textEnded and x.x[x.i] == '.':
      x.nextChar
      discard x.skipWhiteAndComment
      tail = x.parseSingleNode
      discard x.skipWhiteAndComment
    if x.textEnded or x.x[x.i] != ')':
      x.errorWithReason("Right parenthesis required.")
    x.nextChar
    var res = mkListNode(lVal, tail)
    return res.withPos(line, col)
  elif firstChar == '\'':
    x.nextChar
    var res = mkWordNode("'")
    return res.withPos(line, col)
  elif firstChar == '`':
    x.nextChar
    var res = mkWordNode("`")
    return res.withPos(line, col)
  elif firstChar == ',':
    x.nextChar
    var resword = ","
    if not x.textEnded:
      if x.x[x.i] == '@':
        x.nextChar
        resword = ",@"
    var res = mkWordNode(resword)
    return res.withPos(line, col)
  elif firstChar.isDigit:
    var res = mkIntegerNode(x.takeInteger)
    return res.withPos(line, col)
  else:
    var res = mkWordNode(x.takeWord)
    return res.withPos(line, col)

proc parseMultiNode*(x: var ParseState): seq[Node] =
  var res: seq[Node] = @[]
  var x = x.skipWhiteAndComment
  while not x.textEnded and x.x[x.i] != ')' and x.x[x.i] != '.':
    let sr = x.parseSingleNode
    if sr == nil: break
    res.add(sr)
    discard x.skipWhiteAndComment
  return res
  
