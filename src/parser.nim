import std/strutils
import std/tables
import std/sequtils
import std/options
import defs
import filelike

proc isWhite(x: char): bool = " \b\r\n\t".contains(x)
proc isDigit(x: char): bool =
  let o = x.ord
  '0'.ord <= o and o <= '9'.ord

proc errorWithReason*(ps: Filelike, x: string): void =
  raise newException(ValueError, ps.name & "(" & $ps.line & ":" & $ps.col & "): " & x)

proc tryPeekChar(x: var Filelike): Option[char] =
  try:
    let ch = x.peekChar()
    return some(ch)
  except:
    return none(char)

proc tryReadChar(x: var Filelike): Option[char] =
  try:
    let ch = x.readChar()
    return some(ch)
  except:
    return none(char)

proc skipWhite(x: var Filelike): void =
  while x.tryPeekChar().isSome():
    if x.tryPeekChar().get().isWhite:
      discard x.tryReadChar()
      continue
    else:
      break
  return

proc textEnded(x: var Filelike): bool = x.tryPeekChar.isNone()

proc skipComment(x: var Filelike): void =
  x.skipWhite
  if x.textEnded: return
  if x.tryPeekChar().get() != ';': return
  discard x.tryReadChar()
  while x.tryPeekChar().isSome():
    if x.tryPeekChar().get() != '\n':
      discard x.tryReadChar()
      continue
    else:
      discard x.tryReadChar()
      break

proc hasWhiteOrComment(x: var Filelike): bool =
  if x.textEnded: return false
  let ch = x.tryPeekChar()
  if ch.isNone(): return false
  return ch.get().isWhite() or ch.get() == ';'

proc skipWhiteAndComment(x: var Filelike): void =
  x.skipWhite
  while true:
    x.skipComment
    x.skipWhite
    if not x.hasWhiteOrComment: return

proc takeInteger(x: var Filelike): int =
  x.skipWhiteAndComment
  var s = ""
  while x.tryPeekChar().isSome():
    if x.tryPeekChar().get().isDigit():
      s.add(x.tryReadChar().get())
      continue
    else:
      break
  return s.parseInt

proc takeWord(x: var Filelike): string =
  x.skipWhiteAndComment
  var s = ""
  while x.tryPeekChar().isSome():
    let ch = x.tryPeekChar().get()
    if ch.isAlphaAscii() or ch == '#':
      s.add(ch)
      discard x.tryReadChar()
      continue
    else:
      break
  return s

proc nextChar(x: var Filelike): void =
  discard x.tryReadChar()

proc nextCharExistsAndIs(x: var Filelike, c: char): bool =
  x.tryPeekChar().isSome() and x.tryPeekChar().get() == c
proc nextCharExistsAndIsNot(x: var Filelike, c: char): bool =
  x.tryPeekChar().isSome() and x.tryPeekChar().get() != c
  
# NOTE: these two modifies the argument.
proc parseMultiNode*(x: var Filelike): seq[Node]
proc parseSingleNode*(x: var Filelike): Node =
  x.skipWhiteAndComment
  if x.textEnded: return nil
  let line = x.line
  let col = x.col
  let firstChar = x.tryPeekChar().get()
  if firstChar == '(':
    x.nextChar
    let lVal = parseMultiNode(x)
    var tail: Node = nil
    if x.hasWhiteOrComment:
      x.skipWhiteAndComment
    if not x.textEnded and x.nextCharExistsAndIs('.'):
      x.nextChar
      x.skipWhiteAndComment
      tail = x.parseSingleNode
      x.skipWhiteAndComment
    if x.textEnded or x.nextCharExistsAndIsNot(')'):
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
    if not x.textEnded and x.nextCharExistsAndIs('@'):
        x.nextChar
        resword = ",@"
    var res = mkWordNode(resword)
    return res.withPos(line, col)
  elif firstChar.isDigit:
    var res = mkIntegerNode(x.takeInteger)
    return res.withPos(line, col)
  else:
    let v = x.takeWord()
    if v.len <= 0: return nil
    var res = mkWordNode(v)
    return res.withPos(line, col)

proc parseMultiNode*(x: var Filelike): seq[Node] =
  var res: seq[Node] = @[]
  x.skipWhiteAndComment
  while not x.textEnded and x.nextCharExistsAndIsNot(')') and x.nextCharExistsAndIsNot('.'):
    let sr = x.parseSingleNode
    if sr == nil: break
    res.add(sr)
    x.skipWhiteAndComment
  return res
  
