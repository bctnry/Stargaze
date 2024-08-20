import std/strutils
import std/tables
import std/sequtils
import std/options
import defs
import filelike
import error
import aux

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
    if ch.isAlphaNumeric() or "#\\{}<>+=/%$^&*".contains(ch):
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

proc recognizeCharLiteral1(x: string): Node
proc recognizeCharLiteral2(x: string): Node
# NOTE: these two modifies the argument.
proc parseMultiNode*(x: var Filelike): seq[Node]
proc parseSingleNode*(x: var Filelike): Node =
  x.skipWhiteAndComment
  if x.textEnded: return nil
  let line = x.line
  let col = x.col
  let fn = x.name
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
    return res.withMetadata(line, col, fn)
  elif firstChar == '\'':
    x.nextChar
    var res = mkWordNode("'")
    return res.withMetadata(line, col, fn)
  elif firstChar == '`':
    x.nextChar
    var res = mkWordNode("`")
    return res.withMetadata(line, col, fn)
  elif firstChar == ',':
    x.nextChar
    var resword = ","
    if not x.textEnded and x.nextCharExistsAndIs('@'):
        x.nextChar
        resword = ",@"
    var res = mkWordNode(resword)
    return res.withMetadata(line, col, fn)
  elif firstChar.isDigit:
    var res = mkIntegerNode(x.takeInteger)
    return res.withMetadata(line, col, fn)
  elif firstChar == '"':
    var str = ""
    x.nextChar
    while x.tryPeekChar().isSome():
      let ch = x.tryPeekChar().get()
      if ch == '"':
        discard x.tryReadChar()
        var res = mkStrNode(str)
        return res.withMetadata(line, col, fn)
      elif ch == '\\':
        discard x.tryReadChar()
        if x.tryPeekChar().isNone():
          registerError("Invalid syntax for string literal")
          break
        let ch = x.tryReadChar().get()
        case ch:
          of 'x':
            var hexstr = ""
            while x.tryPeekChar().isSome():
              let ch = x.tryPeekChar().get()
              if not ch.isHexDigit: break
              discard x.tryReadChar()
              hexstr.add(ch)
            str.add(hexstr.hexToInt.chr)
          of 'r':
            str.add('\r')
          of 'n':
            str.add('\n')
          of 'b':
            str.add('\b')
          of 't':
            str.add('\t')
          of '\\':
            str.add('\\')
          of '"':
            str.add('\"')
          of 'v':
            str.add('\x0b')
          of 'f':
            str.add('\x0c')
          else:
            registerError("Invalid escape sequence in string literal")
            return nil
      else:
        str.add(ch)
        discard x.tryReadChar()
    registerError("Invalid syntax for string literal")
    return nil
  else:
    let v = x.takeWord()
    if v.len <= 0: return nil
    if v.startsWith("#\\"):
      var res = recognizeCharLiteral1(v)
      return res.withMetadata(line, col, fn)
    elif v.startsWith("#ch{"):
      if not v.endsWith("}"):
        registerError("Invalid syntax for character literal")
      else:
        var res = recognizeCharLiteral2(v)
        return res.withMetadata(line, col, fn)
    var res = mkWordNode(v)
    return res.withMetadata(line, col, fn)

const charLiteral1Map* = {
  "#\\NUL": '\x00',
  "#\\SOH": '\x01',
  "#\\STX": '\x02',
  "#\\ETX": '\x03',
  "#\\EOT": '\x04',
  "#\\ENQ": '\x05',
  "#\\ACK": '\x06',
  "#\\BEL": '\x07',
  "#\\BS": '\x08',
  "#\\HT": '\x09',
  "#\\LF": '\x0a',
  "#\\VT": '\x0b',
  "#\\FF": '\x0c',
  "#\\CR": '\x0d',
  "#\\SO": '\x0e',
  "#\\SI": '\x0f',
  "#\\DLE": '\x10',
  "#\\DC1": '\x11',
  "#\\DC2": '\x12',
  "#\\DC3": '\x13',
  "#\\DC4": '\x14',
  "#\\NAK": '\x15',
  "#\\SYN": '\x16',
  "#\\ETB": '\x17',
  "#\\CAN": '\x18',
  "#\\EM": '\x19',
  "#\\SUB": '\x1a',
  "#\\ESC": '\x1b',
  "#\\FS": '\x1c',
  "#\\GS": '\x1d',
  "#\\RS": '\x1e',
  "#\\US": '\x1f',
  "#\\DEL": '\x7f',
  "#\\esc": '\x1b',
  "#\\backspace": '\x08',
  "#\\linefeed": '\x0a',
  "#\\tab": '\x09',
  "#\\return": '\x0d',
  "#\\space": '\x20',
  "#\\newline": '\x0a'
}.toTable

proc recognizeCharLiteral1(x: string): Node =
  if x in charLiteral1Map:
    return mkCharNode(charLiteral1Map[x])
  else:
    return mkCharNode(x[2])

proc recognizeCharLiteral2(x: string): Node =
  let hex = x[4..<x.len-1]
  return mkCharNode(hex.hexToInt.chr)

proc parseMultiNode*(x: var Filelike): seq[Node] =
  var res: seq[Node] = @[]
  x.skipWhiteAndComment
  while not x.textEnded and x.nextCharExistsAndIsNot(')') and x.nextCharExistsAndIsNot('.'):
    let sr = x.parseSingleNode
    if sr == nil: break
    res.add(sr)
    x.skipWhiteAndComment
  return res
  
