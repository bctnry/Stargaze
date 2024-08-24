import std/strutils
import std/tables
import std/sequtils
import std/options
import aux

type
  NodeType* = enum
    N_WORD
    N_INTEGER
    N_FLOAT
    N_STRING
    N_CHAR
    N_LIST
    N_VECTOR
    N_EOF
  Node* = ref object
    line*: int
    col*: int
    filename*: string
    case nType*: NodeType
    of N_WORD:
      wVal*: string
    of N_INTEGER:
      iVal*: int
    of N_FLOAT:
      fVal*: float
    of N_STRING:
      strVal*: string
    of N_CHAR:
      chVal*: char
    of N_LIST:
      lVal*: seq[Node]
      tail*: Node
    of N_VECTOR:
      vVal*: seq[Node]
    of N_EOF:
      discard

const charLiteralMap = {
  '\x00': "#\\NUL",
  '\x01': "#\\SOH",
  '\x02': "#\\STX",
  '\x03': "#\\ETX",
  '\x04': "#\\EOT",
  '\x05': "#\\ENQ",
  '\x06': "#\\ACK",
  '\x07': "#\\BEL",
  '\x08': "#\\BS",
  '\x09': "#\\HT",
  '\x0a': "#\\LF",
  '\x0b': "#\\VT",
  '\x0c': "#\\FF",
  '\x0d': "#\\CR",
  '\x0e': "#\\SO",
  '\x0f': "#\\SI",
  '\x10': "#\\DLE",
  '\x11': "#\\DC1",
  '\x12': "#\\DC2",
  '\x13': "#\\DC3",
  '\x14': "#\\DC4",
  '\x15': "#\\NAK",
  '\x16': "#\\SYN",
  '\x17': "#\\ETB",
  '\x18': "#\\CAN",
  '\x19': "#\\EM",
  '\x1a': "#\\SUB",
  '\x1b': "#\\ESC",
  '\x1c': "#\\FS",
  '\x1d': "#\\GS",
  '\x1e': "#\\RS",
  '\x1f': "#\\US",
  '\x7f': "#\\DEL",
  '\x1b': "#\\esc",
  '\x08': "#\\backspace",
  '\x0a': "#\\linefeed",
  '\x09': "#\\tab",
  '\x0d': "#\\return",
  '\x20': "#\\space",
  '\x0a': "#\\newline", 
}.toTable
      
proc `$`*(x: Node): string =
  if x == nil: return "nil"
  case x.nType:
    of N_WORD:
      x.wVal
    of N_INTEGER:
      $x.iVal
    of N_FLOAT:
      $x.fVal
    of N_STRING:
      $x.strVal
    of N_CHAR:
      if x.chVal in charLiteralMap:
        return charLiteralMap[x.chVal]
      elif x.chVal <= '\x7f':
        return "#\\" & $x.chVal
      else:
        return "#ch{" & (x.chVal.ord.intToHex) & "}"
    of N_LIST:
      let tail = if x.tail != nil:
                   " . " & $x.tail
                 else:
                   ""
      "(" & x.lVal.mapIt($it).join(" ") & tail & ")"
    of N_VECTOR:
      "#{" & x.vVal.mapIt($it).join(" ") & "}"
    of N_EOF:
      "#eof"

proc mkWordNode*(wVal: string): Node = Node(line: -1, col: -1, filename: "", nType: N_WORD, wVal: wVal)
proc mkIntegerNode*(iVal: int): Node = Node(line: -1, col: -1, filename: "", nType: N_INTEGER, iVal: iVal)
proc mkFloatNode*(fVal: float): Node = Node(line: -1, col: -1, filename: "", nType: N_FLOAT, fVal: fVal)
proc mkCharNode*(chVal: char): Node = Node(line: -1, col: -1, filename: "", nType: N_CHAR, chVal: chVal)
proc mkStrNode*(strVal: string): Node = Node(line: -1, col: -1, filename: "", nType: N_STRING, strVal: strVal)
proc mkListNode*(lVal: seq[Node], tail: Node): Node = Node(line: -1, col: -1, filename: "", nType: N_LIST, lVal: lVal, tail: tail)
proc mkVectorNode*(vVal: seq[Node]): Node = Node(line: -1, col: -1, filename: "", nType: N_VECTOR, vVal: vVal)
proc mkEOFNode*(): Node = Node(line: -1, col: -1, filename: "", nType: N_EOF)
proc withPos*(n: var Node, line: int, col: int): Node =
  n.line = line
  n.col = col
  return n
proc withFileName*(n: var Node, filename: string): Node =
  n.filename = filename
  return n
proc withMetadata*(n: var Node, line: int, col: int, filename: string): Node =
  n.line = line
  n.col = col
  n.filename = filename
  return n

proc isWordNodeOf*(n: Node, w: string): bool =
  n.nType == N_WORD and n.wVal == w
      
type
  # the reason why seq is not suitable here is that in the most direct style
  # of metacircular evaluator the structure of env is implicitly a tree, e.g.
  # two closure could've had two different env that are built from the same env.
  Env* = ref object
    page*: TableRef[string, Value]
    parent*: Env
  ValueType* = enum
    V_INTEGER
    V_FLOAT
    V_BOOL
    V_STRING
    V_CHAR
    V_CLOSURE
    V_PRIMITIVE
    V_SPECIAL_FORM
    V_SYMBOL
    V_PAIR
    V_VECTOR
    V_CHAR_INPUT
    V_CHAR_OUTPUT
    V_EOF
  ImportDescriptor* = ref object
    importPath*: string
    importNameMapping*: TableRef[string,string]
  Value* = ref object
    case vType*: ValueType
    of V_INTEGER:
      iVal*: int
    of V_FLOAT:
      fVal*: float
    of V_BOOL:
      bVal*: bool
    of V_STRING:
      strVal*: string
    of V_CHAR:
      chVal*: char
    of V_CLOSURE:
      cenv*: Env
      # NOTE: we allow "capture-all" arg list like this:
      #     ((fn x (append '(a b c) x)) 3)    ==> '(a b c 3)
      #     ((fn x (append '(a b c) x)) 3 4 5)    ==> '(a b c 3 4 5)
      # this is represented as carglist = @[] and cvararg = "x".
      carglist*: seq[string]
      cvararg*: string
      cbody*: seq[Node]
    of V_PRIMITIVE:
      pbody*: proc (args: seq[Value], e: Env, call: Node): Value
    of V_SPECIAL_FORM:
      sfbody*: proc (args: seq[Node], tail: Node, e: Env, call: Node): Value
    of V_SYMBOL:
      sVal*: string
    of V_PAIR:
      car*: Value
      cdr*: Value
    of V_VECTOR:
      vVal*: seq[Value]
    of V_CHAR_INPUT:
      charInClosed*: bool
      charInFile*: File
    of V_CHAR_OUTPUT:
      charOutClosed*: bool
      charOutFile*: File
    of V_EOF:
      discard

proc `$`*(vt: ValueType): string =
  case vt:
    of V_INTEGER: "INTEGER"
    of V_FLOAT: "FLOAT"
    of V_BOOL: "BOOL"
    of V_STRING: "STRING"
    of V_CHAR: "CHAR"
    of V_CLOSURE: "CLOSURE"
    of V_PRIMITIVE: "PRIMITIVE"
    of V_SPECIAL_FORM: "SPECIAL_FORM"
    of V_PAIR: "PAIR"
    of V_SYMBOL: "SYMBOL"
    of V_VECTOR: "VECTOR"
    of V_CHAR_INPUT: "CHAR_INPUT"
    of V_CHAR_OUTPUT: "CHAR_OUTPUT"
    of V_EOF: "EOF"

proc `$`*(x: Value): string =
  if x == nil: return "nil"
  case x.vType:
    of V_INTEGER:
      $x.iVal
    of V_FLOAT:
      $x.fVal
    of V_BOOL:
      if x.bVal:
        "#t"
      else:
        "#f"
    of V_STRING:
      $x.strVal
    of V_CHAR:
      if x.chVal in charLiteralMap:
        return charLiteralMap[x.chVal]
      elif x.chVal <= '\x7f':
        return "#\\" & $x.chVal
      else:
        return "#ch{" & (x.chVal.ord.intToHex) & "}"
    of V_CLOSURE:
      "<CLOSURE>"
    of V_PRIMITIVE:
      "<PRIMITIVE>"
    of V_SPECIAL_FORM:
      "<SPECIAL_FORM>"
    of V_SYMBOL:
      x.sVal
    of V_PAIR:
      var l: seq[Value] = @[]
      var tv: Value = nil
      var subj = x
      while subj != nil:
        if subj.vType != V_PAIR:
          tv = subj
          break
        l.add(subj.car)
        subj = subj.cdr
      let tailstr = if tv == nil:
                      ""
                    else:
                      " . " & $x.cdr
      "(" & l.mapIt($it).join(" ") & tailstr & ")"
    of V_VECTOR:
      "#{" & x.vVal.mapIt($it).join(" ") & "}"
    of V_CHAR_INPUT:
      "<CHAR_INPUT:" & (if x.charInClosed:
                          "CLOSED"
                        else:
                          "OPEN") & ">"
    of V_CHAR_OUTPUT:
      "<CHAR_OUTPUT:" & (if x.charOutClosed:
                           "CLOSED"
                         else:
                           "OPEN") & ">"
    of V_EOF: "#eof"
          
      
proc mkEnv*(page: TableRef[string, Value], parent: Env): Env = Env(page: page, parent: parent)
proc mkEnv*(page: TableRef[string, Value]): Env = Env(page: page, parent: nil)

proc fromEnv*(x: string, e: Env): Option[Value] =
  var subj = e
  while subj != nil:
    let tp = subj.page
    if tp.hasKey(x): return some(tp[x])
    subj = subj.parent
  return none(Value)

proc registerValue*(e: Env, k: string, v: Value): void =
  assert e != nil
  e.page[k] = v
  
proc mkIntegerValue*(iVal: int): Value = Value(vType: V_INTEGER, iVal: iVal)
proc mkFloatValue*(fVal: float): Value = Value(vType: V_FLOAT, fVal: fVal)
proc mkBoolValue*(bVal: bool): Value = Value(vType: V_BOOL, bVal: bVal)
proc mkStrValue*(strVal: string): Value = Value(vType: V_STRING, strVal: strVal)
proc mkCharValue*(chVal: char): Value = Value(vType: V_CHAR, chVal: chVal)
proc mkClosureValue*(cenv: Env, carglist: seq[string], cvararg: string, cbody: seq[Node]): Value = Value(vType: V_CLOSURE, cenv: cenv, carglist: carglist, cvararg: cvararg, cbody: cbody)
proc mkPrimitiveValue*(pbody: proc (x: seq[Value], e: Env, call: Node): Value): Value = Value(vType: V_PRIMITIVE, pbody: pbody)
proc mkSpecialFormValue*(sfbody: proc (x: seq[Node], tail: Node, e: Env, call: Node): Value): Value = Value(vType: V_SPECIAL_FORM, sfbody: sfbody)
proc mkSymbolValue*(sVal: string): Value = Value(vType: V_SYMBOL, sVal: sVal)
proc mkPairValue*(car: Value, cdr: Value): Value = Value(vType: V_PAIR, car: car, cdr: cdr)
proc mkVectorValue*(vVal: seq[Value]): Value = Value(vType: V_VECTOR, vVal: vVal)
proc mkCharInputValue*(f: File): Value = Value(vType: V_CHAR_INPUT, charInClosed: false, charInFile: f)
proc mkCharOutputValue*(f: File): Value = Value(vType: V_CHAR_OUTPUT, charOutClosed: false, charOutFile: f)
let GlobalEOFValue*: Value = Value(vType: V_EOF)
let GlobalTrueValue*: Value = Value(vType: V_BOOL, bVal: true)
let GlobalFalseValue*: Value = Value(vType: V_BOOL, bVal: false)
let GlobalStdInValue*: Value = mkCharInputValue(stdin)
let GlobalStdOutValue*: Value = mkCharOutputValue(stdout)
let GlobalStdErrValue*: Value = mkCharOutputValue(stderr)

proc valueEqual*(a: Value, b: Value): bool =
  if a == nil and b == nil: return true
  elif a == nil or b == nil: return false
  if a.vType != b.vType: return false
  case a.vType:
    of V_INTEGER: return a.iVal == b.iVal
    of V_FLOAT: return a.fVal == b.fVal
    of V_BOOL: return a.bVal == b.bVal
    of V_STRING: return a.strVal == b.strVal
    of V_CHAR: return a.chVal == b.chVal
    of V_SYMBOL: return a.sVal == b.sVal
    of V_PAIR: return a.car.valueEqual(b.car) and a.cdr.valueEqual(b.cdr)
    of V_VECTOR:
      if not (a.vVal.len == b.vVal.len): return false
      var i = 0
      while i < a.vVal.len:
        if not a.vVal[i].valueEqual(b.vVal[i]): return false
      return true
    else:
      return false

# NOTE THAT we only treat the boolean false value as false.
# nil is treated as empty list and empty list only.
proc valueToBool*(x: Value): bool =
  if x == nil: return true
  assert x.vType == V_BOOL
  return x.bVal
  
proc isValueAList*(x: Value): bool =
  if x == nil: return true
  if x.vType != V_PAIR: return false
  var subj = x
  while true:
    if subj != nil and subj.vType != V_PAIR: return false
    if subj == nil: return true
    subj = subj.cdr

proc isValueNotFalse*(x: Value): bool =
  not (x.vType == V_BOOL and x.bVal == false)
    
proc valueListToSeq*(x: Value): seq[Value] =
  assert x.isValueAList
  var r: seq[Value] = @[]
  var subj = x
  while subj != nil:
    r.add(subj.car)
    subj = subj.cdr
  return r

proc seqToValueList*(x: seq[Value]): Value =
  var r: Value = nil
  var i = x.len-1
  while i >= 0:
    let v = x[i]
    r = mkPairValue(v, r)
    i -= 1
  return r
    
