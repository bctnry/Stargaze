import std/strutils
import std/tables
import std/sequtils

type
  NodeType* = enum
    N_WORD
    N_INTEGER
    N_STRING
    N_CHAR
    N_LIST
  Node* = ref object
    line*: int
    col*: int
    filename*: string
    case nType*: NodeType
    of N_WORD:
      wVal*: string
    of N_INTEGER:
      iVal*: int
    of N_STRING:
      strVal*: string
    of N_CHAR:
      chVal*: char
    of N_LIST:
      lVal*: seq[Node]
      tail*: Node

proc `$`*(x: Node): string =
  if x == nil: return "nil"
  case x.nType:
    of N_WORD:
      x.wVal
    of N_INTEGER:
      $x.iVal
    of N_LIST:
      let tail = if x.tail != nil:
                   " . " & $x.tail
                 else:
                   ""
      "(" & x.lVal.mapIt($it).join(" ") & tail & ")"

proc mkWordNode*(wVal: string): Node = Node(line: -1, col: -1, filename: "", nType: N_WORD, wVal: wVal)
proc mkIntegerNode*(iVal: int): Node = Node(line: -1, col: -1, filename: "", nType: N_INTEGER, iVal: iVal)
proc mkListNode*(lVal: seq[Node], tail: Node): Node = Node(line: -1, col: -1, filename: "", nType: N_LIST, lVal: lVal, tail: tail)
proc withPos*(n: var Node, line: int, col: int): Node =
  n.line = line
  n.col = col
  return n
proc withFileName*(n: var Node, filename: string): Node =
  n.filename = filename
  return n
      
type
  # the reason why seq is not suitable here is that in the most direct style
  # of metacircular evaluator the structure of env is implicitly a tree, e.g.
  # two closure could've had two different env that are built from the same env.
  Env* = ref object
    page*: TableRef[string, Value]
    parent*: Env
  ValueType* = enum
    V_INTEGER
    V_BOOL
    V_STRING
    V_CHAR
    V_CLOSURE
    V_PRIMITIVE
    V_SYMBOL
    V_PAIR
  Value* = ref object
    case vType*: ValueType
    of V_INTEGER:
      iVal*: int
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
      pbody*: proc (x: seq[Node], tail: Node, e: Env): Value
    of V_SYMBOL:
      sVal*: string
    of V_PAIR:
      car*: Value
      cdr*: Value

proc `$`*(x: Value): string =
  if x == nil: return "nil"
  case x.vType:
    of V_INTEGER:
      $x.iVal
    of V_BOOL:
      if x.bVal:
        "#t"
      else:
        "#f"
    of V_STRING:
      $x.strVal
    of V_CHAR:
      $x.chVal
    of V_CLOSURE:
      "<CLOSURE>"
    of V_PRIMITIVE:
      "<PRIMITIVE>"
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
      
proc mkEnv*(page: TableRef[string, Value], parent: Env): Env = Env(page: page, parent: parent)
proc mkEnv*(page: TableRef[string, Value]): Env = Env(page: page, parent: nil)

proc fromEnv*(x: string, e: Env): Value =
  var subj = e
  while subj != nil:
    let tp = subj.page
    if tp.hasKey(x): return tp[x]
    subj = subj.parent
  return nil

proc registerValue*(e: Env, k: string, v: Value): void =
  assert e != nil
  e.page[k] = v
  
proc mkIntegerValue*(iVal: int): Value = Value(vType: V_INTEGER, iVal: iVal)
proc mkBoolValue*(bVal: bool): Value = Value(vType: V_BOOL, bVal: bVal)
proc mkStrValue*(strVal: string): Value = Value(vType: V_STRING, strVal: strVal)
proc mkCharValue*(chVal: char): Value = Value(vType: V_CHAR, chVal: chVal)
proc mkClosureValue*(cenv: Env, carglist: seq[string], cvararg: string, cbody: seq[Node]): Value = Value(vType: V_CLOSURE, cenv: cenv, carglist: carglist, cvararg: cvararg, cbody: cbody)
proc mkPrimitiveValue*(pbody: proc (x: seq[Node], tail: Node, e: Env): Value): Value = Value(vType: V_PRIMITIVE, pbody: pbody)
proc mkSymbolValue*(sVal: string): Value = Value(vType: V_SYMBOL, sVal: sVal)
proc mkPairValue*(car: Value, cdr: Value): Value = Value(vType: V_PAIR, car: car, cdr: cdr)

proc errorWithReason*(n: Node, x: string): void =
  raise newException(ValueError, "(" & $n.line & ":" & $n.col & "): " & x)

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
    
