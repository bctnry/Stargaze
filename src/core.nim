import std/sequtils
import std/tables
import std/options
import aux
import defs
import error

proc errorWithReason(n: Node, x: string): void =
  n.registerError(x)
  raise newException(ValueError, "")

proc quoteAsValue*(x: Node): Value =
  if x == nil: return nil
  case x.nType:
    of N_WORD:
      mkSymbolValue(x.wVal)
    of N_INTEGER:
      mkIntegerValue(x.iVal)
    of N_CHAR:
      mkCharValue(x.chVal)
    of N_STRING:
      mkStrValue(x.strVal)
    of N_LIST:
      var r = x.tail.quoteAsValue
      var i = x.lVal.len-1
      while i >= 0:
        r = mkPairValue(x.lVal[i].quoteAsValue, r)
        i -= 1
      return r
    of N_VECTOR:
      mkVectorValue(x.vVal.mapIt(it.quoteAsValue))

proc applyClosure*(x: Value, arglist: seq[Value], argtail: Value, e: Env): Value
proc applyPrimitive*(x: Value, arglist: seq[Node], argtail: Node, e: Env, call: Node): Value
proc evalSingle*(x: Node, e: Env): Value =
  if x == nil: return nil
  case x.nType:
    of N_WORD:
      if x.wVal == "#t":
        return mkBoolValue(true)
      elif x.wVal == "#f":
        return mkBoolValue(false)
      else:
        let c = x.wVal.fromEnv(e)
        if c.isNone():
          x.errorWithReason("Cannot find name '" & x.wVal & "' in current environment.")
        else:
          return c.get()
    of N_INTEGER:
      return mkIntegerValue(x.iVal)
    of N_CHAR:
      return mkCharValue(x.chVal)
    of N_STRING:
      return mkStrValue(x.strVal)
    of N_LIST:
      if x.lVal.len <= 0:
        x.errorWithReason("Invalid syntax for call")
      var head = x.lVal[0].evalSingle(e)
      if head == nil:
        x.errorWithReason("Cannot call value '" & $x.lVal[0] & "'")
      var el: seq[Node] = x.lVal[1..<x.lVal.len]
      var etail: Node = x.tail
      case head.vType:
        of V_CLOSURE:
          return applyClosure(head, el.mapIt(it.evalSingle(e)), etail.evalSingle(e), e)
        of V_PRIMITIVE:
          return applyPrimitive(head, el, etail, e, x)
        else:
          x.errorWithReason("Cannot apply '" & $head & "' as a function")
    of N_VECTOR:
      return mkVectorValue(x.vVal.mapIt(it.evalSingle(e)))
proc evalMulti*(x: seq[Node], e: Env): Value =
  var last: Value = nil
  for k in x:
    last = k.evalSingle(e)
  return last

proc isWordNodeOf(n: Node, x: string): bool =
  n.nType == N_WORD and n.wVal == x

proc mkClosureBase*(argNode: Node, bodyNodeList: seq[Node]): Value =
  ## returns a closure "base" with no env reference.
  var carglist: seq[string] = @[]
  var cvararg: string = ""
  if argNode.nType == N_WORD:
    carglist = @[]
    cvararg = argNode.wVal
  elif argNode.nType == N_LIST:
    # NOTE: doesn't support default arg val yet. assumes it's all N_WORD
    carglist = argNode.lVal.mapIt(it.wVal)
    cvararg = if argNode.tail != nil:
                argNode.tail.wVal
              else:
                ""
  else: argNode.errorWithReason("Invalid grammar for closure")
  return mkClosureValue(nil, carglist, cvararg, bodyNodeList)

proc applyClosure*(x: Value, arglist: seq[Value], argtail: Value, e: Env): Value =
  assert x.vType == V_CLOSURE
  if not argtail.isValueAList:
    errorWithReason("Value " & $argtail & " cannot be used as trailing arg because it is not a list")
  let fullArgList = arglist.concat(argtail.valueListToSeq)
  let newEnvPage = newTable[string, Value]()
  var i = 0
  let argnames = x.carglist
  let arity = argnames.len
  while i < arity:
    let argName = argnames[i]
    let arg = fullArgList[i]
    newEnvPage[argName] = arg
    i += 1
  let restargs = fullArgList[i..<fullArgList.len]
  newEnvPage[x.cvararg] = restargs.seqToValueList
  return x.cbody.evalMulti(mkEnv(newEnvPage, x.cenv))

proc applyPrimitive*(x: Value, arglist: seq[Node], argtail: Node, e: Env, call: Node): Value =
  assert x.vType == V_PRIMITIVE
  return x.pbody(arglist, argtail, e, call)
  
