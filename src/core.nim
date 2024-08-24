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
      if x.wVal == "#t":
        GlobalTrueValue
      elif x.wVal == "#f":
        GlobalFalseValue
      else:
        mkSymbolValue(x.wVal)
    of N_INTEGER:
      mkIntegerValue(x.iVal)
    of N_FLOAT:
      mkFloatValue(x.fVal)
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
    of N_EOF:
      GlobalEOFValue

proc applyClosure*(x: Value, arglist: seq[Value], argtail: Value, e: Env): Value
proc applyPrimitive*(x: Value, arglist: seq[Value], e: Env, call: Node): Value
proc applySpecialForm*(x: Value, arglist: seq[Node], argtail: Node, e: Env, call: Node): Value
proc quoteF (x: seq[Node], tail: Node, e: Env, call: Node): Value
proc qquoteF (x: seq[Node], tail: Node, e: Env, call: Node): Value
proc unquoteF (x: seq[Node], tail: Node, e: Env, call: Node): Value
proc fnF (x: seq[Node], tail: Node, e: Env, call: Node): Value
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
    of N_FLOAT:
      return mkFloatValue(x.fVal)
    of N_CHAR:
      return mkCharValue(x.chVal)
    of N_STRING:
      return mkStrValue(x.strVal)
    of N_LIST:
      if x.lVal.len <= 0:
        x.errorWithReason("Invalid syntax for call")
      var el: seq[Node] = x.lVal[1..<x.lVal.len]
      var etail: Node = x.tail
      if x.lVal[0].isWordNodeOf("fn"): return fnF(el, etail, e, x)
      elif x.lVal[0].isWordNodeOf("quote"): return quoteF(el, etail, e, x)
      elif x.lVal[0].isWordNodeOf("qquote"): return qquoteF(el, etail, e, x)
      elif x.lVal[0].isWordNodeOf("unquote"): return unquoteF(el, etail, e, x)
      var head = x.lVal[0].evalSingle(e)
      if head == nil:
        x.errorWithReason("Cannot call value '" & $x.lVal[0] & "'")
      case head.vType:
        of V_CLOSURE:
          return applyClosure(head, el.mapIt(it.evalSingle(e)), etail.evalSingle(e), e)
        of V_PRIMITIVE:
          let a = el.mapIt(it.evalSingle(e))
          let b = etail.evalSingle(e)
          if not b.isValueAList():
            x.errorWithReason("Cannot use a non-LIST value as the tail argument")
          return applyPrimitive(head, a.concat(b.valueListToSeq()), e, x)
        of V_SPECIAL_FORM:
          return applySpecialForm(head, el, etail, e, x)
        else:
          x.errorWithReason("Cannot apply '" & $head & "' as a function")
    of N_VECTOR:
      return mkVectorValue(x.vVal.mapIt(it.evalSingle(e)))
    of N_EOF:
      return GlobalEOFValue
          
proc evalMulti*(x: seq[Node], e: Env): Value =
  var last: Value = nil
  for k in x:
    last = k.evalSingle(e)
  return last

proc invalidFormErrorWithReason*(n: Node, name: string, requirement: string = ""): void =
  let tailstr = if requirement == "":
                  "'."
                else:
                  "'; " & requirement & " required."
  n.errorWithReason("Invalid form for '" & name & tailstr)
# NOTE: these names are the names that should not be overridden in anyway.
# these names includes: fn, quote, unquote, qquote
# (fn ARGLIST BODY)
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
proc fnF (x: seq[Node], tail: Node, e: Env, call: Node): Value =
  if tail != nil: tail.invalidFormErrorWithReason("fn")
  if x.len < 2: call.invalidFormErrorWithReason("fn")
  let r = mkClosureBase(x[0], x[1..<x.len])
  r.cenv = e
  return r
proc quoteF (x: seq[Node], tail: Node, e: Env, call: Node): Value =
  if tail != nil: tail.invalidFormErrorWithReason("quote")
  if x.len != 1: call.invalidFormErrorWithReason("quote")
  return x[0].quoteAsValue
# NOTE: unquote should only be expanded under a qquote context, which
#       is handled separately.
proc unquoteF (x: seq[Node], tail: Node, e: Env, call: Node): Value =
  call.errorWithReason("Invalid unquote under current context")
proc evalQuasiQuoteContext(n: Node, e: Env): Value =
  if n == nil: return nil
  case n.nType:
    of N_LIST:
      if n.lVal.len >= 1:
        let head = n.lVal[0]
        if head.isWordNodeOf("unquote"):
          if n.lVal.len != 2 or n.tail != nil: head.invalidFormErrorWithReason("unquote")
          let arg = n.lVal[1]
          return arg.evalSingle(e)
        elif head.isWordNodeOf("quote") or head.isWordNodeOf("qquote"):
          return n.quoteAsValue()
        else:
          let listval = n.lVal.mapIt(it.evalQuasiQuoteContext(e))
          let tailval = n.tail.evalQuasiQuoteContext(e)
          var r = tailval
          var i = listval.len - 1
          while i >= 0:
            r = mkPairValue(listval[i], r)
            i -= 1
          return r
      else:
        if n.tail != nil:
          # this is where cases like "( . 3)" reach.
          n.invalidFormErrorWithReason("qquote")
        else:
          # this is where the empty list reach.
          return nil
    of N_VECTOR:
      return mkVectorValue(n.vVal.mapIt(it.evalQuasiQuoteContext(e)))
    else:
      return n.quoteAsValue()
proc qquoteF (x: seq[Node], tail: Node, e: Env, call: Node): Value =
  if tail != nil: call.invalidFormErrorWithReason("qquote")
  if x.len != 1: call.invalidFormErrorWithReason("qquote", "1 argument")
  return x[0].evalQuasiQuoteContext(e)

  
proc isWordNodeOf(n: Node, x: string): bool =
  n.nType == N_WORD and n.wVal == x

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

proc applyPrimitive*(x: Value, arglist: seq[Value], e: Env, call: Node): Value =
  assert x.vType == V_PRIMITIVE
  return x.pbody(arglist, e, call)

proc applySpecialForm*(x: Value, arglist: seq[Node], argtail: Node, e: Env, call: Node): Value =
  assert x.vType == V_SPECIAL_FORM
  return x.sfbody(arglist, argtail, e, call)
  
