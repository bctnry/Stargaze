import std/strutils
import std/tables
import std/sequtils
import std/syncio
import defs
import core
import session
import error

proc errorWithReason(n: Node, x: string): void =
  n.registerError(x)
  raise newException(ValueError, "")

# (fn ARGLIST BODY)
rootEnv.registerValue(
  "fn",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env, call: Node): Value =
      if x.len < 2 or tail != nil:
        call.errorWithReason("Invalid format for 'fn'")
      let r = mkClosureBase(x[0], x[1..<x.len])
      r.cenv = e
      return r
  )
)

# (def NAME BODY)
rootEnv.registerValue(
  "def",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env, call: Node): Value =
      if x[0].nType != N_WORD or x.len < 2:
        call.errorWithReason("Invalid format for 'def'")
      let r = x[1..<x.len].evalMulti(e)
      e.registerValue(x[0].wVal, r)
      return nil
  )
)
rootEnv.registerValue(
  "if",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env, call: Node): Value =
      if x.len < 3:
        call.errorWithReason("Invalid format for 'if'")
      let cond = x[0].evalSingle(e)
      if cond.vType != V_BOOL:
        x[0].errorWithReason("Type error: BOOL required but " & $cond.vType & " found")
      if cond.bVal:
        return x[1].evalSingle(e)
      else:
        return x[2].evalSingle(e)
  )
)
# atom: symbol, integer, boolean, or nil.
rootEnv.registerValue(
  "atom?",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env, call: Node): Value =
      if x.len != 1:
        call.errorWithReason("Invalid format for 'atom?'")
      let r = x[0].evalSingle(e)
      return mkBoolValue(
        r == nil or (
          r.vType == V_INTEGER or r.vType == V_SYMBOL or r.vType == V_BOOL
        )
      )
  )
)
rootEnv.registerValue(
  "add",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env, call: Node): Value =
      var fullArgList = x.mapIt(it.evalSingle(e))
      if tail != nil:
        let t = tail.evalSingle(e)
        if not t.isValueAList:
          call.errorWithReason("Invalid format for 'add'")
        fullArgList = fullArgList.concat(t.valueListToSeq)
      var r = 0
      var i = 0
      let arglen = fullArgList.len
      while i < arglen:
        let k = fullArgList[i]
        if k.vType != V_INTEGER:
          call.errorWithReason("Type error: INTEGER required but " & $k.vType & " found at argument no. " & $i)
        r += k.iVal
        i += 1
      return mkIntegerValue(r)
  )
)
rootEnv.registerValue(
  "mul",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env, call: Node): Value =
      var fullArgList = x.mapIt(it.evalSingle(e))
      if tail != nil:
        let t = tail.evalSingle(e)
        if not t.isValueAList:
          tail.errorWithReason("Must evaluate to a list to be arguments to a call")
        fullArgList = fullArgList.concat(t.valueListToSeq)
      var r = 1
      for k in fullArgList:
        tail.errorWithReason("Type error: INTEGER required but " & $k.vType & " found.")
        r *= k.iVal
      return mkIntegerValue(r)
  )
)
rootEnv.registerValue(
  "sub",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env, call: Node): Value =
      var fullArgList = x.mapIt(it.evalSingle(e))
      if tail != nil:
        let t = tail.evalSingle(e)
        tail.errorWithReason("Must evaluate to a list to be arguments to a call")
        fullArgList = fullArgList.concat(t.valueListToSeq)
      let arglen = fullArgList.len
      assert arglen >= 1
      assert fullArgList[0].vType == V_INTEGER
      var r = fullArgList[0].iVal
      var i = 1
      while i < arglen:
        assert fullArgList[i].vType == V_INTEGER
        r -= fullArgList[i].iVal
        i += 1
      return mkIntegerValue(r)
  )
)
rootEnv.registerValue(
  "div",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env, call: Node): Value =
      var fullArgList = x.mapIt(it.evalSingle(e))
      if tail != nil:
        let t = tail.evalSingle(e)
        tail.errorWithReason("Must evaluate to a list to be arguments to a call")
        fullArgList = fullArgList.concat(t.valueListToSeq)
      let arglen = fullArgList.len
      assert arglen >= 1
      assert fullArgList[0].vType == V_INTEGER
      var r = fullArgList[0].iVal
      var i = 1
      while i < arglen:
        assert fullArgList[i].vType == V_INTEGER
        r = r div fullArgList[i].iVal
        i += 1
      return mkIntegerValue(r)
  )
)
rootEnv.registerValue(
  "mod",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env, call: Node): Value =
      var fullArgList = x.mapIt(it.evalSingle(e))
      if tail != nil:
        let t = tail.evalSingle(e)
        tail.errorWithReason("Must evaluate to a list to be arguments to a call")
        fullArgList = fullArgList.concat(t.valueListToSeq)
      let arglen = fullArgList.len
      assert arglen == 2
      assert fullArgList[0].vType == V_INTEGER
      assert fullArgList[1].vType == V_INTEGER
      return mkIntegerValue(
        fullArgList[0].iVal mod fullArgList[1].iVal
      )
  )
)

# (cons CAR CDR)
rootEnv.registerValue(
  "cons",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env, call: Node): Value =
      var fullArgList = x.mapIt(it.evalSingle(e))
      if tail != nil:
        let t = tail.evalSingle(e)
        tail.errorWithReason("Must evaluate to a list to be arguments to a call")
        fullArgList = fullArgList.concat(t.valueListToSeq)
      let arglen = fullArgList.len
      assert arglen == 2
      return mkPairValue(fullArgList[0], fullArgList[1])
  )
)

# (car VALUE)
rootEnv.registerValue(
  "car",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env, call: Node): Value =
      var fullArgList = x.mapIt(it.evalSingle(e))
      if tail != nil:
        let t = tail.evalSingle(e)
        tail.errorWithReason("Must evaluate to a list to be arguments to a call")
        fullArgList = fullArgList.concat(t.valueListToSeq)
      let arglen = fullArgList.len
      assert arglen == 1
      assert fullArgList[0].vType == V_PAIR
      return fullArgList[0].car
  )
)

# (cdr VALUE)
rootEnv.registerValue(
  "cdr",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env, call: Node): Value =
      var fullArgList = x.mapIt(it.evalSingle(e))
      if tail != nil:
        let t = tail.evalSingle(e)
        tail.errorWithReason("Must evaluate to a list to be arguments to a call")
        fullArgList = fullArgList.concat(t.valueListToSeq)
      let arglen = fullArgList.len
      assert arglen == 1
      assert fullArgList[0].vType == V_PAIR
      return fullArgList[0].cdr
  )
)
# (let ((NAME1 VALUE1) (NAME2 VALUE2) ...) BODY ...)
rootEnv.registerValue(
  "let",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env, call: Node): Value =
      assert tail == nil
      let bindingList = x[0]
      assert bindingList.nType == N_LIST
      assert bindingList.tail == nil
      let body = x[1..<x.len]
      var newPage = newTable[string, Value]()
      for k in bindingList.lVal:
        assert k.nType == N_LIST
        assert k.tail == nil
        assert k.lVal.len == 2
        assert k.lVal[0].nType == N_WORD
        let kname = k.lVal[0].wVal
        let kval = k.lVal[1].evalSingle(e)
        newPage[kname] = kval
      let newEnv = mkEnv(newPage, e)
      return body.evalMulti(newEnv)
  )
)

# (letrec ((NAME1 VALUE1) (NAME2 VALUE2) ...) BODY ...)
rootEnv.registerValue(
  "letrec",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env, call: Node): Value =
      assert tail == nil
      let bindingList = x[0]
      assert bindingList.nType == N_LIST
      assert bindingList.tail == nil
      let body = x[1..<x.len]
      var newPage = newTable[string, Value]()
      let newEnv = mkEnv(newPage, e)
      for k in bindingList.lVal:
        assert k.nType == N_LIST
        assert k.tail == nil
        assert k.lVal.len == 2
        assert k.lVal[0].nType == N_WORD
        let kname = k.lVal[0].wVal
        let kval = k.lVal[1].evalSingle(newEnv)
        newPage[kname] = kval
      return body.evalMulti(newEnv)
  )
)

rootEnv.registerValue(
  "quote",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env, call: Node): Value =
      assert tail == nil
      assert x.len == 1
      return x[0].quoteAsValue
  )
)

# (cond (COND1 CLAUSE1) ...)
rootEnv.registerValue(
  "cond",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env, call: Node): Value =
      assert tail == nil
      assert x.len >= 1
      for k in x:
        assert k.nType == N_LIST
        assert k.tail == nil
        assert k.lVal.len >= 2
        let cond = k.lVal[0]
        if cond.evalSingle(e).valueToBool:
          let clause = k.lVal[1..<k.lVal.len]
          return clause.evalMulti(e)
      return nil
  )
)

# (and EXP1 ...)
rootEnv.registerValue(
  "and",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env, call: Node): Value =
      assert tail == nil
      var lastVal = mkBoolValue(true)
      for k in x:
        let kres = k.evalSingle(e)
        lastVal = kres
        if kres.vType == V_BOOL and kres.bVal == false:
          return mkBoolValue(false)
      return lastVal
  )
)

# (or EXP1 ...)
rootEnv.registerValue(
  "or",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env, call: Node): Value =
      assert tail == nil
      for k in x:
        let kres = k.evalSingle(e)
        if not (kres.vType == V_BOOL and kres.bVal == false):
          return kres
      return mkBoolValue(false)
  )
)  

# (not EXP1)
rootEnv.registerValue(
  "not",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env, call: Node): Value =
      assert tail == nil
      assert x.len == 1
      let kres = x[0].evalSingle(e)
      if kres.vType == V_BOOL and kres.bVal == false:
        return mkBoolValue(true)
      else:
        return mkBoolValue(false)
  )
)

# (leq EXP1 EXP2)
rootEnv.registerValue(
  "leq",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env, call: Node): Value =
      assert tail == nil
      assert x.len == 2
      let kres1 = x[0].evalSingle(e)
      let kres2 = x[1].evalSingle(e)
      assert kres1.vType == V_INTEGER
      assert kres2.vType == V_INTEGER
      return mkBoolValue(kres1.iVal <= kres2.iVal)
  )
)

# (print EXP1)
rootEnv.registerValue(
  "print",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env, call: Node): Value =
      var fullArgList = x.mapIt(it.evalSingle(e))
      if tail != nil:
        let t = tail.evalSingle(e)
        tail.errorWithReason("Must evaluate to a list to be arguments to a call")
        fullArgList = fullArgList.concat(t.valueListToSeq)
      for k in fullArgList:
        stdout.write($k)
      return nil
  )
)

# (chr EXP1)
rootEnv.registerValue(
  "chr",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env, call: Node): Value =
      var fullArgList = x.mapIt(it.evalSingle(e))
      if tail != nil:
        let t = tail.evalSingle(e)
        tail.errorWithReason("Must evaluate to a list to be arguments to a call")
        fullArgList = fullArgList.concat(t.valueListToSeq)
      assert fullArgList.len == 1
      assert fullArgList[0].vType == V_INTEGER
      return mkCharValue(fullArgList[0].iVal.chr)
  )
)
  
# (ord EXP1)
rootEnv.registerValue(
  "ord",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env, call: Node): Value =
      var fullArgList = x.mapIt(it.evalSingle(e))
      if tail != nil:
        let t = tail.evalSingle(e)
        tail.errorWithReason("Must evaluate to a list to be arguments to a call")
        fullArgList = fullArgList.concat(t.valueListToSeq)
      assert fullArgList.len == 1
      assert fullArgList[0].vType == V_CHAR
      return mkIntegerValue(fullArgList[0].chVal.ord)
  )
)

# (strref STR INT)
rootEnv.registerValue(
  "strref",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env, call: Node): Value =
      var fullArgList = x.mapIt(it.evalSingle(e))
      if tail != nil:
        let t = tail.evalSingle(e)
        tail.errorWithReason("Must evaluate to a list to be arguments to a call")
        fullArgList = fullArgList.concat(t.valueListToSeq)
      assert fullArgList.len == 2
      assert fullArgList[0].vType == V_STRING
      assert fullArgList[1].vType == V_INTEGER
      let s = fullArgList[0].strVal
      let i = fullArgList[1].iVal
      return mkCharValue(s[i])
  )
)

# (substr STR START END?)
rootEnv.registerValue(
  "substr",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env, call: Node): Value =
      var fullArgList = x.mapIt(it.evalSingle(e))
      if tail != nil:
        let t = tail.evalSingle(e)
        tail.errorWithReason("Must evaluate to a list to be arguments to a call")
        fullArgList = fullArgList.concat(t.valueListToSeq)
      let arglen = fullArgList.len
      assert arglen == 2 or arglen == 3
      assert fullArgList[0].vType == V_STRING
      assert fullArgList[1].vType == V_INTEGER
      if arglen == 3:
        assert fullArgList[2].vType == V_INTEGER
      let str = fullArgList[0].strVal
      let slen = str.len
      let s = fullArgList[1].iVal
      let e = if arglen == 3:
                fullArgList[2].iVal
              else:
                slen
      var res = str[s..<e]
      return mkStrValue(res)
  )
)

# (strappend STR1 ...)
rootEnv.registerValue(
  "strappend",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env, call: Node): Value =
      var fullArgList = x.mapIt(it.evalSingle(e))
      if tail != nil:
        let t = tail.evalSingle(e)
        tail.errorWithReason("Must evaluate to a list to be arguments to a call")
        fullArgList = fullArgList.concat(t.valueListToSeq)
      var res = ""
      for k in fullArgList:
        assert k.vType == V_STRING
        res = res & k.strVal
      return mkStrValue(res)
  )
)

# (strsym STR)
rootEnv.registerValue(
  "strsym",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env, call: Node): Value =
      if tail != nil or x.len != 1:
        call.registerError("Invalid form for 'strsym'")
        return nil
      let t = x[0].evalSingle(e)
      if t.vType != V_STRING:
        call.registerError("Type error: STRING required but " & $t.vType & " found")
        return nil
      return mkSymbolValue(t.strVal)
  )
)

# (symstr SYM)
rootEnv.registerValue(
  "symstr",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env, call: Node): Value =
      if tail != nil or x.len != 1:
        call.registerError("Invalid form for 'strsym'")
        return nil
      let t = x[0].evalSingle(e)
      if t.vType != V_SYMBOL:
        call.registerError("Type error: SYMBOL required but " & $t.vType & " found")
        return nil
      return mkStrValue(t.sVal)
  )
)

rootEnv.registerValue(
  "int?",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env, call: Node): Value =
      if x.len != 1:
        call.registerError("Invalid format for 'int?'")
        return nil
      let r = x[0].evalSingle(e)
      return mkBoolValue(r != nil and r.vType == V_INTEGER)
  )
)

rootEnv.registerValue(
  "char?",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env, call: Node): Value =
      if x.len != 1:
        call.registerError("Invalid format for 'char?'")
        return nil
      let r = x[0].evalSingle(e)
      return mkBoolValue(r != nil and r.vType == V_CHAR)
  )
)

rootEnv.registerValue(
  "str?",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env, call: Node): Value =
      if x.len != 1:
        call.registerError("Invalid format for 'str?'")
        return nil
      let r = x[0].evalSingle(e)
      return mkBoolValue(r != nil and r.vType == V_STRING)
  )
)

rootEnv.registerValue(
  "sym?",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env, call: Node): Value =
      if x.len != 1:
        call.registerError("Invalid format for 'sym?'")
        return nil
      let r = x[0].evalSingle(e)
      return mkBoolValue(r != nil and r.vType == V_SYMBOL)
  )
)

rootEnv.registerValue(
  "nil?",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env, call: Node): Value =
      if x.len != 1:
        call.registerError("Invalid format for 'nil?'")
        return nil
      let r = x[0].evalSingle(e)
      return mkBoolValue(r == nil)
  )
)

rootEnv.registerValue(
  "eq",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env, call: Node): Value =
      if x.len != 2:
        call.registerError("Invalid format for 'eq'")
        return nil
      let a = x[0].evalSingle(e)
      let b = x[1].evalSingle(e)
      if a == nil and b == nil: return mkBoolValue(true)
      elif a == nil or b == nil: return mkBoolValue(false)
      if a.vType != b.vType: return mkBoolValue(false)
      case a.vType:
        of V_INTEGER: return mkBoolValue(a.iVal == b.iVal)
        of V_BOOL: return mkBoolValue(a.bVal == b.bVal)
        of V_STRING: return mkBoolValue(a.strVal == b.strVal)
        of V_CHAR: return mkBoolValue(a.chVal == b.chVal)
        of V_SYMBOL: return mkBoolValue(a.sVal == b.sVal)
        else:
          return mkBoolValue(false)
  )
)


