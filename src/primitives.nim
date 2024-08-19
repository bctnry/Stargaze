import std/strutils
import std/tables
import std/sequtils
import defs
import core
import session

# (fn ARGLIST BODY)
rootEnv.registerValue(
  "fn",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env): Value =
      assert x.len >= 2
      let r = mkClosureBase(x[0], x[1..<x.len])
      r.cenv = e
      return r
  )
)

# (def NAME BODY)
rootEnv.registerValue(
  "def",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env): Value =
      assert x[0].nType == N_WORD
      assert x.len >= 2
      let r = x[1..<x.len].evalMulti(e)
      e.registerValue(x[0].wVal, r)
      return nil
  )
)
rootEnv.registerValue(
  "if",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env): Value =
      assert x.len >= 3
      let cond = x[0].evalSingle(e)
      assert cond.vType == V_BOOL
      if cond.bVal:
        return x[1].evalSingle(e)
      else:
        return x[2].evalSingle(e)
  )
)
# atom: symbol, integer, boolean, or nil.
rootEnv.registerValue(
  "atom",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env): Value =
      assert x.len == 1
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
    proc (x: seq[Node], tail: Node, e: Env): Value =
      var fullArgList = x.mapIt(it.evalSingle(e))
      if tail != nil:
        let t = tail.evalSingle(e)
        assert t.isValueAList
        fullArgList = fullArgList.concat(t.valueListToSeq)
      var r = 0
      for k in fullArgList:
        assert k.vType == V_INTEGER
        r += k.iVal
      return mkIntegerValue(r)
  )
)
rootEnv.registerValue(
  "mul",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env): Value =
      var fullArgList = x.mapIt(it.evalSingle(e))
      if tail != nil:
        let t = tail.evalSingle(e)
        assert t.isValueAList
        fullArgList = fullArgList.concat(t.valueListToSeq)
      var r = 1
      for k in fullArgList:
        assert k.vType == V_INTEGER
        r *= k.iVal
      return mkIntegerValue(r)
  )
)
rootEnv.registerValue(
  "sub",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env): Value =
      var fullArgList = x.mapIt(it.evalSingle(e))
      if tail != nil:
        let t = tail.evalSingle(e)
        assert t.isValueAList
        fullArgList = fullArgList.concat(t.valueListToSeq)
      let arglen = fullArgList.len
      assert arglen >= 1
      assert fullArgList[0].vType == V_INTEGER
      var r = fullArgList[0].iVal
      var i = 1
      while i < arglen:
        assert fullArgList[i].vType == V_INTEGER
        r -= fullArgList[i].iVal
      return mkIntegerValue(r)
  )
)
rootEnv.registerValue(
  "div",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env): Value =
      var fullArgList = x.mapIt(it.evalSingle(e))
      if tail != nil:
        let t = tail.evalSingle(e)
        assert t.isValueAList
        fullArgList = fullArgList.concat(t.valueListToSeq)
      let arglen = fullArgList.len
      assert arglen >= 1
      assert fullArgList[0].vType == V_INTEGER
      var r = fullArgList[0].iVal
      var i = 1
      while i < arglen:
        assert fullArgList[i].vType == V_INTEGER
        r = r div fullArgList[i].iVal
      return mkIntegerValue(r)
  )
)
rootEnv.registerValue(
  "mod",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env): Value =
      var fullArgList = x.mapIt(it.evalSingle(e))
      if tail != nil:
        let t = tail.evalSingle(e)
        assert t.isValueAList
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
    proc (x: seq[Node], tail: Node, e: Env): Value =
      var fullArgList = x.mapIt(it.evalSingle(e))
      if tail != nil:
        let t = tail.evalSingle(e)
        assert t.isValueAList
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
    proc (x: seq[Node], tail: Node, e: Env): Value =
      var fullArgList = x.mapIt(it.evalSingle(e))
      if tail != nil:
        let t = tail.evalSingle(e)
        assert t.isValueAList
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
    proc (x: seq[Node], tail: Node, e: Env): Value =
      var fullArgList = x.mapIt(it.evalSingle(e))
      if tail != nil:
        let t = tail.evalSingle(e)
        assert t.isValueAList
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
    proc (x: seq[Node], tail: Node, e: Env): Value =
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
    proc (x: seq[Node], tail: Node, e: Env): Value =
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
    proc (x: seq[Node], tail: Node, e: Env): Value =
      assert tail == nil
      assert x.len == 1
      return x[0].quoteAsValue
  )
)

# (cond (COND1 CLAUSE1) ...)
rootEnv.registerValue(
  "cond",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env): Value =
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
  

