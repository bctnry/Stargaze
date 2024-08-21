import std/tables
import std/sequtils
import std/syncio
import std/options
import defs
import core
import session
import error
import path
import source
import parser

proc errorWithReason(n: Node, x: string): void =
  n.registerError(x)
  raise newException(ValueError, "")

proc typeErrorWithReason(n: Node, req: ValueType, i: int, t: ValueType): void =
  n.errorWithReason("type error: " & $req & " required but " & $t & " found at argument no. " & $(i+1))

proc invalidFormErrorWithReason(n: Node, name: string, requirement: string = ""): void =
  let tailstr = if requirement == "":
                  "'."
                else:
                  "'; " & requirement & " required."
  n.errorWithReason("Invalid form for '" & name & tailstr)

# (fn ARGLIST BODY)
rootEnv.registerValue(
  "fn",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env, call: Node): Value =
      if tail != nil: tail.invalidFormErrorWithReason("fn")
      if x.len < 2: call.invalidFormErrorWithReason("fn")
      let r = mkClosureBase(x[0], x[1..<x.len])
      r.cenv = e
      return r
  )
)

# (def NAME BODY ...)
rootEnv.registerValue(
  "def",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env, call: Node): Value =
      if x[0].nType != N_WORD or x.len < 2:
        call.invalidFormErrorWithReason("def")
      let r = x[1..<x.len].evalMulti(e)
      e.registerValue(x[0].wVal, r)
      return nil
  )
)
rootEnv.registerValue(
  "if",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env, call: Node): Value =
      if tail != nil: tail.invalidFormErrorWithReason("if")
      if x.len < 3: call.invalidFormErrorWithReason("if")
      let cond = x[0].evalSingle(e)
      if cond.vType != V_BOOL:
        x[0].typeErrorWithReason(V_BOOL, 0, cond.vType)
      if cond.bVal:
        return x[1].evalSingle(e)
      else:
        return x[2].evalSingle(e)
  )
)

rootEnv.registerValue(
  "atom?",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env, call: Node): Value =
      var fullArgList = x.mapIt(it.evalSingle(e))
      if tail != nil:
        let t = tail.evalSingle(e)
        if not t.isValueAList:
          tail.errorWithReason("Must evaluate to a list to be arguments to a call")
        fullArgList = fullArgList.concat(t.valueListToSeq)
      if fullArgList.len != 1:
        call.invalidFormErrorWithReason("atom?")
      let r = x[0].evalSingle(e)
      return mkBoolValue(
        r == nil or (
          r.vType == V_INTEGER or r.vType == V_SYMBOL or r.vType == V_BOOL or r.vType == V_STRING or r.vType == V_CHAR
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
          tail.errorWithReason("Must evaluate to a list to be arguments to a call")
        fullArgList = fullArgList.concat(t.valueListToSeq)
      var r = 0
      var i = 0
      let arglen = fullArgList.len
      while i < arglen:
        let k = fullArgList[i]
        if k.vType != V_INTEGER:
          call.typeErrorWithReason(V_INTEGER, 0, k.vType)
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
        if k.vType != V_INTEGER:
          call.typeErrorWithReason(V_INTEGER, 0, k.vType)
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
      if arglen < 1: call.invalidFormErrorWithReason("sub", "1 argument")
      if fullArgList[0].vType != V_INTEGER:
        call.typeErrorWithReason(V_INTEGER, 0, fullArgList[0].vType)
      var r = fullArgList[0].iVal
      var i = 1
      while i < arglen:
        if fullArgList[i].vType != V_INTEGER:
          call.typeErrorWithReason(V_INTEGER, 0, fullArgList[i].vType)
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
      if arglen < 1: call.invalidFormErrorWithReason("div", "1 argument")
      if fullArgList[0].vType != V_INTEGER:
        call.typeErrorWithReason(V_INTEGER, 0, fullArgList[0].vType)
      var r = fullArgList[0].iVal
      var i = 1
      while i < arglen:
        if fullArgList[i].vType != V_INTEGER:
          call.typeErrorWithReason(V_INTEGER, i, fullArgList[i].vType)
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
      if arglen != 2: call.invalidFormErrorWithReason("mod", "at least 2 arguments")
      if fullArgList[0].vType != V_INTEGER:
        call.typeErrorWithReason(V_INTEGER, 0, fullArgList[0].vType)
      if fullArgList[1].vType != V_INTEGER:
        call.typeErrorWithReason(V_INTEGER, 0, fullArgList[1].vType)
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
      if arglen != 2: call.invalidFormErrorWithReason("cons")
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
      if arglen != 1: call.invalidFormErrorWithReason("car")
      if fullArgList[0].vType != V_PAIR: call.typeErrorWithReason(V_PAIR, 0, fullArgList[0].vType)
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
      if arglen != 1: call.invalidFormErrorWithReason("cdr")
      if fullArgList[0].vType != V_PAIR: call.typeErrorWithReason(V_PAIR, 0, fullArgList[0].vType)
      return fullArgList[0].cdr
  )
)
# (let ((NAME1 VALUE1) (NAME2 VALUE2) ...) BODY ...)
rootEnv.registerValue(
  "let",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env, call: Node): Value =
      if tail != nil: tail.invalidFormErrorWithReason("let")
      let bindingList = x[0]
      if bindingList.nType != N_LIST: bindingList.invalidFormErrorWithReason("let")
      if bindingList.tail != nil: bindingList.tail.invalidFormErrorWithReason("let")
      let body = x[1..<x.len]
      var newPage = newTable[string, Value]()
      for k in bindingList.lVal:
        if k.nType != N_LIST or k.lVal.len != 2 or k.lVal[0].nType != N_WORD:
          k.invalidFormErrorWithReason("let")
        if k.tail != nil: k.invalidFormErrorWithReason("let")
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
      if tail != nil: tail.invalidFormErrorWithReason("letrec")
      let bindingList = x[0]
      if bindingList.nType != N_LIST: bindingList.invalidFormErrorWithReason("letrec")
      if bindingList.tail != nil: bindingList.tail.invalidFormErrorWithReason("letrec")
      let body = x[1..<x.len]
      var newPage = newTable[string, Value]()
      let newEnv = mkEnv(newPage, e)
      for k in bindingList.lVal:
        if k.nType != N_LIST or k.lVal.len != 2 or k.lVal[0].nType != N_WORD:
          k.invalidFormErrorWithReason("letrec")
        if k.tail != nil: k.invalidFormErrorWithReason("letrec")
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
      if tail != nil: tail.invalidFormErrorWithReason("quote")
      if x.len != 1: call.invalidFormErrorWithReason("quote")
      return x[0].quoteAsValue
  )
)

# (cond (COND1 CLAUSE1) ...)
rootEnv.registerValue(
  "cond",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env, call: Node): Value =
      if tail != nil: tail.invalidFormErrorWithReason("cond")
      if x.len < 1: call.invalidFormErrorWithReason("cond")
      for k in x:
        if k.nType != N_LIST: k.invalidFormErrorWithReason("cond")
        if k.tail != nil: k.invalidFormErrorWithReason("cond")
        if k.lVal.len < 2: k.invalidFormErrorWithReason("cond")
        let cond = k.lVal[0]
        if cond.evalSingle(e).valueToBool:
          let clause = k.lVal[1..<k.lVal.len]
          return clause.evalMulti(e)
      return nil
  )
)

# NOTE: we don't allow tail in and/or/not (e.g. calling and like this: (and X Y . Z_LIST))
#       is that (1) we need these three to be "short-circuiting"; (2) if we need these to
#       be short-circuiting we cannot evaluate the arguments first; (3) but if we don't
#       evaluate them we cannot check the length of arglist (so that we could check if the
#       call has the correct form). it would be way simpler to just not allow this kind
#       of calls. the same goes for if and cond.
# (and EXP1 ...)
rootEnv.registerValue(
  "and",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env, call: Node): Value =
      if tail != nil: tail.invalidFormErrorWithReason("and")
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
      if tail != nil: tail.invalidFormErrorWithReason("or")
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
      if tail != nil: tail.invalidFormErrorWithReason("not")
      if x.len != 2: call.invalidFormErrorWithReason("not", "1 argument")
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
      if tail != nil: tail.invalidFormErrorWithReason("leq")
      if x.len != 2: call.invalidFormErrorWithReason("leq", "2 arguments")
      let kres1 = x[0].evalSingle(e)
      let kres2 = x[1].evalSingle(e)
      if kres1.vType != V_INTEGER: call.typeErrorWithReason(V_INTEGER, 0, kres1.vType)
      if kres2.vType != V_INTEGER: call.typeErrorWithReason(V_INTEGER, 1, kres2.vType)
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
      let arglen = fullArgList.len
      if arglen != 1: call.invalidFormErrorWithReason("chr", "1 argument")
      if fullArgList[0].vType != V_INTEGER:
        call.typeErrorWithReason(V_INTEGER, 0, fullArgList[0].vType)
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
      let arglen = fullArgList.len
      if arglen != 1: call.invalidFormErrorWithReason("ord", "1 argument")
      if fullArgList[0].vType != V_CHAR:
        call.typeErrorWithReason(V_CHAR, 0, fullArgList[0].vType)
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
      let arglen = fullArgList.len
      if arglen != 2: call.invalidFormErrorWithReason("strref", "2 arguments")
      if fullArgList[0].vType != V_STRING:
        call.typeErrorWithReason(V_STRING, 0, fullArgList[0].vType)
      if fullArgList[1].vType != V_INTEGER:
        call.typeErrorWithReason(V_INTEGER, 1, fullArgList[1].vType)
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
      if arglen != 2 and arglen != 3:
        call.invalidFormErrorWithReason("substr", "2 or 3 argument")
      if fullArgList[0].vType != V_STRING:
        call.typeErrorWithReason(V_STRING, 0, fullArgList[0].vType)
      if fullArgList[1].vType != V_INTEGER:
        call.typeErrorWithReason(V_INTEGER, 1, fullArgList[0].vType)
      if arglen == 3:
        if fullArgList[2].vType != V_INTEGER:
          call.typeErrorWithReason(V_INTEGER, 2, fullArgList[0].vType)
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
      let arglen = fullArgList.len
      var i = 0
      while i < arglen:
        let k = fullArgList[i]
        if k.vType != V_STRING:
          call.typeErrorWithReason(V_STRING, i, k.vType)
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
        call.invalidFormErrorWithReason("strsym")
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
      if tail != nil or x.len != 1: call.invalidFormErrorWithReason("symstr")
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
      if x.len != 1: call.invalidFormErrorWithReason("int?")
      let r = x[0].evalSingle(e)
      return mkBoolValue(r != nil and r.vType == V_INTEGER)
  )
)

rootEnv.registerValue(
  "char?",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env, call: Node): Value =
      if x.len != 1: call.invalidFormErrorWithReason("char?")
      let r = x[0].evalSingle(e)
      return mkBoolValue(r != nil and r.vType == V_CHAR)
  )
)

rootEnv.registerValue(
  "str?",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env, call: Node): Value =
      if x.len != 1: call.invalidFormErrorWithReason("str?")
      let r = x[0].evalSingle(e)
      return mkBoolValue(r != nil and r.vType == V_STRING)
  )
)

rootEnv.registerValue(
  "sym?",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env, call: Node): Value =
      if x.len != 1: call.invalidFormErrorWithReason("sym?")
      let r = x[0].evalSingle(e)
      return mkBoolValue(r != nil and r.vType == V_SYMBOL)
  )
)

rootEnv.registerValue(
  "nil?",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env, call: Node): Value =
      if x.len != 1: call.invalidFormErrorWithReason("nil?")
      let r = x[0].evalSingle(e)
      return mkBoolValue(r == nil)
  )
)

rootEnv.registerValue(
  "equal",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env, call: Node): Value =
      if x.len != 2: call.invalidFormErrorWithReason("eq")
      let a = x[0].evalSingle(e)
      let b = x[1].evalSingle(e)
      return mkBoolValue(a.valueEqual(b))
  )
)

rootEnv.registerValue(
  "include",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env, call: Node): Value =
      if x.len != 1: call.invalidFormErrorWithReason("include")
      let a = x[0].evalSingle(e)
      if a.vType != V_STRING: call.typeErrorWithReason(V_STRING, 0, a.vType)
      let fullPath = a.strVal.resolveModuleByName()
      if fullPath.isNone():
        call.errorWithReason("Cannot resolve module with name '" & a.strVal & "'")
      useSourceFile(fullPath.get())
      var fl = getCurrentSourceFile()
      var parseRes = fl.parseMultiNode()
      try:
        var evalRes = parseRes.evalMulti(e)
        return evalRes
      except:
        return nil
  )
)

rootEnv.registerValue(
  "export",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env, call: Node): Value =
      if tail != nil: call.invalidFormErrorWithReason("export")
      for k in x:
        if k.nType != N_WORD:
          k.invalidFormErrorWithReason("export")
        exportName(k.wVal, call)
      return nil
  )
)

rootEnv.registerValue(
  "import",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env, call: Node): Value =
      if tail != nil: call.invalidFormErrorWithReason("import")
      if x.len < 1 or x.len > 2: call.invalidFormErrorWithReason("import")

      # handle module desc...
      var importedModuleName: string = ""
      var prefix: string = ""
      if x[0].nType == N_STRING:
        importedModuleName = x[0].strVal
      elif x[0].nType == N_LIST:
        if x[0].tail != nil: x[0].invalidFormErrorWithReason("import")
        if x[0].lVal.len != 2: x[0].invalidFormErrorWithReason("import")
        let nameNode = x[0].lVal[0]
        let prefixNode = x[0].lVal[1]
        if nameNode.nType != N_STRING:
          nameNode.errorWithReason("Module name must be a STRING")
        if prefixNode.nType != N_WORD:
          prefixNode.errorWithReason("Prefix must be a WORD")
        importedModuleName = nameNode.strVal
        prefix = prefixNode.wVal
      else:
        call.invalidFormErrorWithReason("import")
      var renamingTable = newTable[string,string]()
      var importedNames: seq[string] = @[]
      if x.len > 1:
        let renameListNode = x[1]
        if renameListNode.tail != nil or renameListNode.nType != N_LIST: renameListNode.invalidFormErrorWithReason("import")
        for k in renameListNode.lVal:
          if k.nType == N_WORD: # with no renaming
            importedNames.add(k.wVal)
            renamingTable[k.wVal] = k.wVal
          elif k.nType == N_LIST: # with renaming
            if k.tail != nil or k.lVal.len != 2 or k.lVal[0].nType != N_WORD or k.lVal[1].nType != N_WORD:
              k.invalidFormErrorWithReason("import")
            importedNames.add(k.lVal[0].wVal)
            renamingTable[k.lVal[0].wVal] = k.lVal[1].wVal
          else:
            k.invalidFormErrorWithReason("import")

      # import the module.
      # to import the module:
      # 1.  save prev env & init new env
      # 2.  read, parse and evaluate imported module with new env
      # 3.  for all required name, check if name exists in export list
      #     if exists, rename accordingly & insert into self env.
      let thisEnv = getCurrentEnv()
      let thisExportList = getCurrentExportList()
      var envFromImportedModule: Env = nil
      var exportListFromImportedModule: seq[(string, Node)] = @[]
      let m = tryGetImportedModule(importedModuleName)
      if m.isNone():
        let moduleRealPath = resolveModuleByName(importedModuleName)
        if moduleRealPath.isNone():
          call.errorWithReason("Cannot find module '" & importedModuleName & "'")
        prepareForNewModule()
        useSourceFile(moduleRealPath.get())
        var fl = getCurrentSourceFile()
        initNewEnv()
        var parseRes = fl.parseMultiNode()
        discard parseRes.evalMulti(getCurrentEnv())
        envFromImportedModule = getCurrentEnv()
        exportListFromImportedModule = getCurrentExportList()
        registerImportedModule(importedModuleName, envFromImportedModule, exportListFromImportedModule)
      else:
        envFromImportedModule = m.get()[0]
        exportListFromImportedModule = m.get()[1]
      setCurrentEnv(thisEnv)
      restoreCurrentExportList(thisExportList)
      if x.len == 1:
        # import all.
        for k in exportListFromImportedModule:
          let name = k[0]
          let v = name.fromEnv(envFromImportedModule)
          if v.isNone():
            call.errorWithReason("Cannot find name '" & name & "' in module " & importedModuleName)
          e.registerValue(prefix & name, v.get())
      else:
        for k in importedNames:
          let v = k.fromEnv(envFromImportedModule)
          if v.isNone():
            call.errorWithReason("Cannot find name '" & k & "' in module " & importedModuleName)
          e.registerValue(renamingTable[k], v.get())
      return nil
  )
)

rootEnv.registerValue(
  "list",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env, call: Node): Value =
      var fullArgList = x.mapIt(it.evalSingle(e))
      if tail != nil:
        let t = tail.evalSingle(e)
        tail.errorWithReason("Must evaluate to a list to be arguments to a call")
        fullArgList = fullArgList.concat(t.valueListToSeq)
      var res: Value = nil
      let arglen = fullArgList.len
      var i = arglen-1
      while i >= 0:
        res = mkPairValue(fullArgList[i], res)
        i -= 1
      return res
  )
)

rootEnv.registerValue(
  "vector",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env, call: Node): Value =
      var fullArgList = x.mapIt(it.evalSingle(e))
      if tail != nil:
        let t = tail.evalSingle(e)
        tail.errorWithReason("Must evaluate to a list to be arguments to a call")
        fullArgList = fullArgList.concat(t.valueListToSeq)
      return mkVectorValue(fullArgList)
  )
)

rootEnv.registerValue(
  "listvec",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env, call: Node): Value =
      var fullArgList = x.mapIt(it.evalSingle(e))
      if tail != nil:
        let t = tail.evalSingle(e)
        tail.errorWithReason("Must evaluate to a list to be arguments to a call")
        fullArgList = fullArgList.concat(t.valueListToSeq)
      if fullArgList.len != 1: call.invalidFormErrorWithReason("listvec", "1 argument")
      return mkVectorValue(fullArgList[0].valueListToSeq())
  )
)

rootEnv.registerValue(
  "veclist",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env, call: Node): Value =
      var fullArgList = x.mapIt(it.evalSingle(e))
      if tail != nil:
        let t = tail.evalSingle(e)
        tail.errorWithReason("Must evaluate to a list to be arguments to a call")
        fullArgList = fullArgList.concat(t.valueListToSeq)
      if fullArgList.len != 1: call.invalidFormErrorWithReason("veclist", "1 argument")
      return fullArgList[0].vVal.seqToValueList()
  )
)

rootEnv.registerValue(
  "vecref",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env, call: Node): Value =
      var fullArgList = x.mapIt(it.evalSingle(e))
      if tail != nil:
        let t = tail.evalSingle(e)
        tail.errorWithReason("Must evaluate to a list to be arguments to a call")
        fullArgList = fullArgList.concat(t.valueListToSeq)
      if fullArgList.len != 2: call.invalidFormErrorWithReason("vecref", "2 argument")
      if fullArgList[0].vType != V_VECTOR:
        call.typeErrorWithReason(V_VECTOR, 0, fullArgList[0].vType)
      if fullArgList[1].vType != V_INTEGER:
        call.typeErrorWithReason(V_INTEGER, 1, fullArgList[1].vType)
      return fullArgList[0].vVal[fullArgList[1].iVal]
  )
)

rootEnv.registerValue(
  "mkvec",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env, call: Node): Value =
      var fullArgList = x.mapIt(it.evalSingle(e))
      if tail != nil:
        let t = tail.evalSingle(e)
        tail.errorWithReason("Must evaluate to a list to be arguments to a call")
        fullArgList = fullArgList.concat(t.valueListToSeq)
      if fullArgList.len != 1: call.invalidFormErrorWithReason("mkvec", "1 argument")
      if fullArgList[0].vType != V_INTEGER:
        call.typeErrorWithReason(V_INTEGER, 0, fullArgList[0].vType)
      var res: seq[Value] = @[]
      for i in 0..<fullArgList[0].iVal: res.add(nil)
      return mkVectorValue(res)
  )
)

rootEnv.registerValue(
  "vecset!",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env, call: Node): Value =
      var fullArgList = x.mapIt(it.evalSingle(e))
      if tail != nil:
        let t = tail.evalSingle(e)
        tail.errorWithReason("Must evaluate to a list to be arguments to a call")
        fullArgList = fullArgList.concat(t.valueListToSeq)
      if fullArgList.len != 3: call.invalidFormErrorWithReason("vecset!", "3 argument")
      if fullArgList[0].vType != V_VECTOR:
        call.typeErrorWithReason(V_VECTOR, 0, fullArgList[0].vType)
      if fullArgList[1].vType != V_INTEGER:
        call.typeErrorWithReason(V_INTEGER, 1, fullArgList[1].vType)
      fullArgList[0].vVal[fullArgList[1].iVal] = fullArgList[2]
      return nil
  )
)

rootEnv.registerValue(
  "set!",
  mkPrimitiveValue(
    proc (x: seq[Node], tail: Node, e: Env, call: Node): Value =
      if tail != nil or x.len != 2:
        call.invalidFormErrorWithReason("set!", "2 argument")
      if x[0].nType != N_WORD:
        x[0].invalidFormErrorWithReason("set!", "a name")
      let newval = x[1].evalSingle(e)
      e.registerValue(x[0].wVal, newval)
      return nil
  )
)






