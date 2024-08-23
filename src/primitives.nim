import std/tables
import std/sequtils
import std/syncio
import std/options
import std/strutils
import std/math
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

proc typeErrorWithReason(n: Node, req: seq[ValueType], i: int, t: ValueType): void =
  n.errorWithReason("type error: " & req.mapIt($it).join(" or ") & " required but " & $t & " found at argument no. " & $(i+1))

proc invalidFormErrorWithReason(n: Node, name: string, requirement: string = ""): void =
  let tailstr = if requirement == "":
                  "'."
                else:
                  "'; " & requirement & " required."
  n.errorWithReason("Invalid form for '" & name & tailstr)

proc ensureArgOfType(n: Node, v: Value, i: int, t: ValueType): void =
  if v.vType != t: n.typeErrorWithReason(t, i, v.vType)

proc ensureArgOfType(n: Node, v: Value, i: int, t: seq[ValueType]): void =
  if not t.contains(v.vType): n.typeErrorWithReason(t, i, v.vType)

proc verdictValue(x: bool): Value =
  if x: GlobalTrueValue else: GlobalFalseValue

# (fn ARGLIST BODY)
rootEnv.registerValue(
  "fn",
  mkSpecialFormValue(
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
  mkSpecialFormValue(
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
  mkSpecialFormValue(
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
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 1:
        call.invalidFormErrorWithReason("atom?")
      let r = x[0]
      return (r == nil or (
        r.vType == V_INTEGER or r.vType == V_SYMBOL or r.vType == V_BOOL or r.vType == V_STRING or r.vType == V_CHAR
      )).verdictValue
  )
)

rootEnv.registerValue(
  "add",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      var r = 0
      for i in 0..<x.len:
        call.ensureArgOfType(x[i], 0, V_INTEGER)
        r += x[i].iVal
      return mkIntegerValue(r)
  )
)

rootEnv.registerValue(
  "mul",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      var r = 1
      for i in 0..<x.len:
        call.ensureArgOfType(x[i], i, V_INTEGER)
        r *= x[i].iVal
      return mkIntegerValue(r)
  )
)

rootEnv.registerValue(
  "sub",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      let arglen = x.len
      if arglen < 1: call.invalidFormErrorWithReason("sub", "1 argument")
      call.ensureArgOfType(x[0], 0, V_INTEGER)
      var r = x[0].iVal
      for i in 1..<x.len:
        call.ensureArgOfType(x[i], i, V_INTEGER)
        r -= x[i].iVal
      return mkIntegerValue(r)
  )
)

rootEnv.registerValue(
  "div",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      let arglen = x.len
      if arglen < 1: call.invalidFormErrorWithReason("div", "1 argument")
      call.ensureArgOfType(x[0], 0, V_INTEGER)
      var r = x[0].iVal
      for i in 1..<x.len:
        call.ensureArgOfType(x[i], i, V_INTEGER)
        r = r div x[i].iVal
      return mkIntegerValue(r)
  )
)
rootEnv.registerValue(
  "mod",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      let arglen = x.len
      if arglen != 2: call.invalidFormErrorWithReason("mod", "at least 2 arguments")
      call.ensureArgOfType(x[0], 0, V_INTEGER)
      call.ensureArgOfType(x[1], 1, V_INTEGER)
      return mkIntegerValue(x[0].iVal mod x[1].iVal)
  )
)

# (cons CAR CDR)
rootEnv.registerValue(
  "cons",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      let arglen = x.len
      if arglen != 2: call.invalidFormErrorWithReason("cons")
      return mkPairValue(x[0], x[1])
  )
)

# (car VALUE)
rootEnv.registerValue(
  "car",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      let arglen = x.len
      if arglen != 1: call.invalidFormErrorWithReason("car")
      call.ensureArgOfType(x[0], 0, V_PAIR)
      return x[0].car
  )
)

# (cdr VALUE)
rootEnv.registerValue(
  "cdr",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      let arglen = x.len
      if arglen != 1: call.invalidFormErrorWithReason("car")
      call.ensureArgOfType(x[0], 0, V_PAIR)
      return x[0].cdr
  )
)

# (let ((NAME1 VALUE1) (NAME2 VALUE2) ...) BODY ...)
rootEnv.registerValue(
  "let",
  mkSpecialFormValue(
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
  mkSpecialFormValue(
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
  mkSpecialFormValue(
    proc (x: seq[Node], tail: Node, e: Env, call: Node): Value =
      if tail != nil: tail.invalidFormErrorWithReason("quote")
      if x.len != 1: call.invalidFormErrorWithReason("quote")
      return x[0].quoteAsValue
  )
)

# (cond (COND1 CLAUSE1) ...)
rootEnv.registerValue(
  "cond",
  mkSpecialFormValue(
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
  mkSpecialFormValue(
    proc (x: seq[Node], tail: Node, e: Env, call: Node): Value =
      if tail != nil: tail.invalidFormErrorWithReason("and")
      var lastVal = GlobalTrueValue
      for k in x:
        let kres = k.evalSingle(e)
        lastVal = kres
        if kres.vType == V_BOOL and kres.bVal == false:
          return GlobalFalseValue
      return lastVal
  )
)

# (or EXP1 ...)
rootEnv.registerValue(
  "or",
  mkSpecialFormValue(
    proc (x: seq[Node], tail: Node, e: Env, call: Node): Value =
      if tail != nil: tail.invalidFormErrorWithReason("or")
      for k in x:
        let kres = k.evalSingle(e)
        if not (kres.vType == V_BOOL and kres.bVal == false):
          return kres
      return GlobalFalseValue
  )
)  

# (not EXP1)
rootEnv.registerValue(
  "not",
  mkSpecialFormValue(
    proc (x: seq[Node], tail: Node, e: Env, call: Node): Value =
      if tail != nil: tail.invalidFormErrorWithReason("not")
      if x.len != 2: call.invalidFormErrorWithReason("not", "1 argument")
      let kres = x[0].evalSingle(e)
      if kres.vType == V_BOOL and kres.bVal == false:
        return GlobalTrueValue
      else:
        return GlobalFalseValue
  )
)

# (leq EXP1 EXP2)
rootEnv.registerValue(
  "leq",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 2: call.invalidFormErrorWithReason("leq", "2 arguments")
      call.ensureArgOfType(x[0], 0, V_INTEGER)
      call.ensureArgOfType(x[1], 1, V_INTEGER)
      return (x[0].iVal <= x[1].iVal).verdictValue
  )
)

rootEnv.registerValue(
  "lt",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 2: call.invalidFormErrorWithReason("lt", "2 arguments")
      call.ensureArgOfType(x[0], 0, V_INTEGER)
      call.ensureArgOfType(x[1], 1, V_INTEGER)
      return (x[0].iVal < x[1].iVal).verdictValue
  )
)

rootEnv.registerValue(
  "geq",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 2: call.invalidFormErrorWithReason("geq", "2 arguments")
      call.ensureArgOfType(x[0], 0, V_INTEGER)
      call.ensureArgOfType(x[1], 1, V_INTEGER)
      return (x[0].iVal >= x[1].iVal).verdictValue
  )
)

rootEnv.registerValue(
  "gt",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 2: call.invalidFormErrorWithReason("gt", "2 arguments")
      call.ensureArgOfType(x[0], 0, V_INTEGER)
      call.ensureArgOfType(x[1], 1, V_INTEGER)
      return (x[0].iVal > x[1].iVal).verdictValue
  )
)

# (print EXP1)
rootEnv.registerValue(
  "print",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      for k in x:
        stdout.write($k)
      return nil
  )
)

# (chr EXP1)
rootEnv.registerValue(
  "chr",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      let arglen = x.len
      if arglen != 1: call.invalidFormErrorWithReason("chr", "1 argument")
      call.ensureArgOfType(x[0], 0, V_INTEGER)
      return mkCharValue(x[0].iVal.chr)
  )
)
  
# (ord EXP1)
rootEnv.registerValue(
  "ord",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      let arglen = x.len
      if arglen != 1: call.invalidFormErrorWithReason("ord", "1 argument")
      call.ensureArgOfType(x[0], 0, V_CHAR)
      return mkIntegerValue(x[0].chVal.ord)
  )
)

# (strref STR INT)
rootEnv.registerValue(
  "strref",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      let arglen = x.len
      if arglen != 2: call.invalidFormErrorWithReason("strref", "2 arguments")
      call.ensureArgOfType(x[0], 0, V_STRING)
      call.ensureArgOfType(x[1], 1, V_INTEGER)
      let s = x[0].strVal
      let i = x[1].iVal
      return mkCharValue(s[i])
  )
)

# (substr STR START END?)
rootEnv.registerValue(
  "substr",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      let arglen = x.len
      if arglen != 2 and arglen != 3:
        call.invalidFormErrorWithReason("substr", "2 or 3 argument")
      call.ensureArgOfType(x[0], 0, V_STRING)
      call.ensureArgOfType(x[1], 1, V_INTEGER)
      if arglen == 3:
        call.ensureArgOfType(x[2], 2, V_INTEGER)
      let str = x[0].strVal
      let slen = str.len
      let s = x[1].iVal
      let e = if arglen == 3:
                x[2].iVal
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
    proc (x: seq[Value], e: Env, call: Node): Value =
      var res = ""
      let arglen = x.len
      for i in 0..<arglen:
        let k = x[i]
        call.ensureArgOfType(k, i, V_STRING)
        res = res & k.strVal
      return mkStrValue(res)
  )
)

# (strsym STR)
rootEnv.registerValue(
  "strsym",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 1:
        call.invalidFormErrorWithReason("strsym")
      let t = x[0]
      call.ensureArgOfType(t, 0, V_STRING)
      return mkSymbolValue(t.strVal)
  )
)

# (symstr SYM)
rootEnv.registerValue(
  "symstr",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 1: call.invalidFormErrorWithReason("symstr")
      let t = x[0]
      call.ensureArgOfType(t, 0, V_SYMBOL)
      return mkStrValue(t.sVal)
  )
)

rootEnv.registerValue(
  "int?",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 1: call.invalidFormErrorWithReason("int?")
      let r = x[0]
      return (r != nil and r.vType == V_INTEGER).verdictValue
  )
)

rootEnv.registerValue(
  "char?",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 1: call.invalidFormErrorWithReason("char?")
      let r = x[0]
      return (r != nil and r.vType == V_CHAR).verdictValue
  )
)

rootEnv.registerValue(
  "str?",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 1: call.invalidFormErrorWithReason("str?")
      let r = x[0]
      return (r != nil and r.vType == V_STRING).verdictValue
  )
)

rootEnv.registerValue(
  "sym?",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 1: call.invalidFormErrorWithReason("sym?")
      let r = x[0]
      return (r != nil and r.vType == V_SYMBOL).verdictValue
  )
)

rootEnv.registerValue(
  "pair?",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 1: call.invalidFormErrorWithReason("pair?")
      let r = x[0]
      return (r != nil and r.vType == V_PAIR).verdictValue
  )
)

rootEnv.registerValue(
  "equal",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 2: call.invalidFormErrorWithReason("eq")
      let a = x[0]
      let b = x[1]
      return (a.valueEqual(b)).verdictValue
  )
)

rootEnv.registerValue(
  "include",
  mkSpecialFormValue(
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
  mkSpecialFormValue(
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
  mkSpecialFormValue(
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
    proc (x: seq[Value], e: Env, call: Node): Value =
      var res: Value = nil
      let arglen = x.len
      var i = arglen-1
      while i >= 0:
        res = mkPairValue(x[i], res)
        i -= 1
      return res
  )
)

rootEnv.registerValue(
  "vector",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      return mkVectorValue(x)
  )
)

rootEnv.registerValue(
  "vec?",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 1: call.invalidFormErrorWithReason("vec?", "1 argument")
      return (x[0].vType == V_VECTOR).verdictValue
  )
)

rootEnv.registerValue(
  "listvec",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 1: call.invalidFormErrorWithReason("listvec", "1 argument")
      return mkVectorValue(x[0].valueListToSeq())
  )
)

rootEnv.registerValue(
  "veclist",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 1: call.invalidFormErrorWithReason("veclist", "1 argument")
      return x[0].vVal.seqToValueList()
  )
)

rootEnv.registerValue(
  "vecref",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 2: call.invalidFormErrorWithReason("vecref", "2 arguments")
      call.ensureArgOfType(x[0], 0, V_VECTOR)
      call.ensureArgOfType(x[1], 1, V_INTEGER)
      return x[0].vVal[x[1].iVal]
  )
)

rootEnv.registerValue(
  "mkvec",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 1: call.invalidFormErrorWithReason("mkvec", "1 argument")
      call.ensureArgOfType(x[0], 0, V_INTEGER)
      var res: seq[Value] = @[]
      for i in 0..<x[0].iVal: res.add(nil)
      return mkVectorValue(res)
  )
)

rootEnv.registerValue(
  "vecset!",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 3: call.invalidFormErrorWithReason("vecset!", "3 argument")
      call.ensureArgOfType(x[0], 0, V_VECTOR)
      call.ensureArgOfType(x[1], 1, V_INTEGER)
      x[0].vVal[x[1].iVal] = x[2]
      return nil
  )
)

rootEnv.registerValue(
  "set!",
  mkSpecialFormValue(
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

rootEnv.registerValue(
  "eof?",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 1: call.invalidFormErrorWithReason("eof?", "1 argument")
      return (x[0].vType == V_EOF).verdictValue
  )
)

rootEnv.registerValue(
  "readch",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 1: call.invalidFormErrorWithReason("readch", "1 argument")
      call.ensureArgOfType(x[0], 0, V_CHAR_INPUT)
      if x[0].charInClosed: return GlobalEOFValue
      try:
        let ch = x[0].charInFile.readChar()
        return mkCharValue(ch)
      except:
        return GlobalEOFValue
  )
)

rootEnv.registerValue(
  "writech",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 2: call.invalidFormErrorWithReason("writech", "1 argument")
      call.ensureArgOfType(x[0], 0, V_CHAR_OUTPUT)
      call.ensureArgOfType(x[1], 1, V_CHAR)
      if x[0].charOutClosed: return nil
      try:
        x[0].charOutFile.write(x[1].chVal)
        return nil
      except:
        return nil
  )
)

rootEnv.registerValue(
  "close",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 1: call.invalidFormErrorWithReason("close", "1 argument")
      call.ensureArgOfType(x[0], 0, @[V_CHAR_OUTPUT, V_CHAR_INPUT])
      try:
        if x[0].vType == V_CHAR_INPUT:
          x[0].charInFile.close()
          x[0].charInClosed = true
        else:
          x[0].charOutFile.close()
          x[0].charOutClosed = true
        return nil
      except:
        return nil
  )
)

rootEnv.registerValue(
  "openinput",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 1: call.invalidFormErrorWithReason("openinput", "1 argument")
      call.ensureArgOfType(x[0], 0, V_STRING)
      let filemode = fmRead
      try:
        let file = open(x[0].strVal, filemode)
        return mkCharInputValue(file)
      except:
        call.errorWithReason("Failed to open file: " & x[0].strVal)
  )
)

rootEnv.registerValue(
  "openoutput",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 1 and x.len != 2: call.invalidFormErrorWithReason("openoutput", "1 or 2 argument")
      call.ensureArgOfType(x[0], 0, V_STRING)
      var filemode = ""
      if x.len == 2:
        call.ensureArgOfType(x[1], 1, V_STRING)
        filemode = x[1].strVal
      let filemodeval = case filemode:
                          of "x": fmReadWriteExisting
                          of "a": fmAppend
                          else: fmWrite
      try:
        let file = open(x[0].strVal, filemodeval)
        return mkCharOutputValue(file)
      except:
        call.errorWithReason("Failed to open file: " & x[0].strVal)
  )
)

rootEnv.registerValue("stdin", GlobalStdInValue)
rootEnv.registerValue("stdout", GlobalStdOutValue)
rootEnv.registervalue("stderr", GlobalStdErrValue)

rootEnv.registerValue(
  "addf",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      var r: float = 0
      for i in 0..<x.len:
        call.ensureArgOfType(x[i], i, V_FLOAT)
        r += x[i].fVal
      return mkFloatValue(r)
  )
)
rootEnv.registerValue(
  "mulf",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      var r: float = 1
      for i in 0..<x.len:
        call.ensureArgOfType(x[i], i, V_FLOAT)
        r *= x[i].fVal
      return mkFloatValue(r)
  )
)
rootEnv.registerValue(
  "subf",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len < 1: call.invalidFormErrorWithReason("subf", "at least 1 argument")
      call.ensureArgOfType(x[0], 0, V_FLOAT)
      var r = x[0].fVal
      for i in 1..<x.len:
        call.ensureArgOfType(x[i], i, V_FLOAT)
        r -= x[i].fVal
      return mkFloatValue(r)
  )
)

rootEnv.registerValue(
  "divf",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len < 1: call.invalidFormErrorWithReason("divf", "at least 1 argument")
      call.ensureArgOfType(x[0], 0, V_FLOAT)
      var r = x[0].fVal
      for i in 1..<x.len:
        call.ensureArgOfType(x[i], i, V_FLOAT)
        r = r / x[i].fVal
      return mkFloatValue(r)
  )
)

rootEnv.registerValue(
  "float",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 1: call.invalidFormErrorWithReason("float", "1 argument")
      call.ensureArgOfType(x[0], 0, @[V_FLOAT, V_INTEGER])
      if x[0].vType == V_FLOAT: return x[0]
      return mkFloatValue(x[0].iVal.float)
  )
)

rootEnv.registerValue(
  "floor",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 1: call.invalidFormErrorWithReason("floor", "1 argument")
      call.ensureArgOfType(x[0], 0, @[V_FLOAT, V_INTEGER])
      if x[0].vType == V_INTEGER: return x[0]
      return mkIntegerValue(x[0].fVal.floor.int)
  )
)

rootEnv.registerValue(
  "ceil",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 1: call.invalidFormErrorWithReason("ceil", "1 argument")
      call.ensureArgOfType(x[0], 0, @[V_FLOAT, V_INTEGER])
      if x[0].vType == V_INTEGER: return x[0]
      return mkIntegerValue(x[0].fVal.ceil.int)
  )
)

rootEnv.registerValue(
  "round",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 1: call.invalidFormErrorWithReason("round", "1 argument")
      call.ensureArgOfType(x[0], 0, @[V_FLOAT, V_INTEGER])
      if x[0].vType == V_INTEGER: return x[0]
      return mkIntegerValue(x[0].fVal.round.int)
  )
)

rootEnv.registerValue(
  "trunc",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 1: call.invalidFormErrorWithReason("trunc", "1 argument")
      call.ensureArgOfType(x[0], 0, @[V_INTEGER, V_FLOAT])
      if x[0].vType == V_INTEGER: return x[0]
      return mkIntegerValue(x[0].fVal.trunc.int)
  )
)

rootEnv.registerValue(
  "float?",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 1: call.invalidFormErrorWithReason("int?")
      return (x[0] != nil and x[0].vType == V_FLOAT).verdictValue
  )
)

rootEnv.registerValue(
  "eqnum",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 2: call.invalidFormErrorWithReason("eqnum", "1 argument")
      call.ensureArgOfType(x[0], 0, @[V_INTEGER, V_FLOAT])
      call.ensureArgOfType(x[1], 1, @[V_INTEGER, V_FLOAT])
      if x[0].vType == V_FLOAT or x[1].vType == V_FLOAT:
        let a = if x[0].vType == V_INTEGER: x[0].iVal.float else: x[0].fVal
        let b = if x[1].vType == V_INTEGER: x[1].iVal.float else: x[1].fVal
        return (a == b).verdictValue
      else:
        let a = x[0].iVal
        let b = x[1].iVal
        return (a == b).verdictValue
  )
)

rootEnv.registerValue(
  "leqf",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 2: call.invalidFormErrorWithReason("leq", "2 arguments")
      call.ensureArgOfType(x[0], 0, V_FLOAT)
      call.ensureArgOfType(x[1], 1, V_FLOAT)
      return (x[0].fVal <= x[1].fVal).verdictValue
  )
)

rootEnv.registerValue(
  "ltf",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 2: call.invalidFormErrorWithReason("leq", "2 arguments")
      call.ensureArgOfType(x[0], 0, V_FLOAT)
      call.ensureArgOfType(x[1], 1, V_FLOAT)
      return (x[0].fVal < x[1].fVal).verdictValue
  )
)

rootEnv.registerValue(
  "geqf",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 2: call.invalidFormErrorWithReason("leq", "2 arguments")
      call.ensureArgOfType(x[0], 0, V_FLOAT)
      call.ensureArgOfType(x[1], 1, V_FLOAT)
      return (x[0].fVal >= x[1].fVal).verdictValue
  )
)

rootEnv.registerValue(
  "gtf",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 2: call.invalidFormErrorWithReason("leq", "2 arguments")
      call.ensureArgOfType(x[0], 0, V_FLOAT)
      call.ensureArgOfType(x[1], 1, V_FLOAT)
      return (x[0].fVal > x[1].fVal).verdictValue
  )
)

rootEnv.registerValue(
  "begin",
  mkSpecialFormValue(
    proc (x: seq[Node], tail: Node, e: Env, call: Node): Value =
      if tail != nil: tail.invalidFormErrorWithReason("while")
      return x.evalMulti(e)
  )
)

rootEnv.registerValue(
  "while",
  mkSpecialFormValue(
    proc (x: seq[Node], tail: Node, e: Env, call: Node): Value =
      if tail != nil: tail.invalidFormErrorWithReason("while")
      if x.len != 2: call.invalidFormErrorWithReason("while", "2 arguments")
      let cond = x[0]
      let body = x[1]
      while cond.evalSingle(e).isValueNotFalse():
        discard body.evalSingle(e)
      return nil
  )
)

rootEnv.registerValue("eof", GlobalEOFValue)

rootEnv.registerValue(
  "length",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 1: call.invalidFormErrorWithReason("length", "1 argument")
      if not x[0].isValueAList: call.errorWithReason("Argument must be a list.")
      var r = 0
      var subj = x[0]
      while subj != nil:
        subj = subj.cdr
        r += 1
      return mkIntegerValue(r)
  )
)

block:
  let v = mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      var i = 0
      var r: seq[Value] = @[]
      while i < x.len:
        if not x[i].isValueAList: call.errorWithReason("Argument must be a list.")
        let p = x[i].valueListToSeq()
        for v in p: r.add(v)
        i += 1
      return r.seqToValueList()
  )
  rootEnv.registerValue("append", v)
  rootEnv.registerValue("list++", v)

rootEnv.registerValue(
  "vec++",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      var i = 0
      var r: seq[Value] = @[]
      while i < x.len:
        call.ensureArgOfType(x[i], i, V_VECTOR)
        r = r.concat(x[i].vVal)
        i += 1
      return mkVectorValue(r)
  )
)

rootEnv.registerValue(
  "veclen",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 1: call.invalidFormErrorWithReason("veclen", "1 argument")
      call.ensureArgOfType(x[0], 0, V_VECTOR)
      return mkIntegerValue(x[0].vVal.len)
  )
)

rootEnv.registerValue(
  "set-car!",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 2: call.invalidFormErrorWithReason("set-car!", "2 argument")
      call.ensureArgOfType(x[0], 0, V_PAIR)
      x[0].car = x[1]
      return nil
  )
)

rootEnv.registerValue(
  "set-cdr!",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 2: call.invalidFormErrorWithReason("set-car!", "2 argument")
      call.ensureArgOfType(x[0], 0, V_PAIR)
      x[0].cdr = x[1]
      return nil
  )
)

rootEnv.registerValue(
  "w/car",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 2: call.invalidFormErrorWithReason("w/car", "2 argument")
      call.ensureArgOfType(x[0], 0, V_PAIR)
      return mkPairValue(x[1], x[0].cdr)
  )
)

rootEnv.registerValue(
  "w/cdr",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 2: call.invalidFormErrorWithReason("w/cdr", "2 argument")
      call.ensureArgOfType(x[0], 0, V_PAIR)
      return mkPairValue(x[0].car, x[1])
  )
)
