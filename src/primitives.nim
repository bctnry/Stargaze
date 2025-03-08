import std/tables
import std/sequtils
import std/syncio
import std/options
import std/strutils
import std/bitops
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

proc ensureArgOfType(n: Node, v: Value, i: int, t: ValueType): void =
  if v.vType != t: n.typeErrorWithReason(t, i, v.vType)

proc ensureArgOfType(n: Node, v: Value, i: int, t: seq[ValueType]): void =
  if not t.contains(v.vType): n.typeErrorWithReason(t, i, v.vType)

proc verdictValue(x: bool): Value =
  if x: GlobalTrueValue else: GlobalFalseValue

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

rootEnv.registerValue(
  "intstr",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 1: call.invalidFormErrorWithReason("instr", "1 argument")
      call.ensureArgOfType(x[0], 0, V_INTEGER)
      return mkStrValue($x[0].iVal)
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
proc isBooleanishlyFalse(x: Value): bool {.inline.} =
  return x.isNil or (x.vType == V_BOOL and x.bVal == false) or (x.vType == V_INTEGER and x.iVal == 0)

rootEnv.registerValue(
  "and",
  mkSpecialFormValue(
    proc (x: seq[Node], tail: Node, e: Env, call: Node): Value =
      if tail != nil: tail.invalidFormErrorWithReason("and")
      var lastVal = GlobalTrueValue
      for k in x:
        let kres = k.evalSingle(e)
        lastVal = kres
        if kres.isBooleanishlyFalse():
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
        if not kres.isBooleanishlyFalse():
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
      if x.len != 1: call.invalidFormErrorWithReason("not", "1 argument")
      let kres = x[0].evalSingle(e)
      if kres.isBooleanishlyFalse():
        return GlobalTrueValue
      else:
        return GlobalFalseValue
  )
)

# (bool EXP1)
rootEnv.registerValue(
  "bool",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 1: call.invalidFormErrorWithReason("bool", "1 argument")
      if x[0].isBooleanishlyFalse():
        return GlobalFalseValue
      else:
        return GlobalTrueValue
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
  "bool?",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 1: call.invalidFormErrorWithReason("bool?")
      let r = x[0]
      return (r != nil and r.vType == V_BOOL).verdictValue
  )
)

rootEnv.registerValue(
  "equal",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 2: call.invalidFormErrorWithReason("equal")
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
  "floatstr",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 1: call.invalidFormErrorWithReason("floatstr", "1 argument")
      call.ensureArgOfType(x[0], 0, V_FLOAT)
      return ($x[0].fVal).mkStrValue()
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

rootEnv.registerValue(
  "map",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len < 2: call.invalidFormErrorWithReason("map", "at least 2 argument")
      call.ensureArgOfType(x[0], 0, @[V_CLOSURE, V_PRIMITIVE])
      var arghead: seq[Value] = x[1..<x.len]
      var resseq: seq[Value] = @[]
      block l1:
        while true:
          var argvec: seq[Value] = @[]
          for i in 0..<arghead.len:
            let k = arghead[i]
            if k == nil or k.vType != V_PAIR: break l1
            argvec.add(k.car)
            arghead[i] = k.cdr
          let r = if x[0].vType == V_CLOSURE:
                    x[0].applyClosure(argvec, nil, e)
                  else:
                    x[0].applyPrimitive(argvec, e, call)
          resseq.add(r)
      return resseq.seqToValueList()
  )
)

rootEnv.registerValue(
  "filter",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 2: call.invalidFormErrorWithReason("filter", "2 argument")
      call.ensureArgOfType(x[0], 0, @[V_CLOSURE, V_PRIMITIVE])
      if not x[1].isValueAList():
        call.errorWithReason("Type error: a proper LIST required but not found at argument no. 2")
      var r = x[1].valueListToSeq().filter(
        proc (v: Value): bool =
          let vv = if x[0].vType == V_CLOSURE:
                     x[0].applyClosure(@[v], nil, e)
                   else:
                     x[0].applyPrimitive(@[v], e, call)
          return not (vv.vType == V_BOOL and vv.bVal == false)
      )
      return r.seqToValueList()
  )
)

rootEnv.registerValue(
  "member",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 2: call.invalidFormErrorWithReason("member", "2 argument")
      call.ensureArgOfType(x[1], 1, V_PAIR)
      var subj = x[1]
      while (not subj.isNil) and subj.vType == V_PAIR:
        if x[0].valueEqual(subj.car): return subj
        subj = subj.cdr
      if subj != nil:
        call.errorWithReason("Type error: a proper LIST is required.")
      return GlobalFalseValue
  )
)

rootEnv.registerValue(
  "assoc",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 2: call.invalidFormErrorWithReason("assoc", "2 argument")
      call.ensureArgOfType(x[1], 1, V_PAIR)
      var subj = x[1]
      while (not subj.isNil) and subj.vType == V_PAIR:
        if subj.car == nil or subj.car.vType != V_PAIR:
          call.errorWithReason("Type error: non-pair found in argument no.2 of assoc")
        if subj.car.car.valueEqual(x[0]): return subj.car
        subj = subj.cdr
      if subj != nil:
        call.errorWithReason("Type error: non-pair found in argument no.2 of assoc")
      return GlobalFalseValue
  )
)

rootEnv.registerValue(
  "mkstr",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 2: call.invalidFormErrorWithReason("mkstr", "2 arguments")
      call.ensureArgOfType(x[0], 0, V_INTEGER)
      call.ensureArgOfType(x[1], 1, V_CHAR)
      var s = ""
      for k in 0..<x[0].iVal: s.add(x[1].chVal)
      return mkStrValue(s)
  )
)

rootEnv.registerValue(
  "strlen",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 1: call.invalidFormErrorWithReason("strlen", "1 argument")
      call.ensureArgOfType(x[0], 0, V_STRING)
      return mkIntegerValue(x[0].strval.len)
  )
)

rootEnv.registerValue(
  "strlist",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 1: call.invalidFormErrorWithReason("strlist", "1 argument")
      call.ensureArgOfType(x[0], 0, V_STRING)
      var r: seq[char] = @[]
      for k in x[0].strVal: r.add(k)
      return r.mapIt(mkCharValue(it)).seqToValueList()
  )
)

rootEnv.registerValue(
  "strvec",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 1: call.invalidFormErrorWithReason("strlist", "1 argument")
      call.ensureArgOfType(x[0], 0, V_STRING)
      var r: seq[char] = @[]
      return mkVectorValue(r.mapIt(mkCharValue(it)))
  )
)

rootEnv.registerValue(
  "str++",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      var res = ""
      for i in 0..<x.len:
        call.ensureArgOfType(x[i], i, V_STRING)
        res &= x[i].strval
      return mkStrValue(res)
  )
)

rootEnv.registerValue(
  "bit~",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 1: call.invalidFormErrorWithReason("bit~", "1 argument")
      call.ensureArgOfType(x[0], 0, V_INTEGER)
      return mkIntegerValue(x[0].iVal.bitnot)
  )
)

rootEnv.registerValue(
  "bit&",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len < 1: call.invalidFormErrorWithReason("bit&", "at least 1 argument")
      var res = x[0].iVal
      for i in 1..<x.len:
        call.ensureArgOfType(x[i], i, V_INTEGER)
        res = res and x[i].iVal
      return mkIntegerValue(res)
  )
)

rootEnv.registerValue(
  "bit^",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len < 1: call.invalidFormErrorWithReason("bit^", "at least 1 argument")
      var res = x[0].iVal
      for i in 1..<x.len:
        call.ensureArgOfType(x[i], i, V_INTEGER)
        res = res xor x[i].iVal
      return mkIntegerValue(res)
  )
)

rootEnv.registerValue(
  "bit<<",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 2: call.invalidFormErrorWithReason("bit<<", "2 argument")
      call.ensureArgOfType(x[0], 0, V_INTEGER)
      call.ensureArgOfType(x[1], 1, V_INTEGER)
      return mkIntegerValue(x[0].iVal shl x[1].iVal)
  )
)

rootEnv.registerValue(
  "bit>>",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 2: call.invalidFormErrorWithReason("bit>>", "2 argument")
      call.ensureArgOfType(x[0], 0, V_INTEGER)
      call.ensureArgOfType(x[1], 1, V_INTEGER)
      return mkIntegerValue(x[0].iVal shr x[1].iVal)
  )
)

rootEnv.registerValue(
  "closure?",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 1: call.invalidFormErrorWithReason("closure?", "1 argument")
      return (x[0].vType == V_CLOSURE).verdictValue
  )
)

rootEnv.registerValue(
  "primitive?",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 1: call.invalidFormErrorWithReason("closure?", "1 argument")
      return (x[0].vType == V_PRIMITIVE).verdictValue
  )
)  

rootEnv.registerValue(
  "procedure?",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 1: call.invalidFormErrorWithReason("closure?", "1 argument")
      return (x[0].vType == V_CLOSURE or x[0].vType == V_PRIMITIVE).verdictValue
  )
)  


rootEnv.registerValue(
  "struct?",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 1: call.invalidFormErrorWithReason("struct?", "1 argument")
      return (x[0].vType == V_STRUCT).verdictValue
  )
)

rootEnv.registerValue(
  "struct-major-label",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 1: call.invalidFormErrorWithReason("struct-major-label", "1 argument")
      call.ensureArgOfType(x[0], 0, V_STRUCT)
      return mkSymbolValue(x[0].sMajorLabel)
  )
)

rootEnv.registerValue(
  "struct-secondary-label",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 1: call.invalidFormErrorWithReason("struct-secondary-label", "1 argument")
      call.ensureArgOfType(x[0], 0, V_STRUCT)
      let sl = x[0].sSecondaryLabel
      return (if sl.len == 0: GlobalFalseValue
              else: mkSymbolValue(sl))
  )
)

rootEnv.registerValue(
  "structvec",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 1: call.invalidFormErrorWithReason("structvec", "1 argument")
      call.ensureArgOfType(x[0], 0, V_STRUCT)
      let ml = x[0].sMajorLabel
      let sl = x[0].sSecondaryLabel
      return mkVectorValue(@[mkSymbolValue(ml),
                             (if sl.len == 0: GlobalFalseValue
                              else: mkSymbolValue(sl))] & x[0].sFieldList)
  )
)

rootEnv.registerValue(
  "struct-arity",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 1: call.invalidFormErrorWithReason("struct-arity", "1 argument")
      call.ensureArgOfType(x[0], 0, V_STRUCT)
      return mkIntegerValue(x[0].sFieldList.len)
  )
)

rootEnv.registerValue(
  "vecstruct",
  mkPrimitiveValue(
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != 1: call.invalidFormErrorWithReason("vecstruct", "1 argument")
      call.ensureArgOfType(x[0], 0, V_VECTOR)
      if x[0].vVal.len < 2: return GlobalFalseValue
      var ml = ""
      if x[0].vVal[0].vType == V_SYMBOL: ml = x[0].vVal[0].sVal
      elif x[0].vVal[0].vType == V_STRING: ml = x[0].vVal[0].strVal
      else: call.errorWithReason("first element of vector must be symbol or string")
      var sl = ""
      if x[0].vVal[1].vType == V_SYMBOL: sl = x[0].vVal[1].sVal
      elif x[0].vVal[1].vType == V_STRING: sl = x[0].vVal[1].strVal
      elif x[0].vVal[1].vType == V_BOOL and x[0].vVal[1].bVal == false:
        sl = ""
      else: call.errorWithReason("second element of vector must be symbol or string or #f")
      return mkStructValue(ml, sl, x[0].vVal[2..^1])
  )
)

proc mkFieldAccessor(fName: string, n: int, ml: string, sl: string = ""): proc (x: seq[Value], e: Env, call: Node): Value =
    let nn = n
    return (
      proc(x: seq[Value], e: Env, call: Node): Value =
        if x.len != 1: call.invalidFormErrorWithReason(fName)
        call.ensureArgOfType(x[0], 0, V_STRUCT)
        if x[0].sMajorLabel != ml: call.errorWithReason("major label mismatch: " & ml & " required but " & x[0].sMajorLabel & " found.")
        if sl != "" and x[0].sSecondaryLabel != sl: call.errorWithReason("secondary label mismatch: " & sl & " required but " & x[0].sSecondaryLabel & " found.")
        return x[0].sFieldList[nn]
    )

proc mkConstructor(ml: string, constrName: string, constrArity: int): proc (x: seq[Value], e: Env, call: Node): Value =
  return (
    proc (x: seq[Value], e: Env, call: Node): Value =
      if x.len != constrArity: call.invalidFormErrorWithReason(constrName)
      return mkStructValue(ml, constrName, x)
  )
      

rootEnv.registerValue(
  "defstruct",
  mkSpecialFormValue(
    proc (x: seq[Node], tail: Node, e: Env, call: Node): Value =
      if tail != nil: tail.invalidFormErrorWithReason("defstruct")
      if x.len < 1: call.invalidFormErrorWithReason("defstruct")
      if x[0].nType != N_WORD: call.invalidFormErrorWithReason("defstruct")
      let ml = x[0].wVal
      let isSimple = x[1].nType == N_WORD
      for i in 1..<x.len:
        if (x[i].nType == N_WORD) != isSimple:
          call.invalidFormErrorWithReason("defstruct")
      let predicateName = ml & "?"
      e.registerValue(
        predicateName,
        mkPrimitiveValue(
          proc (x: seq[Value], e: Env, call: Node): Value =
            if x.len != 1: call.invalidFormErrorWithReason(predicateName)
            if x[0].vType != V_STRUCT: return GlobalFalseValue
            if x[0].sMajorLabel != ml: return GlobalFalseValue
            return GlobalTrueValue
        )
      )
      if isSimple:
        let arity = x.len-1
        e.registerValue(
          ml,
          mkPrimitiveValue(
            proc (x: seq[Value], e: Env, call: Node): Value =
              if x.len != arity: call.invalidFormErrorWithReason(ml)
              return mkStructValue(ml, "", x)
          )
        )
        for i in 0..<arity:
          let fieldName = x[i+1].wVal
          let fName = ml & "/" & fieldName
          e.registerValue(
            fName,
            mkPrimitiveValue(
              mkFieldAccessor(fName, i, ml)
            )
          )
      else:
        let clauseLen = x.len-1
        for i in 0..<clauseLen:
          let clause = x[i+1]
          # we don't support struct with variable len field list yet.
          if not clause.tail.isNil: clause.invalidFormErrorWithReason("defstruct")
          for j in 0..<clause.lVal.len:
            if clause.lVal[j].nType != N_WORD: clause.invalidFormErrorWithReason("defstruct")
          let constrName = clause.lVal[0].wVal
          let constrArity = clause.lVal.len-1
          e.registerValue(
            constrName,
            mkPrimitiveValue(
              mkConstructor(ml, constrName, constrArity)
            )
          )
          for j in 0..<constrArity:
            let getterName = ml & "/" & constrName & "/" & clause.lVal[j+1].wVal
            e.registerValue(
              getterName,
              mkPrimitiveValue(
                mkFieldAccessor(getterName, j, ml, constrName)
              )
            )
      return nil
  )
)

