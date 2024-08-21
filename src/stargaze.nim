import std/syncio
import std/options
import std/cmdline
import std/tables
from std/paths import getCurrentDir, parentDir, Path
import defs
import parser
import core
import session
import cmdargparse
import source
import path
import error

# DO NOT REMOVE THE FOLLOWING LINE - this is used to initialize primitives.
import primitives

initNewEnv()
proc processCurrentSourceFile*(): Value =
  var fl = getCurrentSourceFile()
  var envPage = getCurrentEnv().page
  var parseRes = fl.parseMultiNode()
  # if hasError(): raise newException(ValueError, "")
  prepareForNewModule()
  var evalRes = parseRes.evalMulti(getCurrentEnv())
  for kp in getCurrentExportList():
    let name = kp[0]
    let call = kp[1]
    if not (name in envPage):
      registerError(call.filename, call.line, call.col, "Name '" & name & "' not found in module")
  return evalRes
  
proc readStrForREPL(p: File): Option[string] =
  var res: string = ""
  try:
    while true:
      let ch = p.readChar()
      if ch == '\n':
        return some(res)
      elif ch == '\r':
        res.add('\n')
      else:
        res.add(ch)
  except:
    if res.len() > 0: return some(res)
    else: return none(string)

let helpStr = """
Usage: stargaze [options] [file]
stargaze         -    start repl.
stargaze [file]  -    use [file] as input (but don't start repl)

Options:
    -v          -    show version.
    -h          -    show help.
    -i          -    start repl after the source file is processed.
"""

when isMainModule:
  var prompt = "% "
  var file = stdin
  var replMode = false
  var args: seq[string] = @[]
  var outTarget: seq[string] = @[]
  for i in 0..<paramCount():
    args.add(paramStr(i+1))
  if paramCount() >= 1:
    let table = @[
      (fullKey: "--version", shortKey: "-v", takeValue: false),
      (fullKey: "--help", shortKey: "-h", takeValue: false),
      (fullKey: "--interactive", shortKey: "-i", takeValue: false),
    ].parseCmdArgs(args)
    if table[0].hasKey("--version"):
      echo "0.1.0"
      quit(0)
    elif table[0].hasKey("--help"):
      echo helpStr
      quit(0)
    else:
      if table[0].hasKey("--interactive"):
        replMode = true
      if table[1] >= paramCount():
        registerSourceFile(file, "__stdin__")
        registerPathResolvingBase(getCurrentDir().string)
      else:
        let fileName = paramStr(table[1]+1)
        file = open(fileName,  fmRead)
        registerSourceFile(file, fileName)
        registerPathResolvingBase(fileName.Path.parentDir.string)
  else:
    registerSourceFile(file, "__stdin__")
    registerPathResolvingBase(getCurrentDir().string)
    replMode = true

  if file != stdin:
    discard processCurrentSourceFile()
    reportAllError()
  if file != stdin: file.close()
  if replMode:
    registerREPL()
    prompt = "% "
    while true:
      stdout.write(prompt)
      stdout.flushFile()
      let z = readStrForREPL(stdin)
      if z.isNone(): break
      z.get().addToREPL()
      try:
        let res = processCurrentSourceFile()
        echo res
      except:
        discard
      reportAllError()

      
