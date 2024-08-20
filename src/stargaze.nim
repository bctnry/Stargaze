import std/syncio
import std/options
import std/cmdline
import std/tables
import std/strutils
from std/paths import getCurrentDir, parentDir, Path
import defs
import filelike
import parser
import core
import session
import primitives
import cmdargparse
import source
import path
import error

proc processCurrentSourceFile(): Value =
  var fl = getCurrentSourceFile()
  var parseRes = fl.parseMultiNode()
  var evalRes = parseRes.evalMulti(rootEnv)
  return evalRes

proc readStr(p: File): Option[string] =
  var res: string = ""
  try:
    while true:
      let ch = p.readChar()
      if ch == '\n':
        return some(res)
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
      let z = readStr(stdin)
      if z.isNone(): break
      z.get().addToREPL()
      try:
        let res = processCurrentSourceFile()
        echo res
      except:
        discard
      reportAllError()

#when isMainModule:
  # echo "def a [x y] [add x y]".parseMulti[0].evalMulti(rootEnv)
  # echo "3 4 5 6 7 @[add abc abc] def a [x y] [add x y] a 3 4".parseMulti[0].evalMulti(rootEnv)
  # echo "if false 3 [add abc abc]".parseMulti[0].evalMulti(rootEnv)
  # echo "3 4 5 + 6 7 [&add abc abc]".parseMulti[0]
#  var x = "(def blah #t) (quote (if (atom 3) (add 3 4) (if #f 4 5)))".mkStringFilelike()
  # var x = "if #t".mkParseState()
  # echo x.parseMultiNode()
#  echo x.parseMultiNode.evalMulti(rootEnv)
  # [0].evalSingle(rootEnv)
#  discard nil
  
