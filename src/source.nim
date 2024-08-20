import std/options
import filelike

type
  SourceFileStack* = seq[Filelike]
var sourceFileStack: SourceFileStack = @[]

proc registerSourceFile*(f: File, name: string = "<unnamed>"): void =
  var fl = f.mkNormalFilelike
  fl.name = name
  fl.line = 0
  fl.col = 0
  sourceFileStack.add(fl)

proc registerSourceString*(s: string, name: string = "<unnamed>"): void =
  var fl = s.mkStringFilelike
  fl.name = name
  fl.line = 0
  fl.col = 0
  sourceFileStack.add(fl)

proc registerREPL*(): void =
  var fl = "".mkStringFilelike
  fl.name = "<repl>"
  fl.line = 0
  fl.col = 0
  sourceFileStack.add(fl)

proc addToREPL*(s: string): void =
  var repl = sourceFileStack[^1]
  assert repl.fType == STRING_FILE
  assert repl.name == "<repl>"
  repl.resetStringFilelikeWith(s)

proc useSourceFile*(path: string): void =
  let f = open(path, fmRead)
  registerSourceFile(f, path)
  
proc getCurrentSourceFile*(): Filelike =
  return sourceFileStack[^1]
  
proc getCurrentFileName*(): string =
  return getCurrentSourceFile().name
proc getCurrentLineCol*(): tuple[line: int, col: int] =
  let fl = getCurrentSourceFile()
  return (line: fl.line, col: fl.col)
