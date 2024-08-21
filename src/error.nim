import source
import defs

type
  FMError* = ref object
    line*: int
    col*: int
    fileName*: string
    reason*: string

proc `$`*(x: FMError): string =
  x.fileName & "(" & $x.line & ":" & $x.col & "): " & x.reason

var errorList: seq[FMError] = @[]

proc initErrorList*(): void =
  errorList = @[]

proc registerError*(reason: string): void =
  let fn = getCurrentFileName()
  let lc = getCurrentLineCol()
  errorList.add(FMError(line: lc.line, col: lc.col, fileName: fn, reason: reason))
 
proc registerError*(filename: string, line: int, col: int, reason: string): void =
  errorList.add(FMError(line: line, col: col, fileName: filename, reason: reason))

proc registerError*(node: Node, reason: string): void =
  let fn = node.filename
  let line = node.line
  let col = node.col
  registerError(fn, line, col, reason)

proc hasError*(): bool =
  errorList.len > 0
  
# NOTE: this is called by src/stargaze.nim, not here.
proc reportAllError*(): void =
  for x in errorList:
    stderr.write(x.fileName & "(" & $(x.line+1) & "," & $(x.col+1) & "): " & x.reason & "\n")
    stderr.flushFile()
  errorList = @[]

  
