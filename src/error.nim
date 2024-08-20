import source

type
  FMError* = ref object
    line*: int
    col*: int
    fileName*: string
    reason*: string

var errorList: seq[FMError] = @[]

proc initErrorList*(): void =
  errorList = @[]

proc registerError*(reason: string): void =
  let fn = getCurrentFileName()
  let lc = getCurrentLineCol()
  errorList.add(FMError(line: lc.line, col: lc.col, fileName: fn, reason: reason))

# NOTE: this is called by src/flowmark.nim, not here.
proc reportAllError*(): void =
  for x in errorList:
    stderr.write(x.fileName & "(" & $(x.line+1) & "," & $(x.col+1) & "): " & x.reason & "\n")
    stderr.flushFile()
  errorList = @[]

  
