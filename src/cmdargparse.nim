import std/tables
import std/strformat

type
  ArgDescriptor* = tuple
    fullKey: string
    shortKey: string
    takeValue: bool

proc parseCmdArgs*(x: seq[ArgDescriptor], a: seq[string]): (TableRef[string,string], int) =
  var res = newTable[string,string]()
  var i = 0
  let lena = a.len
  while i < lena:
    var foundMatch = false
    for k in x:
      if k.fullKey == a[i] or k.shortKey == a[i]:
        foundMatch = true
        if k.takeValue:
          if i+1 >= lena: raise newException(ValueError, &"{a[i]} takes an argument.")
          res[k.fullKey] = a[i+1]
          i += 2
        else:
          res[k.fullKey] = k.fullKey
          i += 1
        break
    if not foundMatch: return (res, i)
  return (res, i)

