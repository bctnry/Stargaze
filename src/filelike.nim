import std/syncio
import std/options

type
  FilelikeType* = enum
    STRING_FILE
    NORMAL_FILE
  Filelike* = ref object
    name*: string
    line*: int
    col*: int
    case fType*: FilelikeType
    of STRING_FILE:
      i*: int
      sVal*: string
      sValLen: int
    of NORMAL_FILE:
      fVal*: File
      charbuf: Option[char]

proc mkNormalFilelike*(f: File): Filelike =
  Filelike(name: "", fType: NORMAL_FILE, fVal: f)
proc mkStringFilelike*(s: string): Filelike =
  Filelike(name: "", fType: STRING_FILE, i: 0, sVal: s, sValLen: s.len)

proc resetStringFilelike*(fl: var Filelike): void =
  fl.i = 0
  fl.line = 0
  fl.col = 0
proc resetStringFilelikeWith*(fl: var Filelike, s: string): void =
  fl.sVal = s
  fl.i = 0
  fl.sValLen = s.len
  fl.line = 0
  fl.col = 0

proc readChar*(fl: var Filelike): char =
  case fl.fType:
    of NORMAL_FILE:
      if fl.charbuf.isSome():
        let res = fl.charbuf.get
        if res == '\n':
          fl.line += 1
          fl.col = 0
        else:
          fl.col += 1
        fl.charbuf = none(char)
        return res
      else:
        return fl.fVal.readChar()
    of STRING_FILE:
      if fl.i >= fl.sValLen:
        raise newException(EOFError, "eof")
      else:
        let res = fl.sVal[fl.i]
        if res == '\n':
          fl.line += 1
          fl.col = 0
        else:
          fl.col += 1
        fl.i += 1
        return res

proc peekChar*(fl: var Filelike): char =
  case fl.fType:
    of NORMAL_FILE:
      if fl.charbuf.isSome():
        return fl.charbuf.get()
      let r = fl.readChar()
      fl.charbuf = some(r)
      return r
    of STRING_FILE:
      if fl.i >= fl.sValLen:
        raise newException(EOFError, "eof")
      else:
        let res = fl.sVal[fl.i]
        return res
        
