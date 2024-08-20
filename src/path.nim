## path management.

import std/options
from std/envvars import getEnv, existsEnv
from std/paths import getCurrentDir, `/`, Path, changeFileExt
from std/strutils import split
from std/os import fileExists

var pathResolvingBaseList: seq[string] = @[]

let envBase = getEnv("STARGAZE_IMPORT_PATH")
if envBase != "":
  for x in envBase.split(';'):
    pathResolvingBaseList.add(x)

proc registerPathResolvingBase*(x: string): void =
  pathResolvingBaseList.add(x)

proc resolveModuleByName*(x: string): Option[string] =
  # Starting from the end so the ones added later always get higher priority
  let xr = x.Path.changeFileExt(".sg")
  var i = pathResolvingBaseList.len()-1
  while i >= 0:
    let p: Path = pathResolvingBaseList[i].Path / xr
    if not p.string.fileExists:
      i -= 1
      continue
    return some(p.string)
  return none(string)

