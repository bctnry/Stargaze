import std/tables
import defs

let rootEnvTable: TableRef[string, Value] = newTable[string, Value]()
let rootEnv* = mkEnv(rootEnvTable)

proc newEnvFromRootEnv*(): Env =
  var x = newTable[string, Value]()
  return mkEnv(x, rootEnv)


