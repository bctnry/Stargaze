import std/tables
import defs

let rootEnvTable: TableRef[string, Value] = newTable[string, Value]()
let rootEnv* = mkEnv(rootEnvTable)


