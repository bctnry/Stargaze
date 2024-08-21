import std/tables
import std/options
import defs

let rootEnvTable: TableRef[string, Value] = newTable[string, Value]()
let rootEnv* = mkEnv(rootEnvTable)

proc newEnvFromRootEnv*(): Env =
  var x = newTable[string, Value]()
  return mkEnv(x, rootEnv)
var currentEnv: Env = nil
proc initNewEnv*(): void =
  currentEnv = newEnvFromRootEnv()
proc getCurrentEnv*(): Env = currentEnv
proc setCurrentEnv*(e: Env): void = currentEnv = e

# NOTE: the reason why it's (string, Node) is purely cosmetics; the
# error msg generation down the line uses the metadata embedded in
# the Node value.
var exportList: seq[(string, Node)] = @[]
proc prepareForNewModule*(): void =
  exportList = @[]
proc exportName*(x: string, call: Node): void =
  exportList.add((x, call))
proc getCurrentExportList*(): seq[(string, Node)] = exportList
proc restoreCurrentExportList*(x: seq[(string, Node)]): void =
  exportList = x

var globalImportedModuleTable = newTable[string, (Env, seq[(string, Node)])]()
proc registerImportedModule*(name: string, env: Env, exportList: seq[(string, Node)]): void =
  globalImportedModuleTable[name] = (env, exportList)
proc tryGetImportedModule*(name: string): Option[(Env, seq[(string, Node)])] =
  if name in globalImportedModuleTable:
    return some(globalImportedModuleTable[name])
  else:
    return none((Env, seq[(string, Node)]))
    


