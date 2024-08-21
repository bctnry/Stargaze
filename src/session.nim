import std/tables
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
  
var importList: seq[ImportDescriptor] = @[]
var exportList: seq[string] = @[]
proc prepareForNewModule*(): void =
  importList = @[]
  exportList = @[]
proc getCurrentImportList*(): seq[ImportDescriptor] = importList
proc getCurrentExportList*(): seq[string] = exportList

