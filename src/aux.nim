
proc errorWithReason*(x: string): void =
  raise newException(ValueError, x)
  
