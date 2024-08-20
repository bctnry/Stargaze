
proc errorWithReason*(x: string): void =
  raise newException(ValueError, x)

proc isHexDigit*(x: char): bool =
  ('0' <= x and x <= '9') or ('a' <= x and x <= 'f') or ('A' <= x and x <= 'F')
  
proc hexToInt*(xs: string): int =
  var res = 0
  for x in xs:
    res *= 16
    if '0' <= x and x <= '9':
      res += x.ord - '0'.ord
    elif 'a' <= x and x <= 'f':
      res += (x.ord - 'a'.ord) + 10
    elif 'A' <= x and x <= 'F':
      res += (x.ord - 'A'.ord) + 10
  return res

proc intToHex*(x: int): string =
  if x == 0: return "0"
  var preres = ""
  var subj = x
  var i = 0
  while subj > 0:
    let v = subj mod 16
    preres.add(if v >= 10:
                 (v - 10 + 'a'.ord).chr
               else:
                 (v + '0'.ord).chr)
    subj = subj div 16
    i += 1
  i -= 1
  var res = ""
  while i >= 0:
    res.add(preres[i])
    i -= 1
  return res
  
