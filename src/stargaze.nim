import defs
import parser
import core
import session
import primitives

  
when isMainModule:
  # echo "def a [x y] [add x y]".parseMulti[0].evalMulti(rootEnv)
  # echo "3 4 5 6 7 @[add abc abc] def a [x y] [add x y] a 3 4".parseMulti[0].evalMulti(rootEnv)
  # echo "if false 3 [add abc abc]".parseMulti[0].evalMulti(rootEnv)
  # echo "3 4 5 + 6 7 [&add abc abc]".parseMulti[0]
  var x = "(def blah #t) (quote (if (atom 3) (add 3 4) (if #f 4 5)))".mkParseState()
  # var x = "if #t".mkParseState()
  # echo x.parseMultiNode()
  echo x.parseMultiNode.evalMulti(rootEnv)
  # [0].evalSingle(rootEnv)
  discard nil
  
