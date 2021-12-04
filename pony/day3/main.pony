use "files"
use "promises"

interface EatLine
  be eatLine(line : (String val | None))

class Tree
  var zero : (Tree ref | I32 | None)
  var one : (Tree ref | I32 | None)
  let mask : I32

  new create(mask' : I32) =>
    zero = None
    one = None
    mask = mask'

  fun ref add(v : I32) =>
    let x = v and mask 
    if x == 0 then
      match zero 
      | let t : Tree ref => 
        t.add(v)
      | let i : I32 =>
        let mask' = mask / 2
        if mask' > 0 then
          let t = Tree(mask')
          t.add(v)
          t.add(i)
          zero = t
        end
      | None => 
        zero = v
      end
    else
      match one
      | let t : Tree ref =>
        t.add(v)
      | let i : I32 =>
        let mask' = mask / 2
        if mask' > 0 then
          let t = Tree(mask')
          t.add(v)
          t.add(i)
          one = t
        end
      | None =>
        one = v
      end
    end

actor TreeBuilder
  let tree : Tree
  
  new create(mask : I32) =>
    tree = Tree(mask)

  be eatLine(line : (String val | None)) =>
    match line
    | let s : String =>
      try
        let v = s.i32(2)?
        tree.add(v)
      end
    end

actor KeyBuilder
  let bits : Array[I32]
  let output : OutStream
 
  new create(output' : OutStream) =>
    output = output'
    bits = Array[I32].init(0,12)  

  fun bitsToGamma() : I32 =>
    var r : I32 = 0
    for v in bits.values() do
      r = 2 * r
      if v > 0 then
        r = r + 1
      end
    end
    r

  fun gammaToEpsilon(gamma : I32) : I32 =>
    0b111111111111 - gamma
  
  be eatLine(line : (String val | None)) =>
    match line 
    | let s : String =>
      let i = s.size()
      var x : USize = 0
      for c in s.values() do
        try 
          let vx = bits(x)?
          if c == '0' then
            bits.update(x, vx - 1)?
          else
            bits.update(x, vx + 1)?
          end
          x = x + 1
        end
      end
    | None =>
      let gamma = bitsToGamma()
      let epsilon = gammaToEpsilon(gamma)
      let power : I32 = gamma * epsilon
      output.print(power.string())    
 end
   

actor Lines
  let path : FilePath
  let fileLines : (FileLines | None)
 
  new create(auth: FileAuth, path' : String) =>
    path = FilePath(auth, path')
    match OpenFile(path)
    | let file : File => 
      fileLines = FileLines(file)
    else
      fileLines = None
    end

  be run(targets : Array[EatLine tag] val) =>
    match fileLines 
    | let fl : FileLines =>
      for line in fl do
        let l : String val = consume line 
        for t in targets.values() do
          t.eatLine(l)
        end
      end
      for t in targets.values() do
        t.eatLine(None)
      end
    else
      for t in targets.values() do
        t.eatLine(None)
      end
    end

actor Writer
  let output : OutStream
 
  new create(output' : OutStream) =>
    output = output'

  be eatLine(line : (String | None)) =>
    match line
    | let line' : String =>
      output.print(line')
    end

actor Main
  new create(env: Env) =>
    try 
      let auth = FileAuth.create(env.root as AmbientAuth)
      let lines = Lines(auth, "input.txt")
      let writer = Writer(env.out)
      let keyBuilder = KeyBuilder(env.out)
      let treeBuilder = TreeBuilder(0b100000000000)
      let targets : Array[EatLine tag] val = [ keyBuilder; treeBuilder ]
      lines.run(targets)
    end