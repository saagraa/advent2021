use "files"

primitive O2
primitive CO2

type Life is (O2 | CO2)

interface EatLine
  be eatLine(line : (String val | None))

class Tree
  var zero : (Tree ref | I32 | None)
  var one : (Tree ref | I32 | None)
  var count_zero : I32
  var count_one : I32
  let mask : I32

  new create(mask' : I32) =>
    zero = None
    one = None
    mask = mask'
    count_zero = 0
    count_one = 0

  fun ref getOne(life : Life) : (I32 | None) => 
    match one 
      | let t : Tree ref => t.get(life)
      | let i : I32 => i
      | None => None
    end

  fun ref getZero(life : Life) : (I32 | None) => 
    match zero 
      | let t : Tree ref => t.get(life)
      | let i : I32 => i
      | None => None
    end

  fun ref get(life : Life) : (I32 | None) => 
    match life 
      | O2 => 
         if count_one >= count_zero then
           this.getOne(life)
         else 
           this.getZero(life)
         end
      | CO2 => 
         if count_one >= count_zero then
           this.getZero(life)
         else
           this.getOne(life)
         end
    end

  fun ref add(v : I32) =>
    let x = v and mask 
    if x == 0 then
      count_zero = count_zero + 1
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
      count_one = count_one + 1
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
  var tree : Tree ref
  let mask : I32
  let output : OutStream
  
  new create(output' : OutStream, mask' : I32) =>
    mask = mask'
    tree = Tree(mask)
    output = output'

  be doStuff() =>
    let a = tree.get(O2)
    match a
    | let a' : I32 => 
      let b = tree.get(CO2)
      match b
      | let b' : I32 =>
        output.print("Life support:")
        output.print((a' * b').string())
      end
    end

  be eatLine(line : (String val | None)) =>
    match line
    | let s : String =>
      try
        let v = s.i32(2)?
        tree.add(v)
      end
    | None =>
       doStuff()
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
      output.print("Power:")
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

actor Main
  new create(env: Env) =>
    try 
      let auth = FileAuth.create(env.root as AmbientAuth)
      let lines = Lines(auth, "input.txt")
      let keyBuilder = KeyBuilder(env.out)
      let treeBuilder = TreeBuilder(env.out, 0b100000000000)
      let targets : Array[EatLine tag] val = [ keyBuilder; treeBuilder ]
      lines.run(targets)
    end