use "files"
use "promises"

primitive O2
primitive CO2

type Life is (O2 | CO2)

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

  fun ref find(key : Array[I32] val, life: Life, index: USize) : (I32 | None) =>
    try 
      let v = key(index)?
    match life 
    | O2 =>
      if (v >= 0) then
        match one
        | let t : Tree =>
           t.find(key, life, index + 1)
        | let i : I32 => 
           i
        | None => None
        end
      else
        match zero
        | let t : Tree =>
           t.find(key, life, index + 1)
        | let i : I32 => 
           i
        | None => None
        end
      end    
    | CO2 =>
      if (v >= 0) then
        match zero
        | let t : Tree =>
           t.find(key, life, index + 1)
        | let i : I32 => 
           i
        | None => None
        end
      else
        match one
        | let t : Tree =>
           t.find(key, life, index + 1)
        | let i : I32 => 
           i
        | None => None
        end
       end
     end
   end

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
  var tree : Tree ref
  let promise : Promise[Array[I32] val]
  let mask : I32
  let output : OutStream
  
  new create(output' : OutStream, mask' : I32, promise' : Promise[Array[I32] val]) =>
    mask = mask'
    tree = Tree(mask)
    promise = promise'
    output = output'

  be doStuff(x : Array[I32] val) =>
    let a = tree.find(x, O2, 0)
    match a
    | let a' : I32 => 
      let b = tree.find(x, CO2, 0)
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
       let self = recover tag this end
       promise.next[None]({ (x : Array[I32] val) => self.doStuff(x)})
    end

actor KeyBuilder
  let bits : Array[I32]
  let output : OutStream
  let promise : Promise[Array[I32] val]
 
  new create(output' : OutStream, promise' : Promise[Array[I32] val]) =>
    output = output'
    bits = Array[I32].init(0,12)
    promise = promise'

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
      let copy : Array[I32] iso = recover Array[I32] end
      for v in bits.values() do
        copy.push(v)
      end
      promise(consume copy) 
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
      let key = Promise[Array[I32] val]
      let keyBuilder = KeyBuilder(env.out, key)
      let treeBuilder = TreeBuilder(env.out, 0b100000000000, key)
      let targets : Array[EatLine tag] val = [ keyBuilder; treeBuilder ]
      lines.run(targets)
    end