// my first f# program LOL

open System.IO

type Dir =
   | L
   | R   

type Command = {
   dir: Dir
   amount: int
}

type State = {
   knob: int 
   clicks: int
}

let knobbify  min max value= 
   let max' = max + 1
   match value with
      | v when v < min -> (max' - abs v) % max'
      | v when v > max -> v % max'
      | _ -> value

let movedKnob amount state = 
   if amount = 0 then state else
      let knobbify' = knobbify 0 99
      match knobbify' (state.knob + amount) with
         | v when v = 0 -> { knob = v; clicks = state.clicks + 1 }
         | v -> { knob = v; clicks = state.clicks }

let parseCmd (line: string): Command =
   let dir = 
      match line.Substring(0, 1) with 
         | "L" -> L
         | "R" -> R 
         | _ -> failwith "Invalid direction"
   let amount = 
      line.Substring(1).Trim() |> int
   { dir = dir; amount = amount}

let interpret  (state: State) (cmd: Command) = 
   let amount = 
      match cmd.dir with
      | L -> -cmd.amount
      | R -> cmd.amount
   movedKnob amount state
   

let main  = 
   let input = @"./input.txt"
   // let input = @"./test.txt"
   let fileStream = File.OpenText input
   let initial = { knob = 50; clicks = 0 }
   let rec loop state = 
      let state' =
         fileStream.ReadLine() 
         |> parseCmd  
         |> interpret state 
      if fileStream.EndOfStream then state' else loop state'
   let result = loop initial
   0

main