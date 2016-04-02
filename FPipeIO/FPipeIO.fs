(*
    code map - 0,0   0,1   .  .
               1,0   1,1 
               .           .
               .              .
    
    todo: translate file to code
*)


namespace FPipeIO
    open System.Collections.Generic

    module FPipeIO =
        type DataType =
            | Number of float32
            | Boolean of bool
            | Invocation
        
        type BinaryOperations = 
            | Add 
            | Mul 
            | And 
            | Or
            | Equality 

        type UnaryOperations =
            | Negation
            | Inverse
            | Not

        type PipeDirection = Right | Up | Left | Down    
        type IOOperation = Print | Read 

        type Digit = L0 | L1 | L2 | L3 | L4 | L5 | L6 | L7 | L8 | L9
        type Literal = 
            | IntLit of Digit
            | BoolLit of bool
            | InvocLit
        
        type CodeObject = 
            | Empty                      //behavior:  coded - do nothing (destroyes the information packet)
            | Pipe of PipeDirection      //behavior:  coded - flow according to direction
            | WaiterPipe of PipeDirection
            | IO of IOOperation          //behavior:  coded - (output goes down)
            | Lit of Literal             //behavior:  coded - (output goes down)
            | BinOp of BinaryOperations  //behavior:  coded - output goes down
            | UnaryOp of UnaryOperations //behavior:  coded - output goes down
            | Splitter                   //behavior:  coded - input from top, output goes to left down and right
            | Condition                  //behavior:  coded - input from top/bottom, (if there is more then one bool in the packet they are or'd)
//                                                            if false go left, else go right (by default, if there are no bools will go left)  
            | Invocer                    //behavior:  coded - flows invocation down

        type Code = CodeObject list list
        type State = System.Collections.Generic.Dictionary<int*int, DataType list>

        let charToCodeObject c = match c with
                                 | 'V' -> Pipe Down
                                 | '>' -> Pipe Right
                                 | '<' -> Pipe Left
                                 | '^' -> Pipe Up

                                 | 'U' -> WaiterPipe Down
                                 | ')' -> WaiterPipe Right
                                 | '(' -> WaiterPipe Left
                                 | 'A' -> Pipe Up

                                 | 'P' -> IO Print
                                 | 'R' -> IO Read

                                 | '0' -> Lit (IntLit L0)
                                 | '1' -> Lit (IntLit L1)
                                 | '2' -> Lit (IntLit L2)
                                 | '3' -> Lit (IntLit L3)
                                 | '4' -> Lit (IntLit L4)
                                 | '5' -> Lit (IntLit L5)
                                 | '6' -> Lit (IntLit L6)
                                 | '7' -> Lit (IntLit L7)
                                 | '8' -> Lit (IntLit L8)
                                 | '9' -> Lit (IntLit L9)
                                 | 'T' -> Lit (BoolLit true)
                                 | 'F' -> Lit (BoolLit false)
                                 | 'B' -> Lit InvocLit

                                 | '+' -> BinOp Add
                                 | '*' -> BinOp Mul
                                 | '&' -> BinOp And
                                 | '|' -> BinOp Or
                                 | '=' -> BinOp Equality

                                 | '-'  -> UnaryOp Negation 
                                 | '\\' -> UnaryOp Inverse
                                 | '~'  -> UnaryOp Not

                                 | '@' -> Splitter
                                 | 'S' -> Splitter

                                 | 'D' -> Condition

                                 | 'I' -> Invocer

                                 | _ -> Empty

        let litToValue lit = match lit with
                             | IntLit L0 -> Number 0.0f
                             | IntLit L1 -> Number 1.0f
                             | IntLit L2 -> Number 2.0f
                             | IntLit L3 -> Number 3.0f
                             | IntLit L4 -> Number 4.0f
                             | IntLit L5 -> Number 5.0f
                             | IntLit L6 -> Number 6.0f
                             | IntLit L7 -> Number 7.0f
                             | IntLit L8 -> Number 8.0f
                             | IntLit L9 -> Number 9.0f

                             | BoolLit b -> Boolean b

                             | InvocLit -> Invocation

        let toString dt = match dt with
                          | Invocation -> "I"
                          | Boolean b -> b.ToString()
                          | Number n -> n.ToString()

        let runCode (code : Code) (state : State) : State = 
            let run' (s : State) ((x,y) : int*int) = 
                let mutable state' = s
                if state'.ContainsKey(x,y) then 
                    match code.[y].[x] with
                    | Empty -> state'.Remove((x,y))  |> ignore
                    | Pipe pt ->
                                let temp = state'.[(x,y)]
                                state'.Remove((x,y)) |> ignore
                                match pt with
                                | Up -> 
                                    if (state'.ContainsKey((x,y-1))) then
                                        state'.[(x,y-1)] <- state'.[(x,y-1)] @ temp
                                    else
                                    state'.Add((x, y-1), temp)
                                | Down ->                                     
                                    if (state'.ContainsKey((x,y+1))) then
                                        state'.[(x,y+1)] <- state'.[(x,y+1)] @ temp
                                    else
                                    state'.Add((x, y+1), temp)
                                | Right ->                                    
                                    if (state'.ContainsKey((x+1,y))) then
                                        state'.[(x+1,y)] <- state'.[(x+1,y)] @ temp
                                    else
                                    state'.Add((x + 1, y), temp)
                                | Left -> 
                                    if (state'.ContainsKey((x-1,y))) then
                                         state'.[(x-1,y)] <- state'.[(x-1,y)] @ temp
                                    else
                                    state'.Add((x - 1, y), temp)
                    | WaiterPipe pd ->  let tmp = state.[(x,y)]
                                        if List.contains Invocation tmp
                                           && List.filter (fun x -> x = Invocation |> not) tmp
                                              |> List.length 
                                              |> (<) 0
                                        then state.Remove((x,y)) |> ignore
                                             let temp = List.filter ((=) Invocation >> not) tmp 
                                             match pd with
                                                 | Up -> 
                                                     if (state'.ContainsKey((x,y-1))) then
                                                         state'.[(x,y-1)] <- state'.[(x,y-1)] @ temp
                                                     else
                                                     state'.Add((x, y-1), temp)
                                                 | Down -> 
                                                     if (state'.ContainsKey((x,y+1))) then
                                                         state'.[(x,y+1)] <- state'.[(x,y+1)] @ temp
                                                     else
                                                     state'.Add((x, y+1), temp)
                                                 | Right -> 
                                                     if (state'.ContainsKey((x+1,y))) then
                                                         state'.[(x+1,y)] <- state'.[(x+1,y)] @ temp
                                                     else
                                                     state'.Add((x + 1, y), temp)
                                                 | Left -> 
                                                     if (state'.ContainsKey((x-1,y))) then
                                                          state'.[(x-1,y)] <- state'.[(x-1,y)] @ temp
                                                     else
                                                     state'.Add((x - 1, y), temp)
                    | IO io -> let temp : DataType list = state'.[(x,y)]
                               state'.Remove((x,y)) |> ignore 
                               match io with
                               | Print -> temp
                                          |> List.iter (toString >> (fun s -> (+) s " ") >> System.Console.Write) 
                               | Read -> if List.exists ((=) Invocation) temp then
                                            let value = let v' = System.Console.ReadKey().KeyChar.ToString().ToUpper().Chars(0)
                                                                 |> charToCodeObject
                                                        match v' with 
                                                        | Lit l -> (litToValue l, true)
                                                        | _ -> (Invocation, false)
                                            System.Console.Write(" ") 
                                            match value with
                                            | (v, true) -> 
                                                if (state'.ContainsKey((x,y+1))) then
                                                    state'.[(x,y+1)] <- state'.[(x,y+1)] @ [v]
                                                else state'.Add((x,y+1), [v])
                                            | _ -> ()   
                    | Lit lit -> let temp : DataType list = state'.[(x,y)]
                                 state'.Remove((x,y)) |> ignore 

                                 let value = match lit with
                                             | InvocLit -> Invocation
                                             | BoolLit x -> Boolean x
                                             | IntLit L0 -> Number 0.0f
                                             | IntLit L1 -> Number 1.0f
                                             | IntLit L2 -> Number 2.0f
                                             | IntLit L3 -> Number 3.0f
                                             | IntLit L4 -> Number 4.0f
                                             | IntLit L5 -> Number 5.0f
                                             | IntLit L6 -> Number 6.0f
                                             | IntLit L7 -> Number 7.0f
                                             | IntLit L8 -> Number 8.0f
                                             | IntLit L9 -> Number 9.0f
                                 if List.exists ((=) Invocation) temp then
                                    if state'.ContainsKey((x,y+1)) then
                                        state'.[(x,y+1)] <- state'.[(x,y+1)] @ [value]
                                    else state'.Add((x,y+1), [value])
                    | BinOp bin 
                                -> let temp = state'.[(x,y)]
                                   if temp.Length >= 2 then
                                        state.Remove((x,y)) |> ignore
                                        match bin with 
                                        | Add -> let nums = temp 
                                                            |> List.map (fun dt -> match dt with 
                                                                              | Number n -> n
                                                                              | _ -> 0.0f)
                                                 if state'.ContainsKey((x,y+1)) then
                                                    state'.[(x,y+1)] <- state'.[(x,y+1)]  @ [Number <| List.fold (+) 0.0f nums]
                                                 else state'.Add((x,y+1), [Number <| List.fold (+) 0.0f nums])
                                        | Mul -> let nums = temp 
                                                            |> List.map (fun dt -> match dt with 
                                                                              | Number n -> n
                                                                              | _ -> 1.0f)
                                                 if state'.ContainsKey((x,y+1)) then
                                                    state'.[(x,y+1)] <- state'.[(x,y+1)] @ [Number <| List.fold (*) 1.0f nums]
                                                 else state'.Add((x,y+1), [Number <| List.fold (*) 1.0f nums])
                                        | And -> let bools = temp 
                                                            |> List.map (fun dt -> match dt with 
                                                                              | Boolean b -> b
                                                                              | _ -> true)
                                                 if state'.ContainsKey((x,y+1)) then
                                                    state'.[(x,y+1)] <- state'.[(x,y+1)] @ [Boolean <| List.fold (&&) true bools]
                                                 else state'.Add((x,y+1), [Boolean <| List.fold (&&) true bools])
                                        | Or ->  let bools = temp 
                                                             |> List.map (fun dt -> match dt with 
                                                                               | Boolean b -> b
                                                                               | _ -> false)
                                                 if state'.ContainsKey((x,y+1)) then
                                                    state'.[(x,y+1)] <- state'.[(x,y+1)] @ [Boolean <| List.fold (||) false bools]
                                                 else state'.Add((x,y+1), [Boolean <| List.fold (||) false bools])
                                        | Eq -> let areEq = List.forall ((=) temp.Head) temp
                                                if state'.ContainsKey((x,y+1)) then
                                                    state'.[(x,y+1)] <- state'.[(x,y+1)] @ [Boolean <| areEq]
                                                 else state'.Add((x,y+1), [Boolean <| areEq])
                    | UnaryOp un -> match un with
                                    | Negation -> let temp = state'.[(x,y)]
                                                  state'.Remove((x,y)) |> ignore |> ignore
                                                  let ne = temp
                                                           |> List.map (fun dataO -> match dataO with
                                                                                | Number n -> Number (-n)
                                                                                | x -> x)
                                                  if state'.ContainsKey((x,y+1)) then
                                                    state'.[(x,y+1)] <- state'.[(x,y+1)] @ ne
                                                  else state'.Add((x,y+1), ne)
                                    | Inverse -> let temp = state'.[(x,y)]
                                                 state'.Remove((x,y)) |> ignore |> ignore
                                                 let ne = temp 
                                                          |> List.map (fun dataO -> match dataO with
                                                                               | Number n -> Number (1.0f/n)
                                                                               | x -> x)
                                                 if state'.ContainsKey((x,y+1)) then
                                                    state'.[(x,y+1)] <- state'.[(x,y+1)] @ ne
                                                 else state'.Add((x,y+1), ne)
                                    | Not -> let temp = state'.[(x,y)]
                                             state'.Remove((x,y)) |> ignore |> ignore
                                             let ne = temp 
                                                      |> List.map (fun dataO -> match dataO with
                                                                           | Boolean b -> Boolean (not b)
                                                                           | x -> x)
                                             if state'.ContainsKey((x,y+1)) then
                                                state'.[(x,y+1)] <- state'.[(x,y+1)] @ ne
                                             else state'.Add((x,y+1), ne)
                    | Splitter -> let temp = state'.[(x,y)]
                                  state'.Remove((x,y)) |> ignore |> ignore
                                  if state'.ContainsKey((x+1, y)) then
                                     state'.[(x+1,y)] <- state'.[(x+1,y)] @ temp
                                  else state'.Add((x+1,y), temp)
                                  if state'.ContainsKey((x-1, y)) then
                                     state'.[(x-1,y)] <- state'.[(x-1,y)] @ temp
                                  else state'.Add((x-1,y), temp)
                                  if state'.ContainsKey((x, y + 1)) then
                                     state'.[(x+1,y)] <- state'.[(x+1,y)] @ temp
                                  else state'.Add((x,y + 1), temp)                               
                    | Condition -> let temp = state'.[(x,y)]
                                   state'.Remove((x,y)) |> ignore
                                   if temp
                                         |> List.map (fun dataO -> match dataO with
                                                                        | Boolean x -> x
                                                                        | _ -> false) 
                                   
                                         |> List.fold (||) false
                                   then 
                                         if state'.ContainsKey((x+1,y)) then
                                             state'.[(x+1,y)] <- Invocation :: state'.[(x+1,y)]
                                         else
                                             state'.Add((x+1,y), [Invocation])                                        
                                   else
                                         if state'.ContainsKey((x-1,y)) then
                                             state'.[(x-1,y)] <- Invocation :: state'.[(x-1,y)]
                                         else
                                             state'.Add((x-1,y), [Invocation])                              
                    | Invocer -> state'.Remove((x,y)) |> ignore 
                                 state'.[(x,y+1)] <- [Invocation]
                state'

            let mutable keys = Array.zeroCreate state.Keys.Count            
            state.Keys.CopyTo(keys, 0)            
            keys 
            |> Array.toList
            |> List.filter (fun (x,y) -> x >= 0 && y >= 0)
            |> List.fold run' state

            
  //C:\Users\user\Desktop\Programs\myProg.txt 