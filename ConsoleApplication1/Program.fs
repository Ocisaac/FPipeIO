open FPipeIO.FPipeIO

let translateCode (source : string list) : Code = 
    let max = (source 
              |> (List.maxBy String.length))
              |> String.length 
              |> (+) 2
    let translateLine (ln : string) : CodeObject list = 
        Empty :: (List.map charToCodeObject (Array.toList <| ln.ToCharArray ())) @ [Empty]
    List.replicate max Empty 
    :: List.map translateLine source 
    @ [List.replicate max Empty]

let getInitState (code : Code) : State = 
    let isLit co = match co with 
                   | Lit l -> true
                   | _ -> false
    let mutable state = new State()
    for y = 0 to code.Length - 1 do
        for x = 0 to code.[y].Length - 1 do
            if isLit code.[y].[x] then
                state.Add((x,y), [Invocation])
    state
            
[<EntryPoint>]
let main argv = 
    printfn "Enter file location"
    let path = System.Console.ReadLine()
    printfn "Enter delay in miliseconds:"
    let delay = System.Int32.Parse(System.Console.ReadLine ())
    let lines : string list = System.IO.File.ReadAllLines(path)
                              |> Array.toList
    let code =  translateCode lines
    let mutable state = getInitState code
    while true do
        state <- runCode code state
        System.Threading.Thread.Sleep delay
    printfn ""
    0 // return an integer exit code

    //todo: make a settings type to pass with the code and state to run, such as
    //      whether to show the char I type in or not, do enters after printing and so on