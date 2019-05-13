module TicTacToe.Program
open FSharpx.Collections.PersistentVector
open System
open Elmish
open Elmish.WPF
open FSharpx.Collections

////game design
//module TicTacToeDesign =
//players X and O
type Player = PlayerX | PlayerO | None

//board positions
//board made of horizontal and vertical components
type HorizontalPosition = Left | CenterH | Right 
type VerticalPosition = Top | CenterV | Bottom
type SpacePosition = HorizontalPosition * VerticalPosition

type Line = Line of SpacePosition list
//board spaces
//Spaces either empty or used 
//(used when an X or O is placed on them)
//space made of its position and state
type SpaceSate = Taken of Player | Empty

type Space = {
    position : SpacePosition 
    state : SpaceSate
    image : string
}
//GameState used to keep track of the game
//saves which board spaces are taken 
//type GameState = {
//    spaces : Space list
//}

//TODO: Try seperate from model GameState
let Spaces = 
    empty
    |> conj {position = (Left, Top); state = Empty; image = "dash.png"}
    |> conj {position = (CenterH, Top); state = Empty; image = "dash.png"}
    |> conj {position = (Right, Top); state = Empty; image = "dash.png"}
    |> conj {position = (Left, CenterV); state = Empty; image = "dash.png"}
    |> conj {position = (CenterH, CenterV); state = Empty; image = "dash.png"}
    |> conj {position = (Right, CenterV); state = Empty; image = "dash.png"}
    |> conj {position = (Left, Bottom); state = Empty; image = "dash.png"}
    |> conj {position = (CenterH, Bottom); state = Empty; image = "dash.png"}
    |> conj {position = (Right, Bottom); state = Empty; image = "dash.png"}
    
    
    

type Model =
    {   CurrentPlayer: Player 
        Shape: string
        Spaces: PersistentVector<Space>
        Won: Player 
        PlayerXScore: int
        PlayerOScore: int
        PvAI: bool
    }
    
let init () =
    {   
        CurrentPlayer = PlayerX
        Shape = "cross.jpg"
        Spaces = Spaces
        Won = None
        PlayerXScore = 0
        PlayerOScore = 0
        PvAI = false
    }



////game logic 
//module TicTacToeLogic =

    
/// a list of eight lines to check for 3 in a row
let allPossibleLines m : Space list list = 
        [[m.Spaces.[0]; m.Spaces.[1]; m.Spaces.[2]]; 
        [m.Spaces.[3]; m.Spaces.[4]; m.Spaces.[5]];
        [m.Spaces.[6]; m.Spaces.[7]; m.Spaces.[8]];
        [m.Spaces.[0]; m.Spaces.[3]; m.Spaces.[6]];
        [m.Spaces.[1]; m.Spaces.[4]; m.Spaces.[7]];
        [m.Spaces.[2]; m.Spaces.[5]; m.Spaces.[8]];
        [m.Spaces.[0]; m.Spaces.[4]; m.Spaces.[8]];
        [m.Spaces.[2]; m.Spaces.[4]; m.Spaces.[6]]]
   
//Questions
//How do I check if the player has won/drawn
//How do I branch away from the game loop when someone has won instead of waiting for another move
//How do I let the player chose playing vs AI or a person and how do I change the behaviour of the game accordingly
//How do messages work and can I use them to run code
let checkGameWon m = 
    let record = m
    for line in allPossibleLines m do 
        match line.[0].state, line.[1].state, line.[2].state with
            | Taken PlayerX, Taken PlayerX, Taken PlayerX -> (fun _ -> record.PlayerXScore = record.PlayerXScore + 1)
            | Taken PlayerO, Taken PlayerO, Taken PlayerO -> (fun _ -> record.PlayerOScore = record.PlayerOScore + 1)
            | _ -> (fun _ -> false)
        //match line.[0].state, line.[1].state, line.[2].state with
        //    | Taken PlayerX, Taken PlayerX, Taken PlayerX -> {m with Won = PlayerX}
        //    | Taken PlayerO, Taken PlayerO, Taken PlayerO -> {m with Won = PlayerO}
        //    | Empty, _, _ -> {m with Won = None}
        //    | _, Empty, _ -> {m with Won = None}
        //    | _, _, Empty -> {m with Won = None}
        //m.Won = if line.[0].state = Taken player && line.[1].state = Taken player && line.[2].state = Taken player then true else false
    record

let changePlayer m = 
    match m.CurrentPlayer with
    | PlayerX -> {m with CurrentPlayer = PlayerO}
    | PlayerO -> {m with CurrentPlayer = PlayerX}


let doMove p m = 
    match p with 
    | "0x0" -> 
    match m.Spaces.[0].state with
        | Empty -> 
            let record = m.Spaces
            let x ={m with 
                        Spaces = 
                            record |> update 0 {position=(Left, Top); 
                            state=Taken m.CurrentPlayer; 
                            image = if m.CurrentPlayer = PlayerX 
                                    then "cross.png" 
                                    else "circle.jpg"}} |> checkGameWon |> changePlayer 
            x
        | Taken _ -> m
    | "0x1" -> match m.Spaces.[1].state with
                | Empty -> 
                    let record = m.Spaces
                    let x ={m with 
                                Spaces = 
                                    record 
                                    |> update 1 
                                        {position=(CenterH, Top); 
                                            state=Taken m.CurrentPlayer; 
                                            image = if m.CurrentPlayer = PlayerX 
                                                    then "cross.png" 
                                                    else "circle.jpg"}} |> checkGameWon |> changePlayer 
                    x
                | Taken _ -> m
    | "0x2" -> match m.Spaces.[2].state with
                | Empty -> 
                    let record = m.Spaces
                    let x ={m with 
                                Spaces = 
                                    record 
                                    |> update 2 
                                        {position=(CenterH, Top); 
                                            state=Taken m.CurrentPlayer; 
                                            image = if m.CurrentPlayer = PlayerX 
                                                    then "cross.png" 
                                                    else "circle.jpg"}} |> checkGameWon |> changePlayer 
                    x
                | Taken _ -> m
    | "1x0" -> match m.Spaces.[3].state with
                | Empty -> 
                    let record = m.Spaces
                    let x ={m with 
                                Spaces = 
                                    record 
                                    |> update 3
                                        {position=(CenterH, Top); 
                                            state=Taken m.CurrentPlayer; 
                                            image = if m.CurrentPlayer = PlayerX 
                                                    then "cross.png" 
                                                    else "circle.jpg"}} |> changePlayer 
                    x
                | Taken _ -> m
    | "1x1" -> match m.Spaces.[4].state with
                | Empty -> 
                    let record = m.Spaces
                    let x ={m with 
                                Spaces = 
                                    record 
                                    |> update 4 
                                        {position=(CenterH, Top); 
                                            state=Taken m.CurrentPlayer; 
                                            image = if m.CurrentPlayer = PlayerX 
                                                    then "cross.png" 
                                                    else "circle.jpg"}} |> changePlayer 
                    x
                | Taken _ -> m
    | "1x2" -> match m.Spaces.[5].state with
                | Empty -> 
                    let record = m.Spaces
                    let x ={m with 
                                Spaces = 
                                    record 
                                    |> update 5
                                        {position=(CenterH, Top); 
                                            state=Taken m.CurrentPlayer; 
                                            image = if m.CurrentPlayer = PlayerX 
                                                    then "cross.png" 
                                                    else "circle.jpg"}} |> changePlayer 
                    x
                | Taken _ -> m
    | "2x0" -> match m.Spaces.[6].state with
                | Empty -> 
                    let record = m.Spaces
                    let x ={m with 
                                Spaces = 
                                    record 
                                    |> update 6
                                        {position=(CenterH, Top); 
                                            state=Taken m.CurrentPlayer; 
                                            image = if m.CurrentPlayer = PlayerX 
                                                    then "cross.png" 
                                                    else "circle.jpg"}} |> changePlayer 
                    x
                | Taken _ -> m
    | "2x1" -> match m.Spaces.[7].state with
                | Empty -> 
                    let record = m.Spaces
                    let x ={m with 
                                Spaces = 
                                    record 
                                    |> update 7
                                        {position=(CenterH, Top); 
                                            state=Taken m.CurrentPlayer; 
                                            image = if m.CurrentPlayer = PlayerX 
                                                    then "cross.png" 
                                                    else "circle.jpg"}} |> changePlayer 
                    x
                | Taken _ -> m
    | "2x2" -> match m.Spaces.[8].state with
                | Empty -> 
                    let record = m.Spaces
                    let x ={m with 
                                Spaces = 
                                    record 
                                    |> update 8
                                        {position=(CenterH, Top); 
                                            state=Taken m.CurrentPlayer; 
                                            image = if m.CurrentPlayer = PlayerX 
                                                    then "cross.png" 
                                                    else "circle.jpg"}} |> changePlayer 
                    x
                | Taken _ -> m
    | _ -> changePlayer m 
        

//TODO: Check for draw

//open TicTacToeDesign

//open TicTacToeLogic

type Msg =
    | ChangeShape of string
    | Reset 

let update msg m =
    match msg with
        //ChangeShape message passes coordinates of pressed button as parameter p, turn is handled in doMove function
        //doMove takes then position and current model as paramenters
        | ChangeShape p -> doMove p m
        | Reset -> init()

open Elmish.WPF

let bindings model dispatch =
    [
        
        //ChangeShape calls function with coordinate parameter declared in the XAML
        //Pipes position parameter from XAML into ChangeShape then doMove 
        "ChangeShape" |> Binding.paramCmd (fun p m -> string p |> ChangeShape)
        "Shape0" |> Binding.oneWay (fun m -> m.Spaces.[0].image)
        "Shape1" |> Binding.oneWay (fun m -> m.Spaces.[1].image)
        "Shape2" |> Binding.oneWay (fun m -> m.Spaces.[2].image)
        "Shape3" |> Binding.oneWay (fun m -> m.Spaces.[3].image)
        "Shape4" |> Binding.oneWay (fun m -> m.Spaces.[4].image)
        "Shape5" |> Binding.oneWay (fun m -> m.Spaces.[5].image)
        "Shape6" |> Binding.oneWay (fun m -> m.Spaces.[6].image)
        "Shape7" |> Binding.oneWay (fun m -> m.Spaces.[7].image)
        "Shape8" |> Binding.oneWay (fun m -> m.Spaces.[8].image)
        "CurrentPlayer" |> Binding.oneWay (fun m -> if m.CurrentPlayer = PlayerX then "cross.png" else "circle.jpg")
        "PlayerXScore" |> Binding.oneWay (fun m -> m.PlayerXScore)
        "PlayerOScore" |> Binding.oneWay (fun m -> m.PlayerOScore)
        "PvAI" |> Binding.oneWay (fun m -> m.PvAI)
        "Reset" |> Binding.cmd (fun _ -> Reset)
    ]


[<EntryPoint; STAThread>]
let main argv = 
    Program.mkSimple init update bindings
    |> Program.withConsoleTrace
    |> Program.runWindowWithConfig
        { ElmConfig.Default with LogConsole = true }
        (MainWindow())