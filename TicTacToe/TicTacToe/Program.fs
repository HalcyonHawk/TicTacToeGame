module TicTacToe.Program

open System
open Elmish
open Elmish.WPF

//game design
module TicTacToeDesign =
    //players X and O
    type Player = PlayerX | PlayerO 

    //board positions
    //board made of horizontal and vertical components
    type HorizontalPosition = Left | CenterH | Right 
    type VerticalPosition = Top | CenterV | Bottom
    type SpacePosition = HorizontalPosition * VerticalPosition

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

    //TODO: Try seperate from model GameState
    let Spaces : Space array = [|
        {position = (Left, Top); state = Empty; image = "dash.png"}
        {position = (CenterH, Top); state = Empty; image = "dash.png"}
        {position = (Right, Top); state = Empty; image = "dash.png"}
        {position = (Left, CenterV); state = Empty; image = "dash.png"}
        {position = (CenterH, CenterV); state = Empty; image = "dash.png"}
        {position = (Right, CenterV); state = Empty; image = "dash.png"}
        {position = (Left, Bottom); state = Empty; image = "dash.png"}
        {position = (CenterH, Bottom); state = Empty; image = "dash.png"}
        {position = (Right, Bottom); state = Empty; image = "dash.png"}
        |]

    type Model =
        {   CurrentPlayer: Player 
            Shape: string
            Spaces: Space array}
    
    let init () =
        {   //StepSize = 1 
            CurrentPlayer = PlayerX
            Shape = "cross.jpg"
            Spaces = [|
            {position = (Left, Top); state = Empty; image = "dash.png"}
            {position = (CenterH, Top); state = Empty; image = "dash.png"}
            {position = (Right, Top); state = Empty; image = "dash.png"}
            {position = (Left, CenterV); state = Empty; image = "dash.png"}
            {position = (CenterH, CenterV); state = Empty; image = "dash.png"}
            {position = (Right, CenterV); state = Empty; image = "dash.png"}
            {position = (Left, Bottom); state = Empty; image = "dash.png"}
            {position = (CenterH, Bottom); state = Empty; image = "dash.png"}
            {position = (Right, Bottom); state = Empty; image = "dash.png"}
            |]
        }

    //player position
    //postion player puts a counter on their turn
    type PlayerXPosition = PlayerXPosition of SpacePosition
    type PlayerOPosition = PlayerOPosition of SpacePosition

    //vaild places player can put a counter on their turn
    //places to put a counter decided from making a list of the positions taken
    type PlayerXValidPlaces = PlayerXPosition list
    type PlayerOValidPlaces = PlayerOPosition list

    //turn 
    type TurnResult = 
        | PlayerXTurn of PlayerXValidPlaces
        | PlayerOTurn of PlayerOValidPlaces
        | WinGame of Player
    //When a player places their counter, this position is then taken.
    //GameState is updated to save all the taken positions by 
    //adding this position to the other taken 1s from previous turns
    //input -> output
    type PlayerXPlacesCounter<'GameState> = 'GameState * PlayerXPosition -> 'GameState * TurnResult
    type PlayerOPlacesCounter<'GameState> = 'GameState * PlayerOPosition -> 'GameState * TurnResult
    //New game made by resetting TurnResult and GameState
    type NewGame<'GameState> = 'GameState * TurnResult


//game logic 
module TicTacToeLogic =

    open TicTacToeDesign
    
    let updateElement key f st = 
        st |> Array.map (fun (k, v) -> if k = key then k, f v else k, v)

    let replace index sub = Array.mapi (fun i x -> if i = index then sub else x)

    
    
    let changePlayer m = 
        match m.CurrentPlayer with
        | PlayerX -> {m with CurrentPlayer = PlayerO}
        | PlayerO -> {m with CurrentPlayer = PlayerX}

    let doMove p m : Model = 
        match p with 
        | "0x0" -> 
        match m.Spaces.[0].state with
            | Empty -> {m with Spaces = Spaces |> replace 0 {position=(Left, Top); state=Taken m.CurrentPlayer; image = if m.CurrentPlayer = PlayerX then "cross.png" else "circle.jpg"}}
            | Taken _ -> m
        | "0x1" -> {m with Spaces = Spaces |> replace 1 {position=(CenterH, Top); state=Taken m.CurrentPlayer; image = if m.CurrentPlayer = PlayerX then "cross.png" else "circle.jpg"}}
        | _ -> changePlayer m 
        
      
    //if p = "0x0" then m  
    //    m with CurrentPlayer = PlayerO}

    //GameState used to keep track of the game
    //saves which board spaces are taken 
    //type GameState = {
    //    spaces : Space list
    //}

    //TODO: List horizontal positions 


    //TODO: List vertial positions 


    //TODO: Line of SpacePostions list


    //TODO: Check for 3 in a row
        //3 in a row vertial lines 
        //3 in a row horizontal lines 
        //diagonal 
        //other diagonal 

    //TODO: Check lines for win

    //TODO: Check for draw


//game user interface
//module TicTacToeInterface =

open TicTacToeDesign



//if CurrentPlayer = PlayerX then "cross.jpg" else "circle.jpg"


open TicTacToeLogic

type Msg =
    | ChangeShape of string
    //| Increment
    //| SetStepSize of int
    //| Reset 

let update msg m =
    match msg with
        //| ChangeShape -> {m with Shape = if m.CurrentPlayer = PlayerX then "cross.jpg" else "circle.jpg"}

        //ChangeShape message passes coordinates of pressed button as parameter p, turn is handled in doMove function
        //doMove takes then position and current model as paramenters
        | ChangeShape p -> doMove p m
        //| Increment -> { m with Count = m.Count + m.StepSize }
        //| SetStepSize x -> { m with StepSize = x }
        //| Reset -> init ()

open Elmish.WPF

let bindings model dispatch =
    [
        //"ChangeShape" |> Binding.cmd (fun m -> ChangeShape)
        
        //ChangeShape calls function with coordinate parameter declared in the XAML
        //Pipes position parameter from XAML into ChangeShape then doMove 
        "ChangeShape" |> Binding.paramCmd (fun p m -> string p |> ChangeShape)
        //TODO:TRY TWO WAY BINDINGS, LOOK UP XAML TUTORIAL ON IT
        "Shape0" |> /Binding.oneWay (fun m -> m.Spaces.[0].image)
        "Shape1" |> Binding.oneWay (fun m -> m.Spaces.[1].image)
        "Shape2" |> Binding.oneWay (fun m -> m.Spaces.[2].image)
        "Shape3" |> Binding.oneWay (fun m -> m.Spaces.[3].image)
        "Shape4" |> Binding.oneWay (fun m -> m.Spaces.[4].image)
        "Shape5" |> Binding.oneWay (fun m -> m.Spaces.[5].image)
        "Shape6" |> Binding.oneWay (fun m -> m.Spaces.[6].image)
        "Shape7" |> Binding.oneWay (fun m -> m.Spaces.[7].image)
        "Shape8" |> Binding.oneWay (fun m -> m.Spaces.[8].image)
        //"CounterValue" |> Binding.oneWay (fun m -> m.Count)
        //"Increment" |> Binding.cmd (fun m -> Increment)
        //"StepSize" |> Binding.twoWay
        //  (fun m -> float m.StepSize)
        //  (fun newVal m -> int newVal |> SetStepSize)
    ]


[<EntryPoint; STAThread>]
let main argv = 
    Program.mkSimple init update bindings
    |> Program.withConsoleTrace
    |> Program.runWindowWithConfig
        { ElmConfig.Default with LogConsole = true }
        (MainWindow())