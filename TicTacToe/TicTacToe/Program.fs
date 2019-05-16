module TicTacToe.Program
open FSharpx.Collections.PersistentVector
open System
open System.Windows
open Elmish
open Elmish.WPF
open FSharpx.Collections
open TicTacToe.Views

////game design
//module TicTacToeDesign =
//players X and O
type Player = PlayerX | PlayerO 

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

module PvPWin = 
    type Space = {
        position : SpacePosition 
        state : SpaceSate
        image : string
    }

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
            Spaces: PersistentVector<Space>
            PlayerXScore: int
            PlayerOScore: int
            Winner: String
        }
    
    let init () =
        {   
            CurrentPlayer = PlayerX
            Spaces = Spaces
            PlayerXScore = 0
            PlayerOScore = 0
            Winner = ""
        }

    let restoreInit (xScore, oScore, winner) =
        {   
            CurrentPlayer = PlayerX
            Spaces = Spaces
            PlayerXScore = xScore
            PlayerOScore = oScore
            Winner = winner
        }

    type Msg =
        | ChangeShape of string
        | Reset 
    
    
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
   
    let isRowTaken (list: Space list list) =
        let matchedRow = List.tryFind (fun (elem: Space list) ->
            match elem.[0].state, elem.[1].state, elem.[2].state with
                | Taken PlayerX, Taken PlayerX, Taken PlayerX -> true
                | Taken PlayerO, Taken PlayerO, Taken PlayerO -> true
                | _ -> false) list

        let winningRow = if (Option.isSome matchedRow) then Option.get matchedRow else List.empty<Space>
        if List.length winningRow <> 0 then winningRow.Head.state else Empty


    let checkGameWon m = 
        let record = m
        match isRowTaken (allPossibleLines m) with 
            | Taken PlayerX -> restoreInit (m.PlayerXScore + 1, m.PlayerOScore, "X Wins")
            | Taken PlayerO -> restoreInit (m.PlayerXScore, m.PlayerOScore + 1, "O Wins")
            | Empty -> record
    
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
                                                        else "circle.jpg"}} |> checkGameWon |> changePlayer 
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
                                                        else "circle.jpg"}} |> checkGameWon |> changePlayer 
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
                                                        else "circle.jpg"}} |> checkGameWon |> changePlayer 
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
                                                        else "circle.jpg"}} |> checkGameWon |> changePlayer 
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
                                                        else "circle.jpg"}} |> checkGameWon |> changePlayer 
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
                                                        else "circle.jpg"}} |> checkGameWon |> changePlayer 
                        x
                    | Taken _ -> m
        | _ -> changePlayer m 


    let update msg m =
        match msg with
            //ChangeShape message passes coordinates of pressed button as parameter p, turn is handled in doMove function
            //doMove takes then position and current model as paramenters
            | ChangeShape p -> doMove p m
            | Reset -> init ()


    let bindings () =
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
            "Winner" |> Binding.oneWay (fun m -> m.Winner)
            "Reset" |> Binding.cmd (fun _ -> Reset)
        
        ]

module PvAIWin = 
    type Space = {
        position : SpacePosition 
        state : SpaceSate
        image : string
    }

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
            Spaces: PersistentVector<Space>
            PlayerXScore: int
            PlayerOScore: int
            Winner: String
        }

    let init () =
        {   
            CurrentPlayer = PlayerX
            Spaces = Spaces
            PlayerXScore = 0
            PlayerOScore = 0
            Winner = ""
        }

    let restoreInit (xScore, oScore, winner) =
        {   
            CurrentPlayer = PlayerX
            Spaces = Spaces
            PlayerXScore = xScore
            PlayerOScore = oScore
            Winner = winner
        }

    type Msg =
        | ChangeShape of string
        | AIMove
        | Reset 
    
    
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
   
    //List of all spaces in the passed model
    let allSpaces m = [m.Spaces.[0]; 
                      m.Spaces.[1];
                      m.Spaces.[2]; 
                      m.Spaces.[3]; 
                      m.Spaces.[4]; 
                      m.Spaces.[5]; 
                      m.Spaces.[6]; 
                      m.Spaces.[7]; 
                      m.Spaces.[8]]

    let isRowTaken (list: Space list list) =
        let matchedRow = List.tryFind (fun (elem: Space list) ->
            match elem.[0].state, elem.[1].state, elem.[2].state with
                | Taken PlayerX, Taken PlayerX, Taken PlayerX -> true
                | Taken PlayerO, Taken PlayerO, Taken PlayerO -> true
                | _ -> false) list

        let winningRow = if (Option.isSome matchedRow) then Option.get matchedRow else List.empty<Space>
        if List.length winningRow <> 0 then winningRow.Head.state else Empty


    let checkGameWon m = 
        let record = m
        match isRowTaken (allPossibleLines m) with 
            | Taken PlayerX -> restoreInit (m.PlayerXScore + 1, m.PlayerOScore, "X Wins")
            | Taken PlayerO -> restoreInit (m.PlayerXScore, m.PlayerOScore + 1, "O Wins")
            | Empty -> record
  
    

    let rand = new System.Random()

    let randSpace m = 
        let spaceCoordinates = ["0x0", m.Spaces.[0];
            "0x1", m.Spaces.[1];
            "0x2", m.Spaces.[2]; 
            "1x0", m.Spaces.[3]; 
            "1x1", m.Spaces.[4];
            "1x2", m.Spaces.[5]; 
            "2x0", m.Spaces.[6]; 
            "2x1", m.Spaces.[7]; 
            "2x2", m.Spaces.[8]] |> Map.ofList
        let emptySpaces = spaceCoordinates |> Map.filter (fun key value -> value.state = Empty) 
        let emptySpaceList = Map.toList emptySpaces
        fst emptySpaceList.[rand.Next(emptySpaceList.Length)]

    let changePlayer m = 
        match m.CurrentPlayer with
        | PlayerX -> {m with CurrentPlayer = PlayerO}
        | PlayerO -> {m with CurrentPlayer = PlayerX}


    let doMove p m= 
        match p with 
        | "0x0" -> match m.Spaces.[0].state with
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
                                                        else "circle.jpg"}} |> checkGameWon |> changePlayer 
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
                                                        else "circle.jpg"}} |> checkGameWon |> changePlayer 
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
                                                        else "circle.jpg"}} |> checkGameWon |> changePlayer 
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
                                                        else "circle.jpg"}} |> checkGameWon |> changePlayer 
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
                                                        else "circle.jpg"}} |> checkGameWon |> changePlayer 
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
                                                        else "circle.jpg"}} |> checkGameWon |> changePlayer 
                        x
                    | Taken _ -> m
        | _ -> changePlayer m 
    
   

    let update msg m =
        match msg with
            //ChangeShape message passes coordinates of pressed button as parameter p, turn is handled in doMove function
            //doMove takes then position and current model as paramenters
            | ChangeShape p -> if m.CurrentPlayer = PlayerX then doMove p m else m
            | AIMove -> if (allSpaces m |> List.filter (fun x -> x.state = Taken PlayerX || x.state = Taken PlayerO)) |> List.length <> 9 && m.CurrentPlayer = PlayerO then doMove (randSpace m) m else m
            | Reset -> init ()

    //Removed parameters, added ()
    let bindings () =
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
            "Reset" |> Binding.cmd (fun _ -> Reset)
            "AIMove" |> Binding.cmd(fun _ -> AIMove)
        
        ]
module LeaderboardWin =
    type Model = 
        {  
            player: Player
        }

    let init () =
        {
            player = PlayerX
        }

    type Msg =
        | Message

    let update msg m =
        match msg with
            | Message -> init ()

    let bindings () =
        [ 
            "Message" |> Binding.cmd (fun m -> Message )
        ]

module App =
    type Model = 
        {  
            PvPWin : PvPWin.Model 
            PvAIWin: PvAIWin.Model
            LeaderboardWin: LeaderboardWin.Model
        }

    let init () =
        {   PvPWin = PvPWin.init ()
            PvAIWin = PvAIWin.init () 
            LeaderboardWin = LeaderboardWin.init ()}, Cmd.none

    type Msg =
        | ShowWin1
        | ShowWin2
        | ShowLeaderboard
        | PvPMsg of PvPWin.Msg
        | PvAIMsg of PvAIWin.Msg
        | LeaderboardMsg of LeaderboardWin.Msg

    let showPvpWin () =
        Application.Current.Dispatcher.Invoke(fun () ->
            let pvpWin = MainWindow()
            pvpWin.DataContext <- Application.Current.MainWindow.DataContext
            pvpWin.Show())

    let showPvaiWin () =
        Application.Current.Dispatcher.Invoke(fun () ->
            let pvaiWin = AIGameWin()
            pvaiWin.DataContext <- Application.Current.MainWindow.DataContext
            pvaiWin.Show())

    let showLeaderboard () =
        Application.Current.Dispatcher.Invoke(fun () ->
            let leaderboardWin = LeaderboardWindow()
            leaderboardWin.DataContext <- Application.Current.MainWindow.DataContext
            leaderboardWin.Show())

    let update msg m =  
        match msg with
            | ShowWin1 -> m, Cmd.OfFunc.attempt showPvpWin () raise
            | ShowWin2 -> m, Cmd.OfFunc.attempt showPvaiWin () raise
            | ShowLeaderboard -> m, Cmd.OfFunc.attempt showLeaderboard () raise
            | PvPMsg msg' -> { m with PvPWin = PvPWin.update msg' m.PvPWin }, Cmd.none
            | PvAIMsg msg' -> { m with PvAIWin = PvAIWin.update msg' m.PvAIWin }, Cmd.none
            | LeaderboardMsg msg' -> { m with LeaderboardWin = LeaderboardWin.update msg' m.LeaderboardWin }, Cmd.none

    let bindings model dispatch =
        [ 
            "PvP" |> Binding.cmd (fun m -> ShowWin1)
            "PvAI" |> Binding.cmd (fun m -> ShowWin2)
            "Leaderboard" |> Binding.cmd (fun m -> ShowLeaderboard)
            "MainWindow" |> Binding.subModel (fun m -> m.PvPWin) PvPWin.bindings PvPMsg
            "AIGameWin" |> Binding.subModel (fun m -> m.PvAIWin) PvAIWin.bindings PvAIMsg
            "LeaderboardWin" |> Binding.subModel (fun m -> m.LeaderboardWin) LeaderboardWin.bindings LeaderboardMsg
        ]
    
[<EntryPoint; STAThread>]
let main argv =
  Program.mkProgram App.init App.update App.bindings
  |> Program.withConsoleTrace
  |> Program.runWindowWithConfig
      { ElmConfig.Default with LogConsole = true }
      (MainMenu())

