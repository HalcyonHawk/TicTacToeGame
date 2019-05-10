﻿open System.Web.UI

//GameState used to keep track of the game
//saves which board spaces are taken 
//should be private as its used for game logic
type GameState = exn  // exn is a placeholder

//players X and O
type Player = PlayerX | PlayerO

//board positions
//board made of horizontal and vertical components
type HorizontalPosition = Left | Center | Right 
type VerticalPosition = Top | Center | Bottom
type SpacePosition = HorizontalPosition * VerticalPosition

//board spaces
//Spaces either empty or used 
//(used when an X or O is placed on them)
//space made of its position and state
type SpaceSate = Taken of Player | Empty

type Space = {
    position : SpacePosition 
    state : SpaceSate
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
type PlayerXPlacesCounter = GameState * PlayerXPosition -> GameState * TurnResult
type PlayerOPlacesCounter = GameState * PlayerOPosition -> GameState * TurnResult
//New game made by resetting TurnResult and GameState
type NewGame = GameState * TurnResult


