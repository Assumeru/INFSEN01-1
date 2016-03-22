﻿module Game

type Direction =
    | north = 0
    | east = 1
    | south = 2
    | west = 3

type Object = {
    x: int;
    y: int;
    r: Direction
}

type State = {
    map: string[];
    player: Object;
    running: bool;
    paused: bool;
}

let toString(dir) =
    match dir with
    | Direction.north -> "north"
    | Direction.east -> "east"
    | Direction.south -> "south"
    | Direction.west -> "west"

let tileExists(x, y, state) = 
    y >= 0 && y < state.map.Length && x >= 0 && x < state.map.[y].Length

let moveXY (dir, x, y, state) =
    let newX = state.player.x + x
    let newY = state.player.y + y
    if tileExists(newX, newY, state) then
        let tile = state.map.[newY].[newX]
        match tile with
        | 'o' -> ("You moved " + toString(dir), {state with player = {state.player with x = newX; y = newY; r = dir};})
        | _ -> ("Your path is blocked", state)
    else
        ("You cannot go that way", state)

let applyDir(dir, state, func) =
    match dir with
    | Direction.north -> func(dir, 0, -1, state)
    | Direction.east -> func(dir, 1, 0, state)
    | Direction.south -> func(dir, 0, 1, state)
    | Direction.west -> func(dir, -1, 0, state)

let move (dir, state) =
    applyDir(dir, state, moveXY)

let checkSurrounding (dir, x, y, state) =
    let x = state.player.x 
    let y = state.player.y
    if x < 0 || y >= state.map.Length || x < 0 || x >= state.map.[y].Length then
        ("You cannot go to " + dir, state)
    else
        "You can move to " + dir, state

let gazeAt(tile) =
    match tile with
    | 'o' -> "An empty corridor"
    | 'c' -> "A wall"
    | _ -> "Something unknown"

let lookXY (dir, x, y, state) =
    let x = state.player.x + x
    let y = state.player.y + y
    if tileExists(x, y, state) then
        gazeAt(state.map.[y].[x])
    else
        "The end of the world"

let look(dir, state) =
    applyDir(dir, state, lookXY)

let rec lookList(dirs, state) : string =
    match dirs with
    | [] -> ""
    | x :: y -> "To the " + toString x + ": " + look(x, state) + "\n" + lookList(y, state)

let lookAround (state) = 
    let positionsList = [Direction.north;Direction.east;Direction.south;Direction.west]
    lookList(positionsList, state)

let getDirection(dir, state) =
    match dir with
    | "left" -> enum<Direction>((int state.player.r + 3) % 4)
    | "right" -> enum<Direction>((int state.player.r + 1) % 4)
    | "behind" -> enum<Direction>((int state.player.r + 2) % 4)
    | _ -> state.player.r

let parseCommand (x, state : State) =
    match x with
    | "stop" -> ("Bye", {state with running = false})
    | "north" -> move(Direction.north, state)
    | "east" -> move(Direction.east, state)
    | "south" -> move(Direction.south, state)
    | "west" -> move(Direction.west, state)
    | "look around" -> lookAround(state), state
    | "look left" -> look(getDirection("left", state), state), state
    | "look right" -> look(getDirection("right", state), state), state
    | "look behind" -> look(getDirection("behind", state), state), state
    | "look ahead" -> look(getDirection("ahead", state), state), state
    | "turn left" -> "You turned left", {state with player = {state.player with r = getDirection("left", state)}}
    | "turn right" -> "You turned right", {state with player = {state.player with r = getDirection("right", state)}}
    | "turn around" -> "You turned around", {state with player = {state.player with r = getDirection("behind", state)}}
    | _ -> ("Unknown command", state)

let runFrame (state: State) =
    printfn "(%d, %d)" state.player.x state.player.y
    state
