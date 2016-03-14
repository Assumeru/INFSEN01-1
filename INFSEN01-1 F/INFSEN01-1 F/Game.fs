﻿module Game

let squash (a, (b, c)) = (a, b, c)

type Object = {
    x: int;
    y: int;
}

type State = {
    map: string[];
    player: Object;
}

let moveXY (x, y, state) =
    let newX = state.player.x + x
    let newY = state.player.y + y
    if newY < 0 || newY >= state.map.Length || newX < 0 || newX >= state.map.[newY].Length then
        ("You cannot go that way", state)
    else
        ("You moved... maybe", {state with player = {x = newX; y = newY;};})

let move (dir, state) =
    match dir with
    | "north" -> squash(true, moveXY(0, -1, state))
    | "east" -> squash(true, moveXY(1, 0, state))
    | "south" -> squash(true, moveXY(0, 1, state))
    | "west" -> squash(true, moveXY(-1, 0, state))

let parseCommand (x, state : State) =
    match x with
    | "stop" -> (false, "Bye", state)
    | "north" | "east" | "south" | "west" -> move(x, state)
    | _ -> (true, "Unknown command", state)
