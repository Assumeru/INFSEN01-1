module Game

type Object = {
    x: int;
    y: int;
}

type State = {
    map: string[];
    player: Object;
    running: bool;
}

let moveXY (dir, x, y, state) =
    let newX = state.player.x + x
    let newY = state.player.y + y
    if newY < 0 || newY >= state.map.Length || newX < 0 || newX >= state.map.[newY].Length then
        ("You cannot go that way", state)
    else
        let tile = state.map.[newY].[newX]
        match tile with
        | 'o' -> ("You moved " + dir, {state with player = {x = newX; y = newY;};})
        | _ -> ("Your path is blocked", state)

let move (dir, state) =
    match dir with
    | "north" -> moveXY(dir, 0, -1, state)
    | "east" -> moveXY(dir, 1, 0, state)
    | "south" -> moveXY(dir, 0, 1, state)
    | "west" -> moveXY(dir, -1, 0, state)

let parseCommand (x, state : State) =
    match x with
    | "stop" -> ("Bye", {state with running = false})
    | "north" | "east" | "south" | "west" -> move(x, state)
    | _ -> ("Unknown command", state)
