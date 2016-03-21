module Game

type Object = {
    x: int;
    y: int;
}

type State = {
    map: string[];
    player: Object;
    running: bool;
    paused: bool;
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


let checkSurrounding (dir, x, y, state) =
    let x = state.player.x 
    let y = state.player.y
    if x < 0 || y >= state.map.Length || x < 0 || x >= state.map.[y].Length then
        ("You cannot go to " + dir, state)
    else
        "You can move to " + dir, state

let canMoveTo(dir, state) = 
    match dir with
    | "north" -> checkSurrounding(dir, 0, -1, state)
    | "east" -> checkSurrounding(dir, 1, 0, state)
    | "south" -> checkSurrounding(dir, 0, 1, state)
    | "west" -> checkSurrounding(dir, -1, 0, state)


let lookAround (state) = 
    let output = ""
    // Then go to each direction and give corresponding message
    let positionsList = ["north";"east";"south";"west"]
    for i in positionsList do
        canMoveTo(i, state)
        output = "You can move to" + i
   
    output, state


let parseCommand (x, state : State) =
    match x with
    | "stop" -> ("Bye", {state with running = false})
    | "north" | "east" | "south" | "west" -> move(x, state)
    | "lookaround" -> lookAround(state)
    | _ -> ("Unknown command", state)

let runFrame (state: State) =
    printfn "(%d, %d)" state.player.x state.player.y
    state


