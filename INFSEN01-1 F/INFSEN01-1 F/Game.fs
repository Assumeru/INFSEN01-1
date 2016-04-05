module Game

type Direction =
    | north = 0
    | east = 1
    | south = 2
    | west = 3

type Object = {
    x: int
    y: int
    r: Direction
}

type Player = {
    obj: Object
    hp: int
    xp: int
    gp: int
    mp: int
    damage: int
}

type Monster = {
    obj: Object
    hp: int
    xp: int
    gp: int
    damage: int
    name: string
}

type State = {
    map: string[]
    player: Player
    running: bool
    paused: bool
    monsters: List<Monster> 
    monsterPrefixes: string[]
    monsterNames: string[]
}

let random = System.Random()

let getDirection(dir, state) =
    match dir with
    | "left" -> enum<Direction>((int state.player.obj.r + 3) % 4)
    | "right" -> enum<Direction>((int state.player.obj.r + 1) % 4)
    | "behind" -> enum<Direction>((int state.player.obj.r + 2) % 4)
    | _ -> state.player.obj.r

let createState(map, player, prefixes, names): State =
    {map = map; player = player; running = true; paused = false; monsters = []; monsterPrefixes = prefixes; monsterNames = names}

let createPlayer(x, y, d : Direction) : Player =
    {obj = {x = x; y = y; r = d}; hp = 10; xp = 0; gp = 0; mp = 10; damage = 10}

let generateRandomMonsterName(state) = 
    state.monsterPrefixes.[random.Next(state.monsterPrefixes.Length)] + " " + state.monsterNames.[random.Next(state.monsterNames.Length)]

let createMonster(xp, x, y, d, state) : Monster =
    let hp = random.Next(10) + xp / 10 + 1
    let xp = random.Next(10) + xp + 1
    let gp =  random.Next(xp + 10)
    let damage = random.Next(2) + xp / 100 + 1
    {obj = {x = x; y = y; r = d}; hp = hp ; xp = xp ; gp = gp; damage = damage; name = generateRandomMonsterName(state)}

let toString(dir) =
    match dir with
    | Direction.north -> "north"
    | Direction.east -> "east"
    | Direction.south -> "south"
    | Direction.west -> "west"

let actualMove(dir, state, newX, newY) = 
    ("You moved " + toString(dir), {state with player = {state.player with obj = {x = newX; y = newY; r = dir}}})

let tileExists(x, y, state) = 
    y >= 0 && y < state.map.Length && x >= 0 && x < state.map.[y].Length

let monsterEncounter(state, x, y) =
    let monster = createMonster(state.player.xp, x, y, getDirection("behind", state), state)
    monster.name + " appeared", {state with monsters = monster :: state.monsters}

let rec monsterAt(monsters, x, y) =
    match monsters with
    | a::b -> if(a.obj.x = x && a.obj.y = y) then
                    true
                  else
                    monsterAt(b, x, y)
    | [] -> false

let moveXY (dir, x, y, state) =
    let newX = state.player.obj.x + x
    let newY = state.player.obj.y + y
    if tileExists(newX, newY, state) then
        if(monsterAt(state.monsters, newX, newY)) then
            "A monster is blocking your path", state
        else 
            let tile = state.map.[newY].[newX]
            match tile with
            | 'o' -> actualMove(dir, state, newX, newY)
            | 'g' -> (if(random.NextDouble() < 0.5) then
                            monsterEncounter(state, newX, newY)
                        else
                            actualMove(dir, state, newX, newY)
                        )
            | _ -> "Your path is blocked", state
    else
        "You cannot go that way", state

let applyDir(dir, state, func) =
    match dir with
    | Direction.north -> func(dir, 0, -1, state)
    | Direction.east -> func(dir, 1, 0, state)
    | Direction.south -> func(dir, 0, 1, state)
    | Direction.west -> func(dir, -1, 0, state)

let move (dir, state) =
    applyDir(dir, state, moveXY)

let gazeAt(tile) =
    match tile with
    | 'o' -> "An empty corridor"
    | 'c' -> "A wall"
    | 'g' -> "An ominous passage"
    | _ -> "Something unknown"

let lookXY (dir, x, y, state) =
    let x = state.player.obj.x + x
    let y = state.player.obj.y + y
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

let parseCommand (x, state : State) =
    if(state.running) then
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
        | "turn left" -> "You turned left", {state with player = {state.player with obj = {state.player.obj with r = getDirection("left", state)}}}
        | "turn right" -> "You turned right", {state with player = {state.player with obj = {state.player.obj with r = getDirection("right", state)}}}
        | "turn around" -> "You turned around", {state with player = {state.player with obj = {state.player.obj with r = getDirection("behind", state)}}}
        | "walk" -> move(getDirection("ahead", state), state)
        | "fly" -> "People cannot fly", state
        | "commit suicide" -> "You have died..." , {state with running = false}
        | _ -> ("Unknown command", state)
    else
        "Game over", state

let monsterHitPlayer(state, monster) =
    printfn "%s hit you for %d" monster.name monster.damage
    let state = {state with player = {state.player with hp = state.player.hp - monster.damage}}
    if(state.player.hp <= 0) then
        printfn "You died"
        {state with running = false}
    else
        state

let rec removeFromList n list =
    match list with
    | h::t1 when h = n -> t1
    | h::t1 -> h :: (removeFromList n t1)
    | []    -> []

let attack(state, monster) =
    printfn "You hit %s for %d" monster.name state.player.damage
    let newMonster = {monster with hp = monster.hp - state.player.damage}
    if(newMonster.hp <= 0) then
        printfn "You killed %s" monster.name       
        {state with monsters = removeFromList monster state.monsters}
       else
        let monsters = removeFromList monster state.monsters
        {state with monsters = newMonster :: monsters}

let objectNextTo(a, b) =
    abs(a.x - b.x) + abs(a.y - b.y) < 2

let rec hitPlayer(state, monsters) =
    match monsters with
    | a::b -> if(objectNextTo(state.player.obj, a.obj)) then
                    hitPlayer(monsterHitPlayer(state, a), b)
                else
                    hitPlayer(state, b)
    | [] -> state

let runFrame (state: State) =
    let state = hitPlayer(state, state.monsters)
    state
