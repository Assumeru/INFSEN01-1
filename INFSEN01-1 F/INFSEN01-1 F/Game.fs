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
    inv: Map<string, int>
    lvl: int
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
    map: char[][]
    player: Player
    running: bool
    paused: bool
    monsters: List<Monster> 
    monsterPrefixes: string[]
    monsterNames: string[]
    commands: string[]
}

let random = System.Random()

let addXP(player, xp) =
    let player = {player with Player.xp = player.xp + xp}
    let threshold = player.lvl * player.lvl * 10 + 10;
    if(player.xp >= threshold) then
        let player = {player with xp = player.xp - threshold; lvl = player.lvl + 1; damage = player.damage + 1; hp = player.hp}
        printfn "You advanced to level %d" player.lvl
        player
    else
        player

let applyRandomEffect(state) =
    printfn("You eat the easter egg and wait for something to happen...")
    let randomEffect = [|"health boost"; "mana boost"; "exp boost"; "exp boost"; "exp boost"; "exp boost"; "damage"|]
    let effect = randomEffect.[random.Next(randomEffect.Length)]
    match effect with
    | "health boost" -> "You feel a glowing sensation, it grants you 1 hp!", {state with player={state.player with hp=state.player.hp + 1}}
    | "mana boost" -> "You feel a glowing sensation, it gives you 1 mp!", {state with player={state.player with mp=state.player.mp + 1}}
    | "exp boost" -> "You feel more experienced in the battlefield! +1 xp", {state with player=addXP(state.player, 1)}
    | "damage" -> "You don't feel so good, you took 1 damage", {state with player={state.player with xp=state.player.xp + 1}}
    | _-> "Que?", state

let showInventory(state) =
    printfn "Your inventory contains the following:"
    for KeyValue(k, v) in state.player.inv do
        // 'k' is the key, 'v' is the value
        if(v > 0) then printfn "%i %s" v k
    {state with player={state.player with inv=state.player.inv.Add("easter egg", state.player.inv.["easter egg"] + 1)}}

let examineItem(name, state) =
    if(state.player.inv.[name] > 0) then 
        match name with
        | "health potion" -> "You examine the health potion, the liquid seems to be glowing", state
        | "bread" -> "You examine the loaf of bread, it looks tasty!", state
        | "emerald" -> "You examine the emerald, it looks like it would improve your weapon!", state
        | "mana potion" -> "You examine the mana potion, the liquid seems to be glowing", state
        | "easter egg" -> "You examine the easter egg, how did I acquire this again?", state
        | _ -> "You can't examine that...", state
    else "You don't have this item in your inventory", state

let itemAction(name, state) = 
    match name with
    | "health potion" -> "You drink the health potion, granting 5 hp", {state with player={state.player with hp=state.player.hp + 5; inv=state.player.inv.Add(name, state.player.inv.[name]-1)}}
    | "bread" -> "You eat the loaf of bread, granting 3 hp", {state with player={state.player with hp=state.player.hp + 3; inv=state.player.inv.Add(name, state.player.inv.[name]-1)}}
    | "emerald" -> "You apply the emerald to your weapon, raising its damage by 1", {state with player={state.player with damage=state.player.damage + 1; inv=state.player.inv.Add(name, state.player.inv.[name]-1)}}
    | "mana potion" -> "You drink the mana potion, restoring 5 mp", {state with player={state.player with mp=state.player.mp + 5; inv=state.player.inv.Add(name, state.player.inv.[name]-1)}}
    | "easter egg" -> applyRandomEffect({state with player={state.player with inv=state.player.inv.Add(name, state.player.inv.[name]-1)}})
    | _ -> "You can't use that...", state

let useItem(name, state) =
    if(state.player.inv.[name] > 0) then
        itemAction(name, state)
    else
        "You don't have this item in your inventory", state

let rec getTotalItems(state) : int =
    Map.fold (fun s key value -> s + value) 0 state.player.inv

let showStats(state) = 
    printfn "Your stats: "
    printfn "Level: %i" state.player.lvl    
    printfn "HP: %i" state.player.hp    
    printfn "MP: %i" state.player.mp   
    printfn "Gold: %i" state.player.gp   
    printfn "XP: %i" state.player.xp   
    printfn "Damage: %i" state.player.damage  
    printfn "You have %d items in your inventory" (getTotalItems(state))
    state

let getDirection(dir, state) =
    match dir with
    | "left" -> enum<Direction>((int state.player.obj.r + 3) % 4)
    | "right" -> enum<Direction>((int state.player.obj.r + 1) % 4)
    | "behind" -> enum<Direction>((int state.player.obj.r + 2) % 4)
    | _ -> state.player.obj.r

let createState(map, player, prefixes, names): State =
    {map = map; player = player; running = true; paused = false; monsters = []; monsterPrefixes = prefixes; monsterNames = names; commands = [|""|]}

let createPlayer(x, y, d : Direction) : Player =
    {obj = {x = x; y = y; r = d}; hp = 10; xp = 0; gp = 0; mp = 10; damage = 10; lvl = 0; inv = ["health potion", 2; "bread", 1;
    "emerald", 1; "mana potion", 1; "easter egg", 0] |> Map.ofList}; 

let generateRandomMonsterName(state) = 
     state.monsterPrefixes.[random.Next(state.monsterPrefixes.Length)] + " " + state.monsterNames.[random.Next(state.monsterNames.Length)]

let createMonster(lvl, x, y, d, state) : Monster =
    let hp = random.Next(10) + lvl * 3 + 10
    let xp = random.Next(10) + lvl * 10 + 1
    let gp = random.Next(lvl + 10)
    let damage = random.Next(2) + lvl + 1
    {obj = {x = x; y = y; r = d}; hp = hp ; xp = xp; gp = gp; damage = damage; name = generateRandomMonsterName(state)}


let toString(dir) =
    match dir with
    | Direction.north -> "north"
    | Direction.east -> "east"
    | Direction.south -> "south"
    | Direction.west -> "west"

let rec removeFromList n list =
    match list with
    | h::t1 when h = n -> t1
    | h::t1 -> h :: (removeFromList n t1)
    | []    -> []

let actualMove(dir, state, newX, newY) = 
    ("You moved " + toString(dir), {state with player = {state.player with obj = {x = newX; y = newY; r = dir}}})

let tileExists(x, y, state) = 
    y >= 0 && y < state.map.Length && x >= 0 && x < state.map.[y].Length

let monsterEncounter(state, x, y) =
    let monster = createMonster(state.player.lvl, x, y, getDirection("behind", state), state)
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
            | 'o' | 'l' -> actualMove(dir, state, newX, newY)
            | 'g' -> (if(random.NextDouble() < 0.5) then
                            let state = {state with player = {state.player with obj = {state.player.obj with r = dir}}}
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
    | 'l' -> "A sparkling corridor"
    | 'w' -> "A crumbled wall"
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

let helpCommandsNormalState(state) = 
    [|
    "the following commands are available:";
    "=========";
    "stop";
    "north";
    "east";
    "south";
    "west";
    "look around";
    "look left";
    "look right";
    "look behind";
    "look ahead";
    "turn left";
    "turn right";
    "turn around";
    "walk";
    "fly";
    "commit suicide";
    "help";
    "=========";
    |]

let loot(state) =
    let randomLoot = [|"health potion"; "bread"; "emerald"; "mana potion"; "bread"; "bread"; "bread"; "mana potion"; "health potion"|]
    match state.map.[state.player.obj.y].[state.player.obj.x] with
    | 'l' -> if(random.NextDouble() < 0.1) then
                 let item = randomLoot.[random.Next(randomLoot.Length)]
                 "You looted " + item, {state with player = {state.player with inv = state.player.inv.Add(item, state.player.inv.[item] + 1)}}
             else
                "You did not find any loot", state
    | _ -> "There is nothing to loot", state
    
let lootMonster(state,monster) =
    let randomLoot = [|"health potion"; "bread"; "emerald"; "mana potion"; "bread"; "bread"; "bread"; "mana potion"; "health potion"|]
    let goldAmount = monster.gp
    if(random.NextDouble() < 0.9) then
        let item = randomLoot.[random.Next(randomLoot.Length)]
        printf "You looted %d gp and %s \n" goldAmount item ;
        {state with player = {state.player with Player.gp= state.player.gp + goldAmount; Player.inv = state.player.inv.Add(item, state.player.inv.[item] + 1)}}
        
    else
        printf "You looted %d gp \n" goldAmount;
        {state with player = {state.player with Player.gp= state.player.gp + goldAmount}}

let damageMonster(state, monster, damage) =
    let newMonster = {monster with Monster.hp = monster.hp - damage}
    if(newMonster.hp <= 0) then
        let newState = lootMonster(state,newMonster)
        "You killed " + monster.name ,{newState with monsters = removeFromList monster newState.monsters; player = addXP(newState.player, monster.xp)}
    else
        let monsters = removeFromList monster state.monsters
        "You hit " + monster.name + " for " + damage.ToString(), {state with monsters = newMonster :: monsters}

let attack(state, monster) =
    damageMonster(state, monster, state.player.damage)

let rec getMonsterAt(monsters, x, y) =
    match monsters with
    | a::b -> if(a.obj.x = x && a.obj.y = y) then Some a else getMonsterAt(b, x, y)
    | [] -> None

let attackDir(dir, x, y, state) =
    let x = state.player.obj.x + x
    let y = state.player.obj.y + y;
    if(tileExists(x, y, state)) then
        match getMonsterAt(state.monsters, x, y) with
        | Some monster -> attack(state, monster)
        | _ -> "You take a swing at the empty air", state
    else
        "You take a swing at the empty air", state

let changeTile(x, y, tile, state) =
    state.map.[y].[x] <- tile

let fireball(state, monster) =
    damageMonster(state, monster, 100)

let fireballDir(dir, x, y, state) =
    let x = state.player.obj.x + x
    let y = state.player.obj.y + y;
    if (state.player.mp >= 3) then
        let state = {state with player = {state.player with mp = state.player.mp - 3}}
        if(tileExists(x, y, state)) then
            let tile = state.map.[y].[x]
            match tile with
            | 'w' -> changeTile(x, y, 'o', state)
                     "Your fireball destroyed a wall", state
            | _ ->  match getMonsterAt(state.monsters, x, y) with
                    | Some monster -> fireball(state, monster)
                    | _ -> "You cast a fireball into the empty air", state
        else
            "You cast a fireball into the void", state
    else
        "You don't have enough mana to cast fireball", state

let parseCommand (x, state : State) =
    if(state.running) then
        let commands = helpCommandsNormalState(state)
        match x with
        | "fireball" -> applyDir(getDirection("ahead", state), state, fireballDir)
        | "fireball left" -> applyDir(getDirection("left", state), state, fireballDir)
        | "fireball right" -> applyDir(getDirection("right", state), state, fireballDir)
        | "hit" -> applyDir(getDirection("ahead", state), state, attackDir)
        | "hit left" -> applyDir(getDirection("left", state), state, attackDir)
        | "hit right" -> applyDir(getDirection("right", state), state, attackDir)
        | "stop" -> "Bye", {state with running = false}
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
        | "loot" -> loot(state)
        | "use health potion" -> useItem("health potion", state)
        | "use mana potion" -> useItem("mana potion", state)
        | "eat bread" -> useItem("bread", state)
        | "use emerald" -> useItem("emerald", state)
        | "eat easter egg" -> useItem("easter egg", state)
        | "examine health potion" -> examineItem("health potion", state)
        | "examine mana potion" -> examineItem("mana potion", state)
        | "examine bread" -> examineItem("bread", state)
        | "examine emerald" -> examineItem("emerald", state)
        | "examine easter egg" -> examineItem("easter egg", state)
        | "inventory" -> "", showInventory(state)
        | "help" -> (commands|> String.concat "\n",state)
        | "stats" -> "", showStats(state)
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
