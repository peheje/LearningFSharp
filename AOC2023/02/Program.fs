type GameSet = { red: int; green: int; blue: int }
type Game = { sets: GameSet seq; id: int }

let redLimit = 12
let greenLimit = 13
let blueLimit = 14

let rows = System.IO.File.ReadAllLines "data"

let toGame (row: string) =
    let spaceIndex = row.IndexOf " "
    let colonIndex = row.IndexOf ":"

    let gameId = row[spaceIndex + 1 .. colonIndex - 1] |> int
    let game = row.Substring(colonIndex + 2)

    let sets =
        game.Split "; "
        |> Seq.map (fun rawSet ->
                rawSet.Split ", " |> Seq.fold (fun set cube ->
                    let plays = cube.Split " "
                    let amount = plays[0] |> int
                    let color = plays[1]
                    match color with
                    | "blue" -> { set with blue = amount }
                    | "green" -> { set with green = amount }
                    | "red" -> { set with red = amount }
                ) { red = 0; green = 0; blue = 0 })

    { sets = sets; id = gameId }

let games = rows |> Seq.map toGame

let isPossibleSet set =
    set.green <= greenLimit && set.blue <= blueLimit && set.red <= redLimit

let part1 =
    games
    |> Seq.filter (fun game -> game.sets |> Seq.forall isPossibleSet)
    |> Seq.sumBy _.id

let minimumGameSet (sets: GameSet seq): GameSet =
    sets |> Seq.fold (fun minimalSet currentSet ->
        {
            red = max currentSet.red minimalSet.red
            green = max currentSet.green minimalSet.green
            blue = max currentSet.blue minimalSet.blue
        }
    ) { red = 0; green = 0; blue = 0 }

let powerLevel set =
    set.blue * set.green * set.red

let part2 = games |> Seq.sumBy (fun game ->
    game.sets |> minimumGameSet |> powerLevel
)