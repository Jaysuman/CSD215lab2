type Coach = { Name: string; FormerPlayer: bool }
type Stats = { Wins: int; Losses: int }
type Team = { Name: string; Coach: Coach; Stats: Stats }

let coach1 = { Name = "Joe Mazzulla"; FormerPlayer = true }
let stats1 = { Wins = 64; Losses = 67 }
let team1 = { Name = "Boston Celtics"; Coach = coach1; Stats = stats1 }

let coach2 = { Name = "Michael Malone"; FormerPlayer = false }
let stats2 = { Wins = 57; Losses = 25 }
let team2 = { Name = "Denver Nuggets"; Coach = coach2; Stats = stats2 }

let coach3 = { Name = "Steve Kerr"; FormerPlayer = true }
let stats3 = { Wins = 46; Losses = 50 }
let team3 = { Name = "Golden State Warriors"; Coach = coach3; Stats = stats3 }

let coach4 = { Name = "Rick Carlisle"; FormerPlayer = false }
let stats4 = { Wins = 47; Losses = 35 }
let team4 = { Name = "Indiana Pacers"; Coach = coach4; Stats = stats4 }

let coach5 = { Name = "Tom Thibodeau"; FormerPlayer = false }
let stats5 = { Wins = 50; Losses = 32 }
let team5 = { Name = "New York Knicks"; Coach = coach5; Stats = stats5 }

let teams = [team1; team2; team3; team4; team5]

let displayTeams (teams: Team list) =
    printfn "%-20s %-20s %-15s %-10s %-10s" "Team" "Coach" "Former Player" "Wins" "Losses"
    printfn "%-20s %-20s %-15s %-10s %-10s" "----" "-----" "-------------" "----" "------"
    teams |> List.iter (fun team ->
        printfn "%-20s %-20s %-15b %-10d %-10d"
            team.Name team.Coach.Name team.Coach.FormerPlayer team.Stats.Wins team.Stats.Losses
    )

let displaySuccessfulTeams (successfulTeams: Team list) =
    printfn "%-20s %-20s %-15s %-10s %-10s" "Team" "Coach" "Former Player" "Wins" "Losses"
    printfn "%-20s %-20s %-15s %-10s %-10s" "----" "-----" "-------------" "----" "------"
    successfulTeams |> List.iter (fun team ->
        printfn "%-20s %-20s %-15b %-10d %-10d"
            team.Name team.Coach.Name team.Coach.FormerPlayer team.Stats.Wins team.Stats.Losses
    )

printfn "All Teams:"
displayTeams teams
printfn "" 

let successfulTeams = teams |> List.filter (fun team -> team.Stats.Wins > team.Stats.Losses)

printfn "\nSuccessful Teams:"
printfn "" 
displaySuccessfulTeams successfulTeams
printfn "" 

let successPercentage (team: Team) =
    let wins = float team.Stats.Wins
    let losses = float team.Stats.Losses
    (wins / (wins + losses)) * 100.0

let teamsWithSuccessPercentage = teams |> List.map (fun team -> (team.Name, successPercentage team))

printfn "\nTeams with Success Percentage:"
printfn "" 
teamsWithSuccessPercentage |> List.iter (fun (name, percentage) -> printfn "%s: %.2f%%" name percentage)
printfn "" 

type Cuisine =
    | Korean
    | Turkish

type MovieType =
    | Regular
    | IMAX
    | DBOX
    | RegularWithSnacks
    | IMAXWithSnacks
    | DBOXWithSnacks

type Activity =
    | BoardGame
    | Chill
    | Movie of MovieType
    | Restaurant of Cuisine
    | LongDrive of int * float

let calculateBudget (activity: Activity) : float =
    match activity with
    | BoardGame -> 0.0
    | Chill -> 0.0
    | Movie movieType ->
        match movieType with
        | Regular -> 12.0
        | IMAX -> 17.0
        | DBOX -> 20.0
        | RegularWithSnacks -> 12.0 + 5.0
        | IMAXWithSnacks -> 17.0 + 5.0
        | DBOXWithSnacks -> 20.0 + 5.0
    | Restaurant cuisine ->
        match cuisine with
        | Korean -> 70.0
        | Turkish -> 65.0
    | LongDrive (km, fuelPerKm) -> float km * fuelPerKm

let testActivities = [
    BoardGame
    Chill
    Movie Regular
    Movie RegularWithSnacks
    Restaurant Korean
    Restaurant Turkish
    LongDrive (130, 0.5)
]

let totalBudget = testActivities |> List.map calculateBudget |> List.sum

for activity in testActivities do
    let cost = calculateBudget activity
    printfn "The cost for %A is %.2f CAD" activity cost

printfn "\nTotal budget required: %.2f CAD" totalBudget
