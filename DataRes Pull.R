library(tidyverse)




### This part gets all the data from one season into a list
#availGs has to be list of filenames of the games needed (non playoff, non missing metadata)
# for availGs use list.files(path = getwd())

setwd("/Users/Kaushik/Desktop/ipl_csv/IPL 2019")
availGs <- list.files(path = "/Users/Kaushik/Desktop/ipl_csv/IPL 2019")[1:53]


match_count <- 0
matches <- list()
for (i in 1:53){
  winningScores = c(0,0,0,0,0,0,0,0,0,0,0)
  losingScores = c(0,0,0,0,0,0,0,0,0,0,0)
  
  z <- availGs[i]
  
  game <- read.csv(z, header = FALSE, col.names = c("info", "innings", 
                                                    "ball", "battingteam", 
                                                    "striker", "runner", 
                                                    "bowler", "runs", 
                                                    "extras", "how out", 
                                                    "who out"))
  
  # finf the players from both teams
  players <- unique(game$striker, max = 22, incomparables =  FALSE)
  players2 <- unique(game$bowler, max = 22, incomparables =  FALSE)
  players <- union(players, players2)
  endPoint <- length(players)
  players <- players[2:endPoint]
  
  # extract who is the winner and loser
  winningTeam <- as.character(game[19,3])
  if (winningTeam == as.character(game[2,3])){ 
    losingTeam <- as.character(game[3,3])
  }else losingTeam <- as.character(game[2,3])
  
  #team1 is first batting, team2 is second batting
  team1Players <- c()
  team2Players <- c()
  game$ball <- as.character(game$ball)
  game$battingteam <- as.character(game$battingteam)
  team1 <- game$battingteam[21]
  if (team1 == game$ball[2]){
    team2 <- game$ball[3]
  } else {
    team2 <- game$ball[2]
  }
  x = 1
  p <- length(players) + 1
  while (x < p){
    name <- players[x]
    for (y in 21:nrow(game)){
      if ((game$battingteam[y] == team1) & (game$striker[y] == name)){
        team1Players <- append(team1Players, name)
        break
      }else if ((game$battingteam[y] == team2) & (game$striker[y] == name)){
        team2Players <- append(team2Players, name)
        break
      }
    }
    x <- x + 1
  }
  
  #finding scores for each player for both teams
  team1Scores = c()
  team2Scores = c()
  
  i = 1
  while (i <= length(team1Players)){
    x = 21
    runs = 0
    while (x < nrow(game)){
      if (game[x, "striker"] == team1Players[i]){
        runs <- runs + game[x, "runs"]
      }
      x <- x + 1
    }
    team1Scores = append(team1Scores, runs)
    i = i + 1
  }
  i = 1
  while (i <= length(team2Players)){
    x = 21
    runs = 0
    while (x < nrow(game)){
      if (game[x, "striker"] == team2Players[i]){
        runs <- runs + game[x, "runs"]
      }
      x <- x + 1
    }
    team2Scores = append(team2Scores, runs)
    i = i + 1
  }
  
  remove(i, name, runs, x, y)
  
  remove(players2)
  
  # creating data tables for both teams
  x <- length(team1Players) + 1
  extraPlayers <- c()
  extraScores <- c()
  while (x < 12){
    extraPlayers <- append(extraPlayers, "Player")
    extraScores <- append(extraScores, 0)
    x <- x + 1
  }
  team1Players <- append(team1Players, extraPlayers)
  team1Scores <- append(team1Scores, extraScores)
  team1_data <- data.frame(names = team1Players, scores = team1Scores)
  
  x <- length(team2Players) + 1
  extraPlayers <- c()
  extraScores <- c()
  while (x < 12){
    extraPlayers <- append(extraPlayers, "Player")
    extraScores <- append(extraScores, 0)
    x <- x + 1
  }
  team2Players <- append(team2Players, extraPlayers)
  team2Scores <- append(team2Scores, extraScores)
  team2_data <- data.frame(names = team2Players, scores = team2Scores)
  
  remove(extraPlayers, extraScores, x)
  
  #merging both frames
  team1_data <- mutate(team1_data, team = team1)
  team2_data <- mutate(team2_data, team = team2)
  match_data <- rbind(team1_data, team2_data)
  match_data$team <- as.factor(match_data$team)
  match_data$names <- as.character(match_data$names)
  serial_no <- c(1,2,3,4,5,6,7,8,9,10,11,1,2,3,4,5,6,7,8,9,10,11)
  match_data <- mutate(match_data, serial_no)
  
  match_count <- match_count + 1
  
  if (team1 == winningTeam){
    winningScores = team1Scores
    losingScores = team2Scores
  } else {
    winningScores = team2Scores
    losingScores = team1Scores
  }
  matches[[match_count]] <- list("Match_No" = match_count, 
                             "Winner" = winningTeam, 
                             "WScores" = winningScores, 
                             "Loser" = losingTeam, 
                             "LScores" = losingScores)
}


### this part assists in manual detection of number of wins and losses
wins <- c()
losses <- c()
for (i in 1:53){ wins <- c(wins, matches[[i]]$Winner); losses <- c(losses, matches[[i]]$Loser)}
table(wins)
table(losses)


### create data frames for each team
rcb <- data.frame("Pos" = 1:11, "win" = numeric(11), "win_no" = 4, "lose" = numeric(11), "lose_no" = 8 )

csk <- data.frame("Pos" = 1:11, "win" = numeric(11), "win_no" = 9, "lose" = numeric(11), "lose_no" = 5)

srh <- data.frame("Pos" = 1:11, "win" = numeric(11), "win_no" = 6, "lose" = numeric(11), "lose_no" = 6)

dd <- data.frame("Pos" = 1:11, "win" = numeric(11), "win_no" = 8, "lose" = numeric(11), "lose_no" = 6)

kxip <- data.frame("Pos" = 1:11, "win" = numeric(11), "win_no" = 6, "lose" = numeric(11), "lose_no" = 7)

mi <- data.frame("Pos" = 1:11, "win" = numeric(11), "win_no" = 8, "lose" = numeric(11), "lose_no" = 6)

kkr <- data.frame("Pos" = 1:11, "win" = numeric(11), "win_no" = 5, "lose" = numeric(11), "lose_no" = 7)

rr <- data.frame("Pos" = 1:11, "win" = numeric(11), "win_no" = 5, "lose" = numeric(11), "lose_no" = 8)

### add data to said data frames
for (i in 1:53){
  if (matches[[i]]$Winner == "Royal Challengers Bangalore"){ 
    rcb$win <- rcb$win + matches[[i]]$WScores
  } else if (matches[[i]]$Winner == "Chennai Super Kings"){
    csk$win <- csk$win + matches[[i]]$WScores
  } else if (matches[[i]]$Winner == "Mumbai Indians"){
    mi$win <- mi$win + matches[[i]]$WScores
  } else if (matches[[i]]$Winner == "Sunrisers Hyderabad"){
    srh$win <- srh$win + matches[[i]]$WScores
  } else if (matches[[i]]$Winner == "Kings XI Punjab"){
    kxip$win <- kxip$win + matches[[i]]$WScores
  } else if (matches[[i]]$Winner == "Delhi Capitals"){
    dd$win <- dd$win + matches[[i]]$WScores
  } else if (matches[[i]]$Winner == "Rajasthan Royals"){
    rr$win <- rr$win + matches[[i]]$WScores
  } else if (matches[[i]]$Winner == "Kolkata Knight Riders"){
    kkr$win <- kkr$win + matches[[i]]$WScores
  }
}


for (i in 1:53){
  if (matches[[i]]$Loser == "Royal Challengers Bangalore"){ 
    rcb$lose <- rcb$lose + matches[[i]]$WScores
  } else if (matches[[i]]$Loser == "Chennai Super Kings"){
    csk$lose <- csk$lose + matches[[i]]$WScores
  } else if (matches[[i]]$Loser == "Mumbai Indians"){
    mi$lose <- mi$lose + matches[[i]]$WScores
  } else if (matches[[i]]$Loser == "Sunrisers Hyderabad"){
    srh$lose <- srh$lose + matches[[i]]$WScores
  } else if (matches[[i]]$Loser == "Kings XI Punjab"){
    kxip$lose <- kxip$lose + matches[[i]]$WScores
  } else if (matches[[i]]$Loser == "Delhi Capitals"){
    dd$lose <- dd$lose + matches[[i]]$WScores
  } else if (matches[[i]]$Loser == "Rajasthan Royals"){
    rr$lose <- rr$lose + matches[[i]]$WScores
  } else if (matches[[i]]$Loser == "Kolkata Knight Riders"){
    kkr$lose <- kkr$lose + matches[[i]]$WScores
  }
}


### make the scores averages
csk$win <- csk$win / csk$win_no
csk$lose <- csk$lose / csk$lose_no

rcb$win <- rcb$win / rcb$win_no
rcb$lose <- rcb$lose / rcb$lose_no

kxip$win <- kxip$win / kxip$win_no
kxip$lose <- kxip$lose / kxip$lose_no

dd$win <- dd$win / dd$win_no
dd$lose <- dd$lose / dd$lose_no

mi$win <- mi$win / mi$win_no
mi$lose <- mi$lose / mi$lose_no

kkr$win <- kkr$win / kkr$win_no
kkr$lose <- kkr$lose / kkr$lose_no

srh$win <- srh$win / srh$win_no
srh$lose <- srh$lose / srh$lose_no

rr$win <- rr$win / rr$win_no
rr$lose <- rr$lose / rr$lose_no


###### GRAPHS, examples
ggplot(data = rcb, aes(1:11, win)) + 
  geom_point() + geom_smooth(aes(color = "Win")) + 
  geom_smooth(aes(x = 1:11, y = lose, color = "Loss")) + 
  ggtitle("Royal Challengers Bangalore") + 
  labs(color = "Outcome", x = "Batting Position", y = "Runs Scored")

ggplot(data = csk, aes(1:11, win)) + 
  geom_point() + geom_smooth(aes(color = "Win")) + 
  geom_smooth(aes(x = 1:11, y = lose, color = "Loss")) + 
  ggtitle("Chennai Super Kings") + 
  labs(color = "Outcome", x = "Batting Position", y = "Runs Scored")

