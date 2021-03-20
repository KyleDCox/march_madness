R0 <- read.csv("../march_madness/teams.csv")
# incorporate previous game data


score <- function(team, df){
  w <- df$w[which(df$team == team)]
  l <- df$l[which(df$team == team)]
  seed <- df$seed[which(df$team == team)]
  
  wr <- w / (w + l)
  seed.wt <- (17 - seed) / 17
  return(mean(c(wr, seed.wt)))
}

play <- function(match, df){
  team1 <- df$team[which(df$matches == match)][1]
  team2 <- df$team[which(df$matches == match)][2]
  score1 <- score(team1, df)
  score2 <- score(team2, df)
  # how different would this be if it were deterministic (highest score wins)
  ranks <- sample(c(team1, team2), 2, prob = c(score1, score2) / (score1 + score2), replace = F)
  winner <- ranks[1]
  loser <- ranks[2]
  
  wins <- df$w[which(df$team == winner)] + 1
  losses <- df$l[which(df$team == winner)]
  
  seedW <- df$seed[which(df$team == winner)]
  seedL <- df$seed[which(df$team == loser)]
  if(seedW > seedL){
    # seed updating too strong?
    seedW <- mean(c(seedW, seedL))
  }
  return(c(winner, wins, losses, seedW))
}


run_round <- function(df){
  games <- 1:(nrow(df)/2) 
  df$matches <- rep(games, each=2)
  next_round <- as.data.frame(t(sapply(games, play, df = df)))
  
  colnames(next_round) <- colnames(df)[1:4]
  next_round$w <- as.numeric(next_round$w)
  next_round$l <- as.numeric(next_round$l)
  next_round$seed <- as.numeric(next_round$seed)
  
  return(next_round)
}

R1 <- run_round(R0)
R2 <- run_round(R1)
R3 <- run_round(R2)
R4 <- run_round(R3)
R5 <- run_round(R4)
R6 <- run_round(R5)
