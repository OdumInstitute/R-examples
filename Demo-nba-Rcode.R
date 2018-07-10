###################################
# Data codebook
#
# Overview:  Data includes regular season
# number of wins out of an 82 game season.
# Data covers years from 1990 through 2017, and
# only includes the 16 teams each year that
# made the playoffs.
# Two years are dropped (1999 and 2012) because
# labor disputes shortened the season.  All data
# came from the following website:
#
# http://www.landofbasketball.com/nba_year_by_year.htm
#
# The NBA is organized into two conferences, the
# Eastern and Western conferences. 8 teams from each
# make the playoffs.  The playoffs are organized by conference
# so that the finals are always between one team from
# each conference.
#
# Specific Variables are:
# 
# year = year of championship
# team = name of team.
# wins = number of regular season wins
# champ = NBA Champion, 1=Yes, 0=No
# eastern = Team in Eastern Conference, 1=Yes, 0=No
# topall = Team had best record in the NBA that year, 1=Yes, 0=No
# topconf = Team had best record in its conference that year, 1=Yes, 0=No
# 
############################################################


rm(list=ls(all=TRUE))  # Standard code to clear R workspace

Data <- read.csv("dataset-nba-2017.csv")

attach(Data)

L1 <- glm(champ ~ wins,
          family=binomial(link=logit))

###########################################
# Predicted Probabilitis and 95% CI bounds
# for a range of values for the number
# of wins from the previous season.
###########################################

Num.wins <- seq(min(wins)-2,max(wins)+2,by=1)

Sims <- length(Num.wins)

P.Probs <- matrix(NA,Sims,4)

for(i in 1:Sims){
  profile <- with(Data,data.frame(wins=Num.wins[i]))
  PROB <- as.numeric(predict(L1,newdata=profile,
                             type="response",
                             se.fit=TRUE)[1])
  SE <-  as.numeric(predict(L1,newdata=profile,
                            type="response",
                            se.fit=TRUE)[2])
  CI.lo <- PROB-1.96*SE
  CI.hi <- PROB+1.96*SE
  P.Probs[i,] <- c(Num.wins[i],PROB,CI.lo,CI.hi)
}

pdf("NBA_Demo.pdf",width=7, height=5)

par(mar = c(6,5,4,2)+.1)
plot(P.Probs[,1], P.Probs[,2], type = "n", ylim = c(-.025, 1),
     xlab = "Number of Regular Season Wins", 
     ylab = "Predicted Probability",
     axes = TRUE,
     main="Probability of Winning the NBA Championship",
     sub="Data includes all playoff teams from 1990-2017,dropping 1999 and 2012", cex.sub=0.75)
abline(h = seq(0, 1, .1), col = "gray75", lty = 3)
lines(P.Probs[,1], P.Probs[,2], lwd = 5, lty = 1, col="darkgreen") 
lines(P.Probs[,1], P.Probs[,3], lwd = 5, lty = 2, col="darkorange")
lines(P.Probs[,1], P.Probs[,4], lwd = 5, lty = 2, col="darkorange")
box()
rug(jitter(Data$wins, factor=4), ticksize = .04, col="blue")
legend("topleft", bty = "n", 
       c(expression("Point Est."), 
         expression("95% CI")),
       col=c("darkgreen","darkorange"),lty = c(1, 2), lwd = c(5, 5), cex = 1)

dev.off()

