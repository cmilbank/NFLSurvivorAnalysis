#set working directory

setwd("C:/Users/cmilbank/Documents")

#load libraries

library(expss, lib.loc = "H:/Decision Support/Stats Jam/R files/Libraries")
library(ggplot2, lib.loc = "H:/Decision Support/Stats Jam/R files/Libraries")

#load data

W11 <- read.csv("W11.csv")

#set weekly parameters

remaining_start <- 109
amount <- 250000
n_sims <- 100000

#calculate coverage n's

W11$coverage1 <- remaining_start*W11$C1
W11$coverage2 <- remaining_start*W11$C2

#run sims for visitors

for (i in 1:nrow(W11))
{
  choice <- W11$T1[i]

  do_once <- function()
    {
    W11$rand <- runif(nrow(W11), 0, 1)

    W11$W1[W11$rand < W11$P1] <- 1
    W11$W1[W11$rand > W11$P1] <- 0
    W11$W2 <- 1 - W11$W1

    W11$rem1 <- W11$coverage1*W11$W1
    W11$rem2 <- W11$coverage2*W11$W2

    remaining_end <- sum(W11$rem1) + sum(W11$rem2)

    DR_remaining <- ifelse(W11$rand[i] < W11$P1[i], 1, 0)
    
    EV <- amount/remaining_end*DR_remaining
    EV
    }

  vals <- replicate(n_sims, do_once())
  W11$EW1[i] <- mean(vals)

  next
}

#run sims for home

for (i in 1:nrow(W11))
{
  choice <- W11$T2[i]
  
  do_once <- function()
  {
    W11$rand <- runif(nrow(W11), 0, 1)
    
    W11$W1[W11$rand < W11$P1] <- 1
    W11$W1[W11$rand > W11$P1] <- 0
    W11$W2 <- 1 - W11$W1
    
    W11$rem1 <- W11$coverage1*W11$W1
    W11$rem2 <- W11$coverage2*W11$W2
    
    remaining_end <- sum(W11$rem1) + sum(W11$rem2)
    
    DR_remaining <- ifelse(W11$rand[i] > W11$P1[i], 1, 0)
    
    EV <- amount/remaining_end*DR_remaining
    EV
  }
  
  vals <- replicate(n_sims, do_once())
  W11$EW2[i] <- mean(vals)
  
  next
}

#Summarize data

Away <- W11[,c(1,9)]
names(Away)[1] <- "Team"
names(Away)[2] <- "E_Profit"
Home <- W11[,c(2,10)]
names(Home)[1] <- "Team"
names(Home)[2] <- "E_Profit"
W11_summary <- rbind(Away, Home)
W11_summary <- W11_summary[order(-W11_summary$E_Profit),]
W11_summary_top10 <- W11_summary[1:10,]

#Plot

f <- ggplot(W11_summary_top10, aes(Team, E_Profit))
f + geom_col(fill = "blue2")