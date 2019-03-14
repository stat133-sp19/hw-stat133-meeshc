########################################
## Title: Make Shots Data Script
## Description: A script to prepare the data by creating a data file that will be used in the visualization phase.
## Input: Five CSV files, one for each player 
## Output: CSV file
########################################

iguodala <- read.csv("../data/andre-iguodala.csv", stringsAsFactors = FALSE)
green <- read.csv("../data/draymond-green.csv", stringsAsFactors = FALSE)
durant <- read.csv("../data/kevin-durant.csv", stringsAsFactors = FALSE)
thompson <- read.csv("../data/klay-thompson.csv", stringsAsFactors = FALSE)
curry <- read.csv("../data/stephen-curry.csv", stringsAsFactors = FALSE)

iguodala$name <- "Andre Iguodala"
green$name <- "Draymond Green"
durant$name <- "Kevin Durant"
thompson$name <- "Klay Thompson"
curry$name <- "Stephen Curry"

iguodala$shot_made_flag[iguodala$shot_made_flag == 'n'] <- 'shot_no'
iguodala$shot_made_flag[iguodala$shot_made_flag == 'y'] <- 'shot_yes'
green$shot_made_flag[green$shot_made_flag == 'n'] <- 'shot_no'
green$shot_made_flag[green$shot_made_flag == 'y'] <- 'shot_yes'
durant$shot_made_flag[durant$shot_made_flag == 'n'] <- 'shot_no'
durant$shot_made_flag[durant$shot_made_flag == 'y'] <- 'shot_yes'
thompson$shot_made_flag[thompson$shot_made_flag == 'n'] <- 'shot_no'
thompson$shot_made_flag[thompson$shot_made_flag == 'y'] <- 'shot_yes'
curry$shot_made_flag[curry$shot_made_flag == 'n'] <- 'shot_no'
curry$shot_made_flag[curry$shot_made_flag == 'y'] <- 'shot_yes'

iguodala$minutes[iguodala$shot_made_flag == 'shot_yes'] <- iguodala$period[iguodala$shot_made_flag == 'shot_yes'] * 12 - iguodala$minutes_remaining[iguodala$shot_made_flag == 'shot_yes']
green$minutes[green$shot_made_flag == 'shot_yes'] <- green$period[green$shot_made_flag == 'shot_yes'] * 12 - green$minutes_remaining[green$shot_made_flag == 'shot_yes']
durant$minutes[durant$shot_made_flag == 'shot_yes'] <- durant$period[durant$shot_made_flag == 'shot_yes'] * 12 - durant$minutes_remaining[durant$shot_made_flag == 'shot_yes']
thompson$minutes[thompson$shot_made_flag == 'shot_yes'] <- thompson$period[thompson$shot_made_flag == 'shot_yes'] * 12 - thompson$minutes_remaining[thompson$shot_made_flag == 'shot_yes']
curry$minutes[curry$shot_made_flag == 'shot_yes'] <- curry$period[curry$shot_made_flag == 'shot_yes'] * 12 - curry$minutes_remaining[curry$shot_made_flag == 'shot_yes']

sink(file = "../output/andre-iguodala-summary.txt")
summary(iguodala)
sink()
sink(file = "../output/draymond-green-summary.txt")
summary(green)
sink()
sink(file = "../output/kevin-durant-summary.txt")
summary(durant)
sink()
sink(file = "../output/klay-thompson-summary.txt")
summary(thompson)
sink()
sink(file = "../output/stephen-curry-summary.txt")
summary(curry)
sink()

combined <- do.call("rbind", list(iguodala, green, durant, thompson, curry))
write.csv(
  x = combined,
  file = "../data/shots-data.csv"
)

sink(file = "../output/shots-dtat-summary.txt")
summary(combined)
sink()
