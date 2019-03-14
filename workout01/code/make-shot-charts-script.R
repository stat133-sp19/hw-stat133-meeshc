library(readr)
library(ggplot2)

klay <- read_csv('../data/klay-thompson.csv')

klay_scatterplot <- ggplot(data = klay) + geom_point(aes(x = x, y = y, color = shot_made_flag))

klay_scatterplot