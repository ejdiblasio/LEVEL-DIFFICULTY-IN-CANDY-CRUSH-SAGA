library(tidyverse)
library(ggthemes)

setwd("/Documents/COSC 375/Candy Crush")

# Import the csv file using readr
cc_1 <- read_csv("candy_crush.csv")

# Observe the first 6 rows of the data
head(cc_1)

print("Number of players:")
print(length(unique(cc_1$player_id)))

print("Period for which we have data:")
print(range(cc_1$dt))

# Calculation of difficulty done by wins/attempts (Bernoulli process in that there are only two outcomes, win or lose)
difficulty <- cc_1 %>%
  group_by(level) %>%
  summarize(total_success = sum(num_success), total_attempts = sum(num_attempts)) %>%
  mutate(p_win = (total_success / total_attempts))
print(difficulty$p_win)

# Create a new column that indicates whether a level is hard or not
difficulty <- difficulty %>%
  mutate(diff = ifelse(p_win < 0.10, "HARD", "EASY"))

# Standard Error calculation to be added to the graph
# n is the total attempts because that is the number of datapoints
difficulty <- difficulty %>%
  mutate(std_error = sqrt(p_win * (1 - p_win) / total_attempts))

# Creates a line/point plot to indicate level difficulty (levels over 10% = HARD)
ggplot(data = difficulty, mapping = aes(x = level, y = p_win)) + 
  geom_line() + 
  scale_x_continuous(breaks = 1:15) +
  scale_y_continuous(labels = scales::percent) +
  geom_point(aes(color = diff)) +
  ggtitle("Win Percentage Based On Level Difficulty") +
  ylab("Win %") +
  theme_stata() + 
  geom_hline(yintercept = 0.1, linetype='dashed', color='red') +
  geom_errorbar(aes(ymin= p_win - std_error, ymax = p_win + std_error), width = 0.4)
