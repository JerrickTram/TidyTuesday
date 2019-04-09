library(lubridate)
library(tidyverse)

theme_set(theme_light())

# Load dataset
df_raw <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/grand_slam_timeline.csv")

# Set factor levels
unique(df_raw$outcome)
df_raw$outcome <- factor(df_raw$outcome, 
                         levels = c("<NA>", "Retired", "Absent",
                                    "Lost Qualifier", "1st Round",
                                    "2nd Round", "3rd Round", "4th Round",
                                    "Semi-finalist", "Quarterfinalist",
                                    "Finalist", "Won"))

# Convert year to date class
df_raw$year <- as.Date(paste(df_raw$year, 1, 1, sep = "-"))

# Create conditions for upcoming analysis
df_total <- df_raw %>% 
  mutate(winner = ifelse(outcome == "Won", 1, 0),
         matches_won = case_when(
           outcome == c("<NA>", "Retired", "Absent", "Lost Qualifier", "1st Round") ~ 0,
           outcome == "2nd Round" ~ 1,
           outcome == "3rd Round" ~ 2,
           outcome == "4th Round" ~ 3,
           outcome == "Semi-finalist" ~ 4,
           outcome == "Quarterfinalist" ~ 5,
           outcome == "Finalist" ~ 6,
           outcome == "Won" ~ 7)
         )

# Top 10 Tennis Athletes with the most Grand Slam Titles
top_5 <- df_total %>% 
  group_by(player) %>% 
  summarize(total_wins = sum(winner, na.rm = TRUE)) %>% 
  top_n(5, total_wins)

# How long does the prime of a top 10 tennis athletes in total grand slam
# wins last?
df_total %>% 
  filter(player %in% top_5$player) %>% 
  ggplot(aes(year, matches_won)) +
    geom_line() +
    geom_point() +
    facet_grid(tournament ~ player) +
    labs(x = "",
         y = "Matches Won",
         title = "Total Matches Won by the Top 5 \nTennis Athletes in Grand Slam Titles",
         subtitle = "Dashed Line Indicates Winning Grand Slam Title",
         caption = "Data Source: Wikipedia") +
    geom_hline(aes(yintercept = 7), linetype = "dashed") 

ggsave("Tennis.png", width = 30, height = 20, units = "cm")
