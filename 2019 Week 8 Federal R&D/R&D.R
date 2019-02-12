library(gganimate)
library(tidyverse)

rd <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-12/fed_r_d_spending.csv")


# Which department frequently gets the most funding?
rd %>% 
  group_by(year) %>% 
  mutate(budget_rank = rank(-rd_budget)) %>%             # Rank budget from most to least
  transmute(department, rd_budget, budget_rank) %>%
  ggplot(aes(department, budget_rank)) +
    geom_col() + 
    coord_flip() +
    facet_wrap(~ department) +
    labs(x = "Budget Rank",
         y = "Frequency",
         fill = "Department",
         title = "The Big Budgets of Government Agencies in R&D")

# What department(s) have obtained larger budgets over time?
rd %>% 
  group_by(year) %>% 
  mutate(budget_rank = rank(-rd_budget)) %>%             
  transmute(department, rd_budget, budget_rank) %>%
  ggplot(aes(department, budget_rank)) +
  geom_col() + 
  coord_flip() +
  labs(x = "Budget Rank",
       y = "Frequency",
       fill = "Department",
       title = "The Big Budgets of Government Agencies in R&D in {frame_time}") +
  transition_time(year)