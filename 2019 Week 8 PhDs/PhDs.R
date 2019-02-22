library(gganimate)
library(ggthemes)
library(tidyverse)

df <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-19/phd_by_field.csv")

df$year <- as.Date(paste(df$year, 1, 1, sep = "-"))

df_social <- df %>% 
  filter(broad_field == "Psychology and social sciences") %>% 
  na.omit()
   
df_social %>% 
  ggplot(aes(major_field, n_phds)) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y = "median", geom = "point", shape = 23, size = 1) +
  labs(x = "Field",
       y = "Total PhDs Awarded",
       title = "Allocation of Social Science PhDs in {round(frame_time)}",
       caption = "Source: NSF") +
  transition_time(year) +
  theme_economist()