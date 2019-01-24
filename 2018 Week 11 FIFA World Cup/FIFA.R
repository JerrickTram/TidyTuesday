library(ggrepel)
library(ggthemes)
library(tidyverse)

df <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018/2018-06-12/week11_fifa_audience.csv")

df %>% 
  top_n(50, wt = tv_audience_share) %>% 
  ggplot(aes(fct_reorder(confederation, tv_audience_share), 
             tv_audience_share, 
             col = population_share, 
             label = country)) +
  geom_boxplot() +
  geom_jitter(width = 0.1) +
  geom_text_repel(data = subset(df, tv_audience_share >= 2.5),
                  direction = "y",
                  size = 5,
                  box.padding = unit(0.5, "lines"),
                  point.padding = unit(0.5, "lines")) +
  coord_flip() + 
  labs(x = "Confederation",
       y = "TV Audience Share",
       col = "Population Share (%)",
       title = "Top 50 Countries in World Cup Viewership (2010)",
       caption = "Source: FiveThirtyEight Article") +
  theme_few()