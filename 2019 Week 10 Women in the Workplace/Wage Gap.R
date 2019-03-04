library(stargazer)
library(tidyverse)

df <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-05/jobs_gender.csv")

# Is the female wage gap more prominent in different class of work
df_class <- df %>% 
  filter(minor_category %in% c("Management", 
                             "Office and Administrative Support",
                             "Food Preparation and Serving Related")) %>% 
  mutate(majority_female = ifelse(percent_female > 50.0, 1, 0),
         male_differential = total_earnings_male - total_earnings,
         female_differential = total_earnings_female - total_earnings) 

df_class %>% 
  na.omit() %>% 
  group_by(year, minor_category) %>% 
  summarize(differential = median(wage_percent_of_male)) %>% 
  ggplot(aes(year, differential, fill = minor_category)) +
    geom_col(position = "dodge") +
    scale_y_continuous(limits = c(0, 100)) +
    labs(x = "",
         y = "Female Wage Percentage vs Male",
         fill = "Profession Class Level",
         title = "Tidy Tuesday Week 10: Women in the Workplace",
         caption = "Source: Census Bureau")
ggsave("women_wage.png")

# Does higher representation of females reduce the wage differential
model_1 <- lm(female_differential ~ percent_female + 
                                    majority_female + 
                                    minor_category + 
                                    total_earnings_female,
               data = df_class)
summary(model_1)

model_2 <- lm(female_differential ~ percent_female + 
                majority_female + 
                minor_category:percent_female + 
                total_earnings_female,
              data = df_class)
summary(model_2)

stargazer(model_1, model_2, type = "text",
          column.labels = c("Main Effects", "Interaction"), 
          intercept.bottom = FALSE, 
          single.row=FALSE,     
          notes.append = FALSE, 
          header=FALSE) 