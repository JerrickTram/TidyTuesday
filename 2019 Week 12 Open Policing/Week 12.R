library(maps)
library(stargazer)
library(tidyverse)

# Prepare dataset for merging
df <- read_csv("https://raw.githubusercontent.com/5harad/openpolicing/master/results/data_for_figures/combined_data.csv")
names(df)[2] <- "region"
names(df)[1] <- "subregion"

# Convert abbreviation to state name
df$region <- str_to_lower(state.name[match(df$region, state.abb)])

# Load states data
states_map <- map_data("state")

# Join fields by region column
new_map <- left_join(df, states_map, by = c("region" = "region"))
names(new_map)[1] <- "subregion"
new_map <- arrange(new_map, group, order)

# Plot map
new_map %>% 
  ggplot(aes(long, lat, group = group, fill = stops_per_year/10000)) + 
    geom_polygon(color = "white") +
    coord_map() + 
    labs(x = "",
         y = "",
        fill = "Stops per Year (0,000s) ",
        title = "Annual Stoppages by Law Enforcement Agencies",
        caption = "Source: Stanford Open Policing Project") +
    facet_wrap(~ driver_race) +
  scale_fill_continuous(type = "viridis")

# Create Regression Table with Interaction Effects
map_reg <- lm(arrest_rate ~ driver_race + stops_per_year + 
                            search_rate, data = df)

summary(map_reg)

map_interaction <- lm(arrest_rate ~ driver_race + stops_per_year +
                                    search_rate + driver_race:search_rate, 
                                    data = df)

summary(map_interaction)

# Table
stargazer(map_reg, map_interaction, type = "html",
          column.labels = c("Base Model", "Interaction"),
          covariate.labels = c("Hispanic", "White", "Stops Per Year",
                              "Search Rate", "Hispanic * Search Rate",
                               "White * Search Rate"),
          omit = "Constant",
          omit.stat = c("f", "ser"),
          omit.summary.stat = "sd",
          font.size = "normalsize",
          intercept.bottom = FALSE, 
          single.row = FALSE,     
          notes.append = FALSE, 
          header = TRUE) 