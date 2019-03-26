library(maps)
library(tidyverse)
library(zipcode)

# Load datasets
df <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-26/seattle_pets.csv")
data(zipcode)
zipcode <- zipcode
WA <- map_data("state", region = "Washington")

# Check dimensions
glimpse(df)

# Change column names in datasets for merging prep
names(df)[7] <- "zip"
names(WA)[1:2] <- c("longitude", "latitude")
names(WA)[5] <- "state"

# Join datasets by zip code
df_zip <- merge(df, zipcode, by.x = "zip", by.y = "zip")

# Are short names more popular amongst dogs or cats in Seattle?
df_zip %>% 
  na.omit(animals_name) %>% 
  mutate(short = ifelse(str_length(animals_name) <= 5, "Yes", "No")) %>%
  filter(state == "WA") %>% 
  ggplot(aes(longitude, latitude)) +
  geom_polygon(WA, mapping = aes(longitude, latitude, group = group), fill = "gray") +
  geom_point(aes(color = short)) +
  coord_fixed() +
  facet_wrap(~ species) +
  labs(x = "",
       y = "",
       col = "Short Name",
       title = "Pets in Seattle, Washington with less than or equal to 5 characters",
       caption = "Source: Seattle Open Portal")

ggsave("Pet Names.png", width = 20, height = 10, units = "cm")

# Do Seattle residents prefer short-named cats or dogs?
df_zip %>% 
  na.omit(animals_name) %>% 
  mutate(short = ifelse(str_length(animals_name) <= 5, "Yes", "No")) %>%
  filter(state == "WA") %>% 
  group_by(species) %>% 
  count(short, sort = TRUE)
