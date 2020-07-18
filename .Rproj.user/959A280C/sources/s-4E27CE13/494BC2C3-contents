# Import tidy tuestday data from
# https://github.com/rfordatascience/tidytuesday/tree/master/data/2020/2020-05-26

library(tidyverse)

# Alt 1
# Download file from github , note need to change the domain of the URL first part!
# https://stackoverflow.com/questions/59144837/accessing-a-csv-file-hosted-on-github-with-r

download.file("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/boston_cocktails.csv",
              "src/data/boston_cocktails.csv")

td <- readr::read_csv("src/data/boston_cocktails.csv")

# Alt 2
# read it directly without first download the file first
td2 <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/boston_cocktails.csv")
(rm2)

# Check dataset
glimpse(td)

# count the names and row_id, they seem so be same when looking at the data
td %>% count(name)   # 989 rows
td %>% count(row_id) # 989 rows
td %>% count(name,row_id)

td %>%
  group_by(name) %>%
  summarise(ids = n_distinct(row_id)) %>%
  filter(ids > 1)
# no output, so name and row_id is equivalent

# check distribution of numeric variables
td %>%
  select_if(is_numeric) %>%
  gather(key = "key", value = "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(~key, scales = "free")

# Check the number of ingredients in each cocktail
td %>%
  count(name) %>% # count how many ingredients (rows) each coctacil name has
  count(n) %>%    # count again, how many have e.g n = 5 (a tibble with 6 rows x 2 cols)
  ggplot(aes(x = n, y = nn)) +
  geom_col() +
  coord_flip()
# most have 3 or 4 ingredients

# 31 cocktails with 1 ingredient, suggests only booze is included
td %>%
  group_by(name) %>%
  filter(n() == 1)

td %>%
  group_by(name) %>%
  filter(n() == 2)

# How big is each cocktail?

td %>%
  count(measure, sort = TRUE)
# most of them are 1 oz


non_spirit <- c("Chilled Champagne", "Water", "Orange Juice", "Cranberry Juice", "Light Cream (if desired)", "Fresh orange juice", "Orange juice")

# Hadley: Have removed all cocktails with ounces of bitters - that is most
# likely a data entry error (it certainly doesn't give me much confidence
# in the quality of this data)

sizes <- td %>%
  filter(str_detect(measure, "oz")) %>%
  filter(!str_detect(ingredient, fixed("bitters", ignore_case = TRUE))) %>%
  filter(!ingredient %in% non_spirit) %>%
  mutate(oz = str_replace(measure, " oz", "")) %>%
  mutate(oz = str_replace(oz, " ?1/2", ".5")) %>%
  mutate(oz = str_replace(oz, " ?1/4", ".25")) %>%
  mutate(oz = str_replace(oz, " ? ?3/4", ".75")) %>%
  mutate(oz = str_replace(oz, " ?1/3", ".33")) %>%
  mutate(oz = str_replace(oz, " ?2/3", ".66")) %>%
  mutate(oz = as.numeric(oz))

filter(sizes, oz > 3)
filter(sizes, oz > 10)

total_size <- sizes %>%
  group_by(name) %>%
  summarise(n = n(), oz = sum(oz))

total_size %>% filter(oz > 20)

# distribution of sizes
total_size %>%
  filter(oz < 20) %>%
  ggplot(aes(x = oz)) +
  geom_histogram(binwidth = 0.5)

total_size %>%
  filter(oz > 6) %>%
  semi_join(td, ., by = "name") %>%
  View()

td %>%
  filter(str_detect(ingredient, "bitters"))

sizes %>%
  group_by(ingredient) %>%
  summarise(n = n(), oz = mean(oz)) %>%
  filter(n > 5) %>%
  arrange(desc(oz), sort = TRUE) # Why sort = TRUE?

# Which are the main ingredients

td <- td %>%
  mutate(ingredient = tolower(ingredient))

td %>%
  count(ingredient = tolower(ingredient), sort = TRUE) %>% # here sort = TRUE makes sense, it arranges n
  head(20)

standard_ingredients <- tribble(
  ~ ingredient,        ~ standard_name,
  "fresh lemon juice", "lemon juice",
  "juice of a lemon",  "lemon juice",
  "fresh lime juice",  "lime juice",
  "juice of a lime",   "lime juice",
  "bitters",           "angostura bitters"
)

class(standard_ingredients)
str(standard_ingredients)
glimpse(standard_ingredients)

ingredient_changes <- td %>%
  select(name, ingredient_number, ingredient) %>% # 3 643
  right_join(standard_ingredients) %>% # reduces it down to 372
  select(name, ingredient_number, ingredient = standard_name)

td %>%
  rows_update(ingredient_changes, by = c("name","ingredient_number")) %>%
  count(ingredient, sort = TRUE) %>%
  head(20)

# source: https://gist.github.com/hadley/a892ff8f00973e3bc864851deae8315f
# Source: https://www.youtube.com/watch?v=kHFmtKCI_F4&feature=youtu.be
