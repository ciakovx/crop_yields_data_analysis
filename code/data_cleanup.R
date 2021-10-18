# this lesson is inspired by Julia Silge: 'Train and analyze many models for #TidyTuesday crop yields' at https://juliasilge.com/blog/crop-yields/
# check out all #TidyTuesday lessons at https://github.com/rfordatascience/tidytuesday

# load libraries ----------------------------------------------------------

library(tidyverse)
library(here)
library(janitor)


# read in the data --------------------------------------------------------

key_crop_yields <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/key_crop_yields.csv')

# create directories
dir.create(here("data"))
dir.create(here("data", "raw"))
dir.create(here("data", "results"))
dir.create("code")

# write raw data file
write_csv(key_crop_yields, here("data", "raw", "key_crop_yields_raw.csv"))


# explore key_crop_yields -------------------------------------------------

# which unique entities exist?
key_crop_yields %>%
  distinct(Entity) %>%
  View()

min(key_crop_yields$Year)
max(key_crop_yields$Year)
summary(key_crop_yields$Year)
hist(key_crop_yields$Year)


# read in land_use --------------------------------------------------------

land_use <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/land_use_vs_yield_change_in_cereal_production.csv')

# write to csv
write_csv(land_use, here("data", "raw", "land_use_raw.csv"))



# get the top 30 countries by population ----------------------------------

# remove NA values from code and remove World
# keep only the most recent year for each entity
# slice to keep the top 30 countries
top_countries <- land_use %>%
  clean_names() %>%
  filter(!is.na(code),
         entity != "World") %>%
  group_by(entity) %>%
  filter(year == max(year)) %>%  
  ungroup() %>%
  slice_max(total_population_gapminder, n = 30) %>%
  pull(entity)


# tidy the crop yields ----------------------------------------------------

# pivot the columns that include crop yield data
# remove the extra text from the crop values
# filter to keep the top 30 countries
# filter to keep only wheat, rice, maize and barley
# remove rows where there is no yield data
tidy_yields <- key_crop_yields %>%
  clean_names() %>%
  pivot_longer(contains("per_hectare"),
               names_to = "crop",
               values_to = "yield") %>%
  mutate(crop = str_remove(crop, "_tonnes_per_hectare")) %>%
  filter(entity %in% top_countries,
         crop %in% c("wheat", "rice", "maize", "barley"),
         !is.na(yield))

write_csv(tidy_yields, here("data", "results", "tidy_yields.csv"))

# explore the output ------------------------------------------------------

# for each country, what is the mean yield across the years specified?
mean_yield_per_country <- tidy_yields %>%
  group_by(entity) %>%
  summarize(mean_yields = mean(yield)) %>%
  arrange(desc(mean_yields))

# for each country and crop, what is the year with the highest yield?
top_yield_per_country_crop <- tidy_yields %>%
  group_by(entity, crop) %>%
  slice_max(yield) %>%
  ungroup() %>%
  filter(entity == "United States")

# plot yield amounts across time
tidy_yields %>%
  ggplot(aes(year, yield, color = crop)) +
  geom_line(alpha = 0.7, size = 1.5) +
  geom_point() +
  facet_wrap(~entity, ncol = 5) +
  scale_x_continuous(guide = guide_axis(angle = 90)) +
  labs(x = NULL, y = "yield (tons per hectare)")
