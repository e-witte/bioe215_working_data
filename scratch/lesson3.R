library(tidyverse)

#Read and inspect

surveys <- read_csv("data/portal_data_joined.csv")

head(surveys)
summary(surveys)

#Q1: character, numeric or integer, but I vote numeric due to decimal places

#Q2: columns: 13; rows: 34786
dim(surveys)

# select changes columns
select(surveys, plot_id, species_id, weight_g = weight)
select(surveys, -record_id, -species_id)

#Filter changes rows
filter(surveys, year == 1995, plot_id == 7)
filter(surveys, month == 2 | day == 20) #| or operator

#Q3
filter(surveys, month == 11, hindfoot_length > 36.0)

#Q4: 
filter(surveys, year == 1995) # need to use double equals
filter(surveys, plot_id == 2) # spelling mistake in plot

#pipes - helpful for a sequence transformations
select(filter(surveys, year == 1995), plot_id, weight)

#OR

surveys_psw <- surveys %>% 
  filter(year ==1995) %>% 
  select(plot_id, weight)

#Q5: 
surveys5 <- surveys %>% 
  filter(year < 1995) %>% 
  select(year, sex, weight)

#MUTATE - add columns or change in place
surveys %>% 
  mutate(weight_kg = weight/1000, weight_lb = weight_kg * 2.2) %>% 
  view()

surveys %>% 
  filter(!is.na(weight)) %>%
  mutate(weight_kg = weight/1000, 
         weight_lb = weight_kg * 2.2) %>% 
  view()

#Q6:

surveys6 <- surveys %>% 
  filter(!is.na(hindfoot_length), hindfoot_length < 30) %>%
  mutate(hindfoot_cm = hindfoot_length /10) %>% 
  select(species_id, hindfoot_cm) %>% 
  view()

#Summarize
surveys %>% 
  group_by(sex) %>% 
  summarize (mean_weight = mean(weight, na.rm = TRUE))

surveys %>% 
  drop_na(weight, sex) %>% 
  group_by(species_id, sex) %>% 
  summarize(mean_weight = mean(weight),
            min_weight = min(weight),
            .groups = "drop") %>% 
  arrange(desc(mean_weight))

#Q7:
surveys %>% 
  group_by(plot_type) %>% 
  summarize(animal_count = n_distinct(record_id),
            .groups = "drop") %>% 
  arrange(desc(animal_count))

#Q8:
surveys %>% 
  drop_na(hindfoot_length) %>% 
  group_by(species_id) %>% 
  summarize(mean_hfl= mean(hindfoot_length), min_hfl = min(hindfoot_length), max_hfl = max(hindfoot_length), n())
            .groups = "drop") %>% 
  arrange(species_id)
  

#Q9:
surveys %>% 
  drop_na(weight, year) %>% 
  group_by(year) %>% 
  summarize(heaviest = max(weight),
            .groups = "drop") %>% 
  arrange(year) # I am struggling to return more values...

#joining data
count(surveys, taxa)
taxa_iucn <- data.frame(
  taxa = c("Bird", "Rabbit", "Rodent"),
  iucn = c("NT", "LC", "LC")
)
taxa_iucn

surveys_iucn <- left_join(surveys, taxa_iucn, by = "taxa")
head(surveys_iucn)
view(surveys_iucn)

#Q10:
surveys_iucn %>% 
  drop_na(iucn) %>% 
  group_by(iucn) %>% 
  summarize(count = n_distinct(record_id),
            .groups = "drop")

#Q11:
surveys_iucn %>% 
  filter(is.na(iucn)) %>% 
  select(taxa) #Reptiles!

surveys_iucn2 <- inner_join(surveys, taxa_iucn, by = "taxa")

#Q12:
nrow(surveys_iucn) - nrow(surveys_iucn2)
# 14 rows present in surveys_iucn2, not present in surveys_iucn. Reptiles do not have an IUCN status associated, therefore cannot be keyed to carry out using innerjoin. 
