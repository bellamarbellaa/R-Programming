library("tidyverse")

# Install and load packages for cleaning data
install.packages("here")
library("here")
install.packages("skimr")
library("skimr")
install.packages("janitor")
library("janitor")

# Load the penguins data set from the pre-installed "palmerpenguins" data sets package.
# The penguins data set mainly contains 4 size measurements for 
# 3 species of penguins that live in the Palmer Archipelago, Antarctica. 
library("palmerpenguins")
data(penguins)

# Cleaning Data 
# Return the summary of the penguins dataset with the skim_without_charts,
# glimpse, str, and head functions. The penguins dataset includes 344 observations
# and 8 columns: species, island, measurements for bills and flippers,
# body mass in grams, sex, and the year of data collection
skim_without_charts(penguins)
glimpse(penguins)
str(penguins)
head(penguins)

penguins1 <- penguins %>%
  drop_na(bill_length_mm) %>%
  group_by(species, island) %>%
  summarize(average_bill_length = mean(bill_length_mm)) %>%
  arrange(desc(average_bill_length))

penguins_new <- penguins %>%
  select(-species) %>%
  rename(island_new = island) %>%
  rename_with(toupper) %>%
  rename_with(tolower) %>%
  clean_names()

# Organizing Data
penguins_new2 <- penguins_new %>%
  filter(island_new == "Biscoe") %>%
  arrange(-bill_length_mm)
  
penguins_new3 <- penguins %>%
  drop_na() %>%
  group_by(island) %>%
  summarize(longest_bill_length_mm = max(bill_length_mm)) %>%
  arrange(longest_bill_length_mm)

# Summarizing Data
penguins_new4 <- penguins %>%
  drop_na() %>%
  group_by(island, species) %>%
  summarize(longest_bill_length_mm = max(bill_length_mm), average_bill_length_mm = mean(bill_length_mm))

# Transforming Data by adding new column(s)
penguins_new5 <- penguins %>%
  mutate(body_mass_kg = body_mass_g/1000, flipper_length_m = flipper_length_mm/1000)

# Visualizing Data
# The code below takes the "penguins" data, 
# plots the body_mass_g column on the x-axis,
# the flipper_length_mm column on the y-axis,
# and represents the data as a scatter plot, 
# with the difference in color of the data points
# represents the 3 different species of penguins 
# recorded in the species column of the data.
# A trend line is also added to the plot. 
# The plot shows a positive relationship between body mass and flipper
# length, which means the larger the penguin, the longer the flipper is.
# As shown in the legend, among the 3 species of penguins in the data, 
# Gentoo penguins are the heaviest and have the longest flippers. 
ggplot(data = penguins, mapping = aes(x = body_mass_g, y = flipper_length_mm)) + 
  geom_smooth(color = "black") +
  geom_jitter(mapping = aes(color = species), shape = "square") +
  annotate("text", x = 5500, y = 190, color = "#3240a8", family = "sans",  size = 3, fontface = "bold",
  label = "The Gentoos are the 
  heaviest and have
  the longest flippers")
  
# Use the facet_wrap function to create a separate 
# scatter plot and trend line for each species of the penguins.
# The 3 separate scatter plots focus on showing how the
# relationship and trend between body mass and flipper length
# compare between the 3 species of penguins in the data
ggplot(data = penguins, mapping = aes(x = body_mass_g, y = flipper_length_mm)) +
  geom_jitter(mapping = aes(color = species)) +
  geom_smooth(color =  "black") +
  facet_wrap(~species)

# Use the labs function to add title, subtitle, and caption as 
# important background information outside the grid of the plot.
ggplot(data = penguins, mapping = aes(x = body_mass_g, y = flipper_length_mm)) +
  geom_jitter(mapping = aes(color = species)) +
  geom_smooth(color = "black") +
  facet_wrap(~species) +
  labs(title = "Palmer Penguins by Species: Body Mass vs Flipper Length",
       subtitle = "Based on a sample of 344 observations from 3 palmer penguins species",
       caption = "Data collected by Dr. Kristen Gorman between 2007 and 2009",
       x = "body mass (g)", y = "flipper length (mm)")

# Use annotate function to add text inside the grid of the plot
# to highlight specific data points referring to Gentoo penguins
ggplot(data = penguins, mapping = aes(x = body_mass_g, y = flipper_length_mm)) + 
  geom_smooth(color = "black") +
  geom_jitter(mapping = aes(color = species)) +
  annotate("text", x = 5500, y = 190, color = "#3240a8", family = "sans",  size = 3, fontface = "bold",
           label = "The Gentoos are the 
  heaviest and have
  the longest flippers")

# Store titled plot as a variable "scatterplot_penguins" to be 
# used later to avoid duplicating plot-building codes
scatterplot_penguins <- 
  ggplot(data = penguins, mapping = aes(x = body_mass_g, y = flipper_length_mm)) + 
  geom_smooth(color = "black") +
  geom_jitter(mapping = aes(color = species)) +
  labs(title = "Palmer Penguins: Body Mass vs Flipper Length") +
  theme(plot.title = element_text(hjust = 0.5))

scatterplot_penguins2 <- scatterplot_penguins + annotate ("text", x = 5600, y = 190, color = "#3240a8", family = "sans",  size = 3, fontface = "bold", angle = 20,
label = "The Gentoos are the 
heaviest and have
the longest flippers")

scatterplot_penguins2 + labs(caption = "Data collected by Dr. Kristen Gorman")

# Filter the sex column of the data to ensure only
# rows with sex listed as either male or female is 
# are returned to be used for the plot creation.
# Use the facet_grid function to create a separate 
# scatter plot and trend line for each combination
# of specific species and sex category.  The 6
# separate scatter plots focus on showing how the
# relationship and trend between body mass and flipper length
# compare between 6 combinations of penguins' species and sex, 
# each made of one of the 3 different species and 
# one of the 2 different sexes of penguins in the data.
# Use the pipe operator to link the operations of filtering 
# the data and creating scatter plots from the filtered dataset
penguins %>% 
  filter(sex == "female" | sex == "male") %>%
  ggplot(mapping = aes(x = body_mass_g, y = flipper_length_mm)) +
  geom_jitter(mapping = aes(color = species)) +
  geom_smooth(color =  "black") +
  facet_grid(sex~species)

# Use a sequence of data frames to filter the dataset prior
# to creating the scatter plots on the filtered dataset
penguins_filtered_sex <- filter(penguins, sex == "female" | sex == "male")
ggplot(data = penguins_filtered_sex, mapping = aes(x = body_mass_g, y = flipper_length_mm)) +
  geom_jitter(mapping = aes(color = species)) +
  geom_smooth(color =  "black") +
  facet_grid(sex~species)

# The code below takes the "penguins" data,
# plots the body_mass_g column on the x axis
# the flipper_length_mm column on the y-axis, 
# and represent the data as a line chart with
# 3 types of line that represent 3 different 
# species of penguins in the species column. 
# The plot shows how the trends of positive 
# relationship between body mass and flipper length 
# differ for 3 species of penguins recorded in the data
ggplot(data = penguins) + 
  geom_smooth(mapping = aes(x = body_mass_g, y = flipper_length_mm, linetype = species), color = "#f542b3")


