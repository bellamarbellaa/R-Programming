# Load the installed tidyverse packages
library("tidyverse")

# Load the diamond data set consisting of 10 columns, which includes
# price and physical attributes like carat and clarity for 
# 53,940 diamonds. The diamond data set comes from the pre-installed 
# ggplot2 package, which is part of the tidyverse collection of packages
data(diamonds)

# Display the full contents of the diamonds dataset 
# in a spreadsheet-like format within the the data viewer
# The diamonds dataset is stored as a data frame 
# with 10 columns and 53,490 rows
View(diamonds)

# Preview the columns and first 6 rows of the data frame
head(diamonds)

# Return summaries of the diamonds data frame arranged 
# horizontally by using glimpse(), str(), and colnames() functions:
# Return the transposed version of the data frame with
# column names running down and the few first rows running across
glimpse(diamonds)
# Return the structural overview of the data frame,
# including column names, data types, and the
# first 10 rows of each column in the data frame
str(diamonds)
# Return the list of column names in the data frame
colnames(diamonds)

# Rename several columns in the diamonds dataset
rename(diamonds, carat_new = carat, cut_new = cut)

# Generate mean for the diamonds' carat values
summarize(diamonds, mean_carat = mean(carat))

# Build visualization with the ggplot2 package:
# The code below takes the "diamonds" data, 
# plots the carat column on the X-axis, 
# the price column on the Y-axis, 
# and represents the data as a scatter plot 
# using the "geom_point()" command.
ggplot(data = diamonds, mapping = aes(x = carat, y = price)) + geom_point()

# Use the size argument to decrease the size of data points and
# color argument to change the color of the data points 
# so the colors represent the different diamonds' cut categories
ggplot(data = diamonds, mapping = aes(x = carat, y = price, color = cut), size = 1) +
  geom_point()

# Use the "facet_wrap()" function to create 
# different plots for each category of diamond' cuts
ggplot(data = diamonds, mapping = aes(x = carat, y = price, color = cut), size = 1) +
  geom_point() + 
  facet_wrap(~cut)

# The code below takes the "diamonds" data,
# plots the cut column on the x-axis, the 
# count of observations for each cut on the y-axis,
# and represents the data as a bar chart with 
# differently colored bars to represent
# the 5 categories of diamonds' cut in the data.
# The bar chart compares the quantity of diamonds
# in each category relative to each other, with
# over 20,000 diamonds are observed to have an
# ideal cut, which is the most common type of
# diamond cut categories recorded in the data. 
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = cut), width = 0.9)


# Map fill argument to the clarity variable to
# create a stacked bar chart that represent
# 40 different combinations of cut and clarity as 
# differently sized and colored rectangles. The difference 
# in colors within the same bar reflects the difference 
# in clarity types within the same cut, while the  
# the length of rectangle is proportionate to the amount
# of that specific combination of cut and clarity observed.
# The stacked bar chart compares the difference in volume 
# between both the 5 different categories of diamonds' cut and 
# the 8 different types of clarity within each cut category.
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = clarity), width = 0.9)

# Use the facet_wrap function to create a separate bar chart
# for every category of diamonds' cut, with each bar chart
# use colors and height of bars to compare the volume of 
# 8 different types of clarity present in each cut category.
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = clarity, fill = clarity), width = 0.8) +
  facet_wrap(~cut)

