################################################################################
## This file is a compilation of all code from Spring 2023 semester of FW5411 ##
################################################################################
# Need to submit in notepad plus plus at the end of the semester #

################################################################################
# Date: 11 JAN 2023 (Wed)     ##################################################
# Week: 1                     ##################################################
# Day: 2                      ##################################################
################################################################################
## Testing calculus ##
1+1
3*67

###### Install packages and load in libraries ######
# install.packages("tidyverse") if not already installed
library(tidyverse)
library(ggplot2)

### Import data from Excel and rename something shorter ###
# Can be done via environment tab #
# Can also convert file to csv and read in that way; Easier than readxl #

## Using data from tidyverse pkg ##
mpg

# Use below if you need help finding out what mpg is and what it tells you #
?mpg

########################### PLOTTING DATA ######################################
### Using mpg data ###
# Plot highway mpg by engine size (displacement) using ggplot #
ggplot(data = mpg) + geom_point(mapping= aes(x = displ, y = hwy))

## Two different ways of viewing the same graph: 1 w/ color, 1 w/ shapes sorted by class ##
ggplot(data = mpg) + geom_point(mapping= aes(x = displ, y = hwy, alpha= class))

ggplot(data = mpg) + geom_point(mapping= aes(x = displ, y = hwy, shape= class))
# Helpful but overwhelming to look at

# Plot city mpg by engine size (displacement) using ggplot #
ggplot(data = mpg) + geom_point(mapping= aes(x = displ, y = cty))

## Two different ways of viewing the same graph: 1 w/ color, 1 w/ shapes sorted by class ##
ggplot(data = mpg) + geom_point(mapping= aes(x = displ, y = cty, alpha= class))

ggplot(data = mpg) + geom_point(mapping= aes(x = displ, y = cty, shape= class))

# Plot miles per gallon on the highway by number of cylinders using ggplot #
ggplot(data = mpg) + geom_point(mapping= aes(x = cyl, y = hwy))

## Two different ways of viewing the same graph as above: One w/ color, one w/ shapes sorted by class ##
ggplot(data = mpg) + geom_point(mapping= aes(x = cyl, y = hwy, alpha= class))

ggplot(data = mpg) + geom_point(mapping= aes(x = cyl, y = hwy, shape= class))

# Plot miles per gallon in the city by number of cylinders using ggplot #
ggplot(data = mpg) + geom_point(mapping= aes(x = cyl, y = cty))

## Two different ways of viewing the same graph as above: One w/ color, one w/ shapes sorted by class ##
ggplot(data = mpg) + geom_point(mapping= aes(x = cyl, y = cty, alpha= class))

ggplot(data = mpg) + geom_point(mapping= aes(x = cyl, y = cty, shape= class))

# Plot miles the type of drive train against the type of car using ggplot #
ggplot(data = mpg) + geom_point(mapping= aes(x = drv, y = class))

################################################################################
### Using week 1 data ###

# Plot mean annual temp against log 10 transformation of biomass #
ggplot(data = dataweek1) + geom_point(mapping= aes(x = MAT, y = log10_BIOM2))

# Make the data points all the same color (blue) #
ggplot(data = dataweek1) + geom_point(mapping= aes(x = MAT, y = log10_BIOM2), color= "blue")

# Box plot of biomass index #
boxplot(dataweek1$log10_BIOM2, main= "Box plot of biomass index")

# Add more variables (site, block, treatment) for week 1 data #
# Denoting each site by color. Helpful but not the best. #
ggplot(data = dataweek1) + geom_point(mapping = aes(x = MAT, y = log10_BIOM2, color = Site))

# Looking at data in terms of treatment #
ggplot(data = dataweek1) + geom_point(mapping = aes(x = MAT, y = log10_BIOM2, color = Treated))

# Looking at based on each block... too confusing, not helpful #
ggplot(data = dataweek1) + geom_point(mapping = aes(x = MAT, y = log10_BIOM2, color = Block))

# Using two separate identifiers to look at the data based on site and treatment. #
# This may be helpful if the symbols were larger #
ggplot(data = dataweek1) + geom_point(mapping = aes(x = MAT, y = log10_BIOM2, color = Site, shape = Treated))

################################################################################
################# Add facets (wrap and grid) to your plots #####################
### Using mpg data ###
# Plot engine displacement against highway mileage, by class type using facet wrap #
ggplot(data= mpg) + geom_point(mapping = aes(y= displ, x= hwy)) + facet_wrap(~ class, nrow=2)

# Plot engine displacement against highway mileage, by drive train and cylinder using facet grid #
ggplot(data= mpg) + geom_point(mapping = aes(y= displ, x= hwy)) + facet_grid(drv ~ cyl)

# Plot engine displacement against highway mileage, by cylinder using facet grid #
ggplot(data= mpg) + geom_point(mapping = aes(y= displ, x= hwy)) + facet_grid(. ~ cyl)

# Plot engine displacement against highway mileage, by drive train using facet grid #
ggplot(data= mpg) + geom_point(mapping = aes(y= displ, x= hwy)) + facet_grid(. ~ drv)

### Using week 1 data ###
# Plot MAT against log 10 trans. biomass, by site using facet wrap #
# View stacked
ggplot(data= dataweek1) + geom_point(mapping = aes(y= MAT, x= log10_BIOM2)) + facet_wrap(~ Site, nrow=2)

# View side-by-side
ggplot(data= dataweek1) + geom_point(mapping = aes(y= MAT, x= log10_BIOM2)) + facet_wrap(~ Site)

# Plot MAT against log 10 trans. biomass by site and treatment, using facet grid #
ggplot(data= dataweek1) + geom_point(mapping = aes(y= MAT, x= log10_BIOM2)) + facet_grid(Site ~ Treated)

################################################################################
################################# SMOOTHING ####################################
### Using mpg data ###
# Plot smoothed data of displacement and hwy mileage, sorted by drive train type #
# Using various separate lines to denote drive type #
ggplot(data= mpg) + geom_smooth(mapping = aes(y= displ, x= hwy, linetype= drv))

# Using various colors to denote drive type #
ggplot(data= mpg) + geom_smooth(mapping = aes(y= displ, x= hwy, color= drv))

### Combine geom smoothing and geom point together into one plot ###
# Plot of displacement against hwy mpg with a smoothed curve #
ggplot(data= mpg) + geom_point(mapping = aes(y= displ, x= hwy)) + 
  geom_smooth(mapping= aes(y= displ, x= hwy))

# Plot of hwy mpg against displacement based on the class of car, with a smoothed line of fit #
ggplot(data= mpg, mapping = aes(x= displ, y= hwy)) + geom_point(mapping= aes(color= class)) +
  geom_smooth(dta= filter(mpg, class== "subcompact"), se= FALSE)


### Using week 1 data ###
# Plot smoothed data of MAT and biomass index. #
ggplot(data = dataweek1) + 
  geom_smooth(mapping = aes(x = MAT, y = log10_BIOM2))

# Plot smoothed data of MAT and biomass index by treatment (yes or no) #
ggplot(data = dataweek1) + 
  geom_smooth(mapping = aes(x = MAT, y = log10_BIOM2, linetype = Treated))

# Plot smoothed data of MAT and biomass index by site (Ford, Kellogg) #
ggplot(data = dataweek1) + 
  geom_smooth(mapping = aes(x = MAT, y = log10_BIOM2, linetype = Site))

# Plot smoothed data of MAT and biomass index by site, without legend #
ggplot(data = dataweek1) + 
  geom_smooth(mapping = aes(x = MAT, y = log10_BIOM2, group = Site), show.legend = FALSE)

# Plot smoothed data of MAT and biomass index by treatment, without legend #
ggplot(data = dataweek1) + 
  geom_smooth(mapping = aes(x = MAT, y = log10_BIOM2, group = Treated), show.legend = FALSE)

# Plot smoothed data of MAT and biomass index by various color for species (3 species) #
ggplot(data = dataweek1, mapping = aes(x = MAT, y = log10_BIOM2)) +
  geom_point(aes(color = Species)) +
  geom_smooth()

# Plot smoothed data of MAT and biomass index for species by treatment #
ggplot(data = dataweek1, mapping = aes(x = MAT, y = log10_BIOM2)) +
  geom_point(aes(color = Species)) +
  geom_smooth(mapping = aes(group = Treated))

# Plot smoothed data of MAT and biomass index for species by site #
ggplot(data = dataweek1, mapping = aes(x = MAT, y = log10_BIOM2)) +
  geom_point(aes(color = Species)) +
  geom_smooth(mapping = aes(group = Site))

############################# End of Day 2 #####################################

############################## Start Day 3 #####################################
################################################################################
# Date: 13 JAN 2023 (Fri)     ##################################################
# Week: 1                     ##################################################
# Day: 3                      ##################################################
################################################################################
# Load in necessary libraries #
library(tidyverse)
# Still having issues with this loading... #
# finally got it! #

library(ggplot2)
library(dplyr)
library(nycflights13)

################################################################################
########################### From Ch. 4- Coding basics ##########################

# Assigning a value to "x" #
x <- 3*4

# Print value of "x" in the console #
x

# Assigning a value to another name... a long one #
# Best to name things shorter than this. Easier to type and view the code #
this_is_a_really_long_name <- 2.5

# Print the value associated with that name #
this_is_a_really_long_name

# Ordering the sequence, in order from 1-10 #
seq(1, 10)
seq(1,5)
seq(1,50)

################################################################################
######################## From Ch. 5- Data Transformations ######################

# Investigating the tibble of 'flights' #
# A tibble is a different type of data frame. It only shows the first 10 rows #
library(nycflights13)

# Shows the tibble of flights in the console #
flights

# Shows the data frame of flights in the data viewer that pops up in the script pane #
View(flights)

# Provides more information on the "flights" data #
?flights

################################################################################
###### Ch.5.2- Using the filter function for data manipulation "filter()" ######
?filter

# Filter flights that occurred on Jan 1, 2013 ONLY #
filter(flights, month == 1, day == 1)
# == mean "equals", so above we are looking for only month of Jan and day 1 #

# Save as new data frame "df" #
jan1 <- filter(flights, month == 1, day == 1)
# Went from 1372 obs of 22 variables to 842 obs of 19 variables after filter

# Find flights that left during either November or December of 2013 #
filter(flights, month == 11 | month == 12)

# Can also use piping to simplify #
filter(flights, month %in% c(11, 12))

# Name nov_dec #
nov_dec <- filter(flights, month %in% c(11, 12))
# 55403 obs of 19 variables; a much bigger set of data that jan1 #

# Find flights that left before 6 am #
filter(flights, dep_time < 6)

################################################################################
################# Ch.5.3- Using the arrange function  "arrange()" ##############
?arrange

# arrange flights by year, then month, then day #
arrange(flights, year, month, day)

# Arrange in descending order by departure delays #
arrange(flights, desc(dep_delay))

################################################################################
################# Ch.5.4- Select specific columns using "select()" #############
?select

# Select columns year, month, day #
select(flights, year, month, day)
# Use : to select everything from year through day #
select(flights, year:day)

# Select every column except year, month, day #
select(flights, -(year:day))

# Rename is a variant of "select()"#
# changes the name of 1 column while keeping others the same
rename(flights, tail_num = tailnum)

# "select()" can rearrange the order of columns #
# Can be helpful when viewing data frames, etc. in your own data #
select(flights, time_hour, air_time, everything())

################################################################################
################################################################################
# In-class Exercises #
### Using week 1 data ###
dataweek1

# Exercise 1a: Make a df that only contains Qe (filter)
Qe <- filter(dataweek1, Species == "Qe")

# Exercise 1b: Save the Qe df as"Qe_data"
Qe_data <- Qe
# How many samples (rows) are in Qe
# A: There are 190 rows in Qe data

# Exercise 2" Sort (arrange) Qe by "Height"
arrange(Qe_data, desc(Height))
# What is the height of the three tallest plants?
# A: 188, 175, 155 cm

# Exercise 3: From Dataweek1, select (filter) all plants that are between 100 cm and 200 cm tall, save as "Height_select"
Height_select <- filter(dataweek1, Height > 100 | Height < 200)
# Looking for any Qe or Qa in new df #
arrange(Height_select, Species)
totalQa <- filter(Height_select, Species == "Qa")
totalQe <- filter(Height_select, Species == "Qe")

# Are there any Qe or Qa in the new df, if so, how many?
# A: Yes there are 190 observations of species Qe and 33 observations of species Qa

# Exercise 4: Make a new df that contains the following columns: ID, Species, Height, stem, Crown, save as "Biom_raw"
biom_raw <- select(Height_select, ID, Species, Height, stem, Crown)

# Exercise 5a: From Dataweek1, make a scatterplot (geom_point) of CD1 vs CD2
ggplot(data = dataweek1) + geom_point(mapping = aes(x = CD1, y = CD2))

# Exercise 5b: Add trendlines (geom_smooth) for each species (Qr, Qe, Qa) and color these trendlines differently
ggplot(data = dataweek1) + 
  geom_point(mapping = aes(x = CD1, y = CD2)) +
  geom_smooth(mapping = aes(x = CD1, y = CD2, color = Species))

################################# End Day 3 ####################################

################################ start Day 4 ###################################
################################################################################
# Date: 18 JAN 2023 (Mon) ######################################################
# Week: 2                 ######################################################
# Day: 4                  ######################################################
################################################################################

# Load in packages #
library(tidyverse) # To include ggplot2 and dplyr
library(dplyr) # Sometimes ggplot and dpyl don't load correctly into this version
library(ggplot2)
library(nycflights13)


# Ch. 5.5- Add variables to the data set using mutate()
?mutate

View(flights)

# Make smaller data set to work with using flights data #
flights_sml <- select(flights, 
                      year:day, 
                      ends_with("delay"), 
                      distance, 
                      air_time)

View(flights_sml)

# Add two columns, one for the time gained during flight 'gain' and one for average 'speed' 
mutate(flights_sml,
       gain = dep_delay - arr_delay,
       speed = distance / air_time * 60)
# Example of adding columns based on what you already have

# A new tibble with just the new columns is created with transmute()
transmute(flights,
          gain = dep_delay - arr_delay,
          hours = air_time / 60,
          gain_per_hour = gain / hours)
# Helpful for checking what you did was correct/worked the way you expected

################################################################################
# Exercise 5.5.2 (1)
# Convert dep_time and sched_dep_time to easier to read #s to represent minutes since midnight
transmute(flights, dep_time, dep_hour = dep_time %/% 100, 
          dep_min = dep_time %% 100)
################################################################################

# Ch. 5.6- Data summary with summarise()
?summarise

# The mean departure delay in 2013 flights #
summarise(flights, delay = mean(dep_delay))

summarise(flights, delay = mean(dep_delay, na.rm = TRUE)) # Removing any NA values in order to get the mean

# Departure delay for each day (using group_by first) #
by_day <- group_by(flights, year, month, day)
summarise(by_day, delay = mean(dep_delay, na.rm = TRUE))

# Exercise 5.6.1- Combining multiple operations with the pipe %>% #

# In 3 separate steps:
# Grouping by destination
by_dest <- group_by(flights, dest)

# Summarizing based on destination
delay <- summarise(by_dest,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE))

# Filtering flights < 20 going to a specific destination #
# Looking at flights from NYC in terms of average delay and distance #
# Size of the circle is relative to # of flights #
delay <- filter(delay, count > 20, dest != "HNL")
ggplot(data = delay, mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE)

# Pipe to avoid intermediate data frames (shorten your code!)
delays <- flights %>% 
  group_by(dest) %>% 
  summarise(
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)) %>% 
  filter(count > 20, dest != "HNL")
delays <- filter(delay, count > 20, dest != "HNL")
ggplot(data = delay, mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE)

# Save non-cancelled flights as new df #
not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))
# Any missing data is excluded! #
# ! means "is not" #

# No more need of the na.rm = TRUE command #
# This is because we excluded the missing data in lines 94-95 #
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay))

# Ch. 5.6.3- Counts #
# Looking at arrival delay using the tail number of the plane #
delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay))

ggplot(data = delays, mapping = aes(x = delay)) + 
  geom_freqpoly(binwidth = 10)
# Bin width smooths out the curve... The wider the bin width, the less noise you'll have #

# Delays from all non-cancelled by tail number #
delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay),
    n = n())

ggplot(data = delays, mapping = aes(x = n, y = delay)) + 
  geom_point(alpha = 1/10)
# The greater # of flights a tail number has, the closer to 0 minutes of delays it will have #
# More noise in the lower n. Could set a filter and only look at flights with greater than x flights per year #

# Filter out the extremes #
# Filtering out any flights that had less than 25 flights in a year. #
# This will help reduce the noise on the low end of the x-axis #
delays %>% 
  filter(n > 25) %>% 
  ggplot(mapping = aes(x = n, y = delay)) + 
  geom_point(alpha = 1/10)
# This plot looks much different and has less noise that previous. 
# Here we can see that planes with > 25 flights per year are more on time #

# Baseball example
#install.packages("Lahman") # Already installed, but leave here for future reference. 
library(Lahman)
batting <- as_tibble(Lahman::Batting)
?Lahman

batters <- batting %>% 
  group_by(playerID) %>% 
  summarise(
    ba = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE),
    ab = sum(AB, na.rm = TRUE)
  )
# ba= batting average, ab= at bats
batters %>% 
  filter(ab > 2) %>% 
  ggplot(mapping = aes(x = ab, y = ba)) +
  geom_point() + 
  geom_smooth(se = FALSE)
# Distribution of batting averages against at-bats #

# Graph of players with more than 100 at-bats #
# See how much it changes form the previous graph #
# Higher ba= more at-bats... for a good reason #
batters %>% 
  filter(ab > 100) %>% 
  ggplot(mapping = aes(x = ab, y = ba)) +
  geom_point() + 
  geom_smooth(se = FALSE)

# Ch. 5.6.5- Grouping by multiple variables
daily <- group_by(flights, year, month, day)
(per_day   <- summarise(daily, flights = n())) # Counts all flights in this category
(per_month <- summarise(per_day, flights = sum(flights)))
(per_year  <- summarise(per_month, flights = sum(flights)))

# Ch.5.6.6- Ungrouping 
daily %>% 
  ungroup() %>%             
  summarise(flights = n())

# Ch.5.7- Grouped mutates and filters
flights_sml %>% 
  group_by(year, month, day) %>%
  filter(rank(desc(arr_delay)) < 10)

################################################################################
########################## Ch.6- Workflow: Scripts #############################
# Start each script with required packages
# library()
# NEVER add setwd() or install.packages into your final, shared scripts. Not good practice! #

################################################################################
######################## Ch.7- Exploratory data analysis #######################
#req package(s): tidyverse
library(tidyverse)
library(dplyr)
library(ggplot2)

# Barchart of diamond cut type
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))

# Or count them by cut type
diamonds %>% 
  count(cut)

# Use a histogram for continuous variables:
ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = carat), binwidth = 0.5)

# Or count:
diamonds %>% 
  count(cut_width(carat, 0.5))
# The majority of diamonds are ~1 carat in this data set #
# Very few are either very small or very large #

# Exploring various bin sizes #
ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = carat), binwidth = 0.1) # Very detailed 

ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = carat), binwidth = 1.0) # Too broad

# Explore bin width with diamonds < 3 carat
# Creating a smaller data set
smaller <- diamonds %>% 
  filter(carat < 3)

ggplot(data = smaller, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.1)

# Multiple histograms can be depicted with freqpoly() #
ggplot(data = smaller, mapping = aes(x = carat, colour = cut)) +
  geom_freqpoly(binwidth = 0.1)
# Basically 5 histograms on top of one another for the quality of the cut 

# Find 3 questions from this graph:
ggplot(data = smaller, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.01)
# Q1: Why are there so many more peaks below 1.0 carats compared to the 1-2 carat range?
# Q2: How does the cut quality affect the number of diamonds at each carat weight?
# Q3: What producers have the most carat diamonds between 0-1.0 carat range?

# Questioning eruption times of the Old Faithful geyser #
ggplot(data = faithful, mapping = aes(x = eruptions)) + 
  geom_histogram(binwidth = 0.25)
# Binomial dist. of eruption times
# Higher frequency between 4-5 hours than 0-3 hours #

# Outlier detection
# Size dist. in millimeters
ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = y), binwidth = 0.5)

# To help, we zoom in:
# We do this by limiting what portion of the graph we are observing "ylim" #
ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = y), binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50))
# ~9 diamonds as outliers

# Extract the outliers with dplyr:
unusual <- diamonds %>% 
  filter(y < 3 | y > 20) %>% 
  select(price, x, y, z) %>%
  arrange(y)
# Smaller than 3 mm and larger than 20 mm #
unusual
# Likely these are data entry errors and need to be excluded #
# Always state which data points were excluded, or replace values with "NA" #

################################ End day 4 #####################################

############################# Start days 5-6 ###################################

################################################################################
# Date: 20 JAN 2023 (Mon) and 22 JAN (Wed) #####################################
# Week: 2                   ####################################################
# Day: 5-6                  ####################################################
################################################################################
### Using Oak data: Please work in groups of 3 ###
# Load in libraries
library(tidyverse)
library(ggplot2) # If tidyverse doesn't load correctly
library(dplyr)

#1. Create a new column using mutate with a biomass index (call it biom3) using at least 3 out of 4 from CD1, CD2, Height, stem
oak_data <- mutate(dataweek1,
                   biom3 = (CD1+CD2)*(stem/Height))
#1.b) Compare your biom3 with biom2 using geom_point and geom_smooth - do you have a good correlation?
ggplot(data = oak_data, mapping = aes(x=BIOM2, y=biom3)) +
  geom_point() +
  geom_smooth()

## Yes, there is some positive correlation, although the relationship doesn't look linear
## Potentially some outliers

#1.c) Add another column with mutate with the log2 of your biom3 data
oak_data <- mutate(oak_data,
                   log2_biom3 = log2(biom3))

#1.d) compare log_biom2 with your log2_biom3 (as above) and discuss the correlation 

ggplot(data = oak_data, mapping = aes(x=log10_BIOM2, y=log2_biom3)) +
  geom_point() +
  geom_smooth()

## Yes, there IS correlation as suspected. However, the data appears left-skewed 
## Logging the data made the relationship more linear, which we would expect


#2. Use summarize to get the mean height of trees
?summarize()
oak_data %>%
  summarize(meanHeight=mean(Height))
## The mean height of all trees in 91.1 cm


#2.b) group mean height by species and by population
heightmeans <-oak_data %>%
  group_by(Species, Population) %>%
  summarize (meanHeight=mean(Height))


#3. Provide the interquartile range of 
#a) biom2
summarize(oak_data, IQR(BIOM2)) 
# IQR 1361

#b) log_biom2
summarize(oak_data, IQR(log10_BIOM2)) 
# IQR 0.480

#c) biom3
summarize(oak_data, IQR(biom3)) 
# IQR 17.7

#d) log2_biom3
summarize(oak_data, IQR(log2_biom3)) 
# IQR 0.999

#4. Use the count function to find how many trees per population there are
?count()
oak_data %>%
  group_by(Population) %>%
  count()
# This is a relatively large tibble to list # of trees per pop'ln. #

#5. Make a bar chart for number of trees per species
ggplot(data = oak_data) +
  geom_bar(mapping = aes(x=Species, fill = Species))
# Each species is a different color to help visualize a bit better

#5.b) Make a boxplot for height by species
?boxplot
ggplot(oak_data, aes(x=Species, y=Height, fill=Treated))+
  geom_boxplot()
# We added separation by treatment (yes or no)

#5.c) Make a histogram with height. Bin by 1, 5, 10 What is the difference?
# Bin wdth 1.0
ggplot(data = oak_data) +
  geom_histogram(mapping = aes(x = Height), binwidth = 1.0) 
# Lots of noise, difficult to see real, gross trends in the data at this width

# Changing the bin width to 5.0
ggplot(data = oak_data) +
  geom_histogram(mapping = aes(x = Height), binwidth = 5.0)
# Easier to see some trends and distribution of the data

# Changing the bin width to 10
ggplot(data = oak_data) +
  geom_histogram(mapping = aes(x = Height), binwidth = 10)
# You can see the distribution shape better with a larger bin width.
## The larger the bin width = less noise (The graph looks smoother). 

#5.d) Separate height distribution by Species using geom_freqpoly
ggplot(data = oak_data, mapping = aes(x = Height, color= Species)) +
  geom_freqpoly(binwidth = 10)

#6 You noted that some trees at the Ford site suffered freezing damage, in some cases so severe that the lead shoot died. 
# You speculate that southern, warmer provenances had more frost damage. Because the southern provenances grow generally faster, 
# it is difficult to compare height directly. What else could you compare between northern and southern? Make some graphs and 
# formulate questions/hypothesis iteratively.
# COMPARISONS: We could compare many things between the north and south sites, such as...
# CD1, CD2, Stem width/thickness, total crown diameter
# If southern sites are taller at first (pre-frost damage), comparing height only is not useful
# We could compare crown indices (Crown) versus MAT for FORD SITE only

# Just for Ford, compare MAT and Crown, but create new df for Ford site first.
ford <- oak_data %>% filter(Site== "F")

# MAT versus height divided by H_crown
ggplot(data = ford, mapping = aes(x= MAT, y= H_crown)) +
  geom_point() + geom_smooth(method = "lm")
# MAT and H_crown have a relatively weak negative linear relationship. Regression would be better here

# MAT versus stem
ggplot(data = ford, mapping = aes(x= MAT, y= stem)) +
  geom_point() + geom_smooth(method = "lm")
# MAT and stem have a relatively weak negative linear relationship. Regression would be better here

oak_data %>% 
  ggplot(aes(x= MAT, y= H_crown)) + 
  geom_point(aes(color = Species)) + geom_smooth() +
  facet_grid(~Site)


#7 You are interested in the sources of variation in the experiment. How much variation resides within families, populations, between regions?
?var
?cov

# Variance of indv. families and height
ggplot(data = oak_data) + geom_violin(aes(x= Height, y= Family))

################################## END DAYS 5-6 ################################

################################## Start day 7 #################################
################################################################################
# Date: 25 JAN 2023 (Wed)     ##################################################
# Week: 3                     ##################################################
# Day: 7                      ##################################################
################################################################################
## Load in libraries ##
library(tidyverse)
library(modelr)
library(nycflights13)

# 7.4 Missing values

# Creating a new data set with y between 3 and 20
# Using the diamond data set
diamonds2 <- diamonds %>% 
  filter(between(y, 3, 20))

# This works better ("ifelse()" replaces values based on specified criteria) 
diamonds2 <- diamonds %>% 
  mutate(y = ifelse(y < 3 | y > 20, NA, y))

# Scatter plot of new df "diamonds" of length/width values between 3 and 20 mm
# x= length in mm; y= width in mm
# Adding a red trendline
ggplot(data = diamonds2, mapping = aes(x = x, y = y)) + 
  geom_point() + geom_smooth(method = "lm", color= "red")

# OR, you can exclude y values that were <3mm or >20mm with NA
# This will show you data that is likely not a mistake on the y-axis
# We still have this potential issue on the x-axis
ggplot(data = diamonds2, mapping = aes(x = x, y = y)) + 
  geom_point(na.rm = TRUE) + geom_smooth(method = "lm", color= "red")

################################################################################
############################### 7.5 Covariation ################################

# Boxplot of price by diamond cut to observe price distribution
ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
  geom_boxplot()
# Idela is cheapest? Could be due to cut, clarity, SIZE

# Reorder boxplots by value, e.g. median
ggplot(data = diamonds, mapping = aes(x = reorder(cut, price, FUN = median), y = price)) +
  geom_boxplot()

# You can flip graph by 90 degrees. I like this view better.
ggplot(data = diamonds, mapping = aes(x = reorder(cut, price, FUN = median), y = price)) +
  geom_boxplot() +
  coord_flip()

# Why are the highest quality diamonds the cheapest? Exploring w/ covariation.
# Covariation between two continuous variables (various visualizations)
ggplot(data = diamonds) +
  geom_point(mapping = aes(x = carat, y = price))

ggplot(data = diamonds) + 
  geom_point(mapping = aes(x = carat, y = price), alpha = 1 / 100)
# Making data set "smaller"

smaller <- diamonds %>% filter(carat < 3)
ggplot(data = smaller) +
  geom_bin2d(mapping = aes(x = carat, y = price))

ggplot(data = smaller, mapping = aes(x = carat, y = price)) + 
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.1)))

################################################################################
############################ 7.6 Patterns and models ###########################
# From observations, you can formulate questions
# Old faithful eruption length and time between eruptions
ggplot(data = faithful) + 
  geom_point(mapping = aes(x = eruptions, y = waiting))

# What do you see?
# The longer you wait, the more eruptions of Old Faithful you are likely to see

# Investigating further the connection between diamond price and cut quality
library(modelr)

# Transforming price and carat with log transformation calling it "mod"
# "lm" stands for linear model
# Creating a linear model with eveyrthing added afterwards
mod <- lm(log(price) ~ log(carat), data = diamonds)

# Creating a new df using the transformed price and carat values
# Adding residuals from the model. (Exponential residuals)
# Residuals = the 
# Smaller residuals = the better your model is
# Larger residuals = the worse your model is
# ** NOTE that residuals are in the units your data is in. If your values in 
# your data are small, your residuals will likely be small.
diamonds2 <- diamonds %>% 
  add_residuals(mod) %>% 
  mutate(resid = exp(resid))

# Scatter plot of transformed data 
ggplot(data = diamonds2) + 
  geom_point(mapping = aes(x = carat, y = resid))

ggplot(data = diamonds2) + 
  geom_boxplot(mapping = aes(x = cut, y = resid))

################################################################################
##################### 7.7 Shortening code with ggplot ##########################
### Using Old Faithful data ###

# Instead of using this:
ggplot(data = faithful, mapping = aes(x = eruptions)) + 
  geom_freqpoly(binwidth = 0.25)

# You can write:
ggplot(faithful, aes(eruptions)) + 
  geom_freqpoly(binwidth = 0.25)

# Ch. 9: Introduction (no code for this)
# Ch. 10: Tibbles (no code for this, already did)
################################################################################
# Ch. 11: Data import
?readr
?read_csv # Don't use csv2 because this will do semicolon delimited rather than comma delimited

################################# End Day 7 ####################################
############################# Start days 8-9 ###################################
################################################################################
# Date: 30 JAN 2023, 1 FEB (Mon, Wed)  #########################################
# Week: 4                     ##################################################
# Day: 8-9                    ##################################################
################################################################################
## Linear Regression ##
# Load in packages #
library(tidyverse)
library(ggplot2)

# Wing length (cm) (y) versus age (x) in days in 13 sparrows #
# wing length is dependent, age independent. #
wing_data <- tibble(
  days = c(3, 4, 5, 6, 8, 9, 10, 11, 12, 14, 15, 16, 17),
  wing_length = c(1.4, 1.5, 2.2, 2.4, 3.1, 3.2, 3.2, 3.9, 4.1, 4.7, 4.5, 5.2, 5.0)
)

wing_data

# Plotting days against wing length
ggplot(wing_data) +
  geom_point(aes(x = days, y = wing_length)) 

# Plotting days against wing length with geom_smooth. This adds a line to the plot 
ggplot(wing_data) +
  geom_point(aes(x = days, y = wing_length)) +
  geom_smooth(aes(x = days, y = wing_length))

# Calculate sums of X, Y, X2, Y2, XY
?sum
n = 13  # Number of observations

# Calculating x sum; x sum = 130
SumX <- sum(wing_data$days)

# Calculating y sum; y sum= 44.4
SumY <- sum(wing_data$wing_length)

# Calculating x^2 sum; X^2 sum= 1562
SumX2 <- sum(wing_data$days^2)

# Calculating y^2 sum; y^2 sum = 171.3
SumY2 <- sum(wing_data$wing_length^2)

# Calculating xy sum; xy sum= 514.8
SumXY <-  sum(wing_data$days*wing_data$wing_length)

# Calculate beta (slope) = 0.270229
beta <- (((13*514.8)-(130*44.4))/((13*1562)-130^2))

# Calculate alpha (intercept) = 0.7130946
alpha <- ((1/13)*44.4)-(0.270229*(1/13)*130)

# Make a graph with both scatter plot and regression line using geom_abline
?geom_abline
ggplot(wing_data) +
  geom_point(aes(x = days, y = wing_length)) +
  geom_abline(aes(intercept = 0.7130946, slope = 0.270229)) # Inputting our intercept and slope base don the model

# Calculate the squared standard error
SSE <- (1/(13*(13-2)))*((13*171.3)-(44.4^2)-(0.270229^2)*((13*1562)-(130^2)))

# Calculate the squared standard error of beta; = 0.0001820649
SSEbeta <- ((n*SSE)/((n*SumX2)-((SumX)^2)))

# Calculate the squared standard error of alpha; = 0.0218758
SSEalpha <- (SSEbeta)*(1/n)*(SumX2)

# Calculate the product-moment correlation coefficient r; = 0.9865931
r <-((n*SumXY)-(SumX*SumY)) / (sqrt(((n*SumX2)-(SumX^2))*((n*SumY2)-SumY^2)))

# Calculate r2; r2= 0.9733659 (0.9865931 * 0.9865931)
r2 <- ((r)^2)    

# Calculate the 95% confidence interval
CIpos <- beta + 2.201 * sqrt(beta)
CIneg <- beta - 2.201 * sqrt(beta)

# t*11 = 2.201

# Sbeta = sqrt of squared standard error of beta

################################################################################
# Linear regression function in R using lm (linear model)
?lm
# Always your response variable before the tilde in the model. See below.
lm1 <- lm(wing_length ~ days, data = wing_data)
summary(lm1)
# Find the values you have calculated before (b, a, SE, bE, aE, r2)
# What other information does the summary contain?

# create a plot with data and regression line
plot(wing_data) # Using base R. gpglot will give better graphs, and ggpubr even better 
abline(lm1)

# Plot residuals - these should be randomly distributed
plot(lm1$residuals)

# Overview of some standard model parameters
par(mfrow = c(2,2)) # Puts four plots in frame rather than just one
plot(lm1) # WAY easier than trying to plot each of these indv. 

# Top left: Should be a straight line. Need more values
# Top right: Should be a linear line going through 0
# Bottom left: Uses a transformation... should also be straight


par(mfrow = c(1,1)) # Back to 1 figure at a time
plot(lm1)

# Let us create a new tibble that contains a single outlier wing span data point
# due to an data entry error
wd2 <- tibble(
  days = c(3, 4, 5, 6, 8, 9, 10, 11, 12, 14, 15, 16, 17),
  wing_length = c(14, 1.5, 2.2, 2.4, 3.1, 3.2, 3.2, 3.9, 4.1, 4.7, 4.5, 5.2, 5.0)
)

# Rerun linear regression, summarize, and plot
lm2 <- lm(wing_length ~ days, data = wd2)
summary(lm2)

plot(wd2)
abline(lm2)

# Find outlier(s) with the cooks.distance function
plot(cooks.distance(lm2)) # Could we also use a Runs Test?

# Data point 1 is an outlier, we can remove the point by:
wd2[1, 2] = NA
wd2 # View the tibble and make sure your code ran correctly

# Re-run the model and plot
lm2 <- lm(wing_length ~ days, data = wd2)
summary(lm2)

plot(wd2)
abline(lm2)
#################################### END DAYS 8-9 ##############################
################ Worked on Final Project for the rest of Day 9 #################
########################## See Final Project code ##############################

############################### Start Day 10 ###################################
################################################################################
# Date: 6 FEB 2023 (Mon)      ##################################################
# Week: 5                     ##################################################
# Day: 10                     ##################################################
################################################################################

# Load in packages #
library(tidyverse)
library(ggplot2)

# Load in Royer et al. 215 data set as a tibble #
# short_data for 1st half of class
# long_data for 2nd half of class

# Investigate distribution of variables (e.g. boxplot, histogram) from short_data
par(mfrow = c(2,2)) # doesn't work with ggplot? 

# Box plot and histogram for MAT #
# Using base R language
par(mfrow= c(1,1))
boxplot(short_data$MAT, short_data$Margin_perc, short_data$Teeth_N, short_data$Tooth_area)

# Using gpglot
ggplot(data = short_data, mapping = aes(x= MAT)) +
  geom_boxplot()
ggplot(data = short_data, mapping = aes(x= MAT)) +
  geom_histogram(binwidth = 0.25)

# Box plot and histogram for MARGIN PERCENTAGE #
ggplot(data = short_data, mapping = aes(x= Margin_perc)) +
  geom_boxplot()
ggplot(data = short_data, mapping = aes(x= Margin_perc)) +
  geom_histogram(binwidth = 0.25)

# Box plot and histogram for NUMBER OF TEETH #
ggplot(data = short_data, mapping = aes(x= Teeth_N)) +
  geom_boxplot()
ggplot(data = short_data, mapping = aes(x= Teeth_N)) +
  geom_histogram(binwidth = 0.25)

# Box plot and histogram for TOOTH AREA #
ggplot(data = short_data, mapping = aes(x= Tooth_area)) +
  geom_boxplot()
ggplot(data = short_data, mapping = aes(x= Tooth_area)) +
  geom_histogram(binwidth = 0.25)


# Produce scatter plot of MAT each of the 3 explanatory variables
# Scatter plot for MARGIN PERCENTAGE
ggplot(data = short_data, mapping = aes(x = MAT, y = Margin_perc)) + 
  geom_point() + geom_smooth(method = "lm", color = "Brown")

# Scatter plot for NUMBER OF TEETH
ggplot(data = short_data, mapping = aes(x = MAT, y = Teeth_N)) + 
  geom_point() + geom_smooth(method = "lm", color = "Black")

# Scatter plot for TOOTH AREA
ggplot(data = short_data, mapping = aes(x = MAT, y = Tooth_area)) + 
  geom_point() + geom_smooth(method = "lm", color = "Blue")

########## MODELS ############# MODELS ################ MODELS #################
################################################################################
######################### MODEL 1: MARGIN PERCENTAGE ###########################

# Build linear regression models and note alpha, beta, adjusted R2
lm_Margin_perc <- lm(Margin_perc ~ MAT, data = short_data)
summary(lm_Margin_perc)
# Residuals are not great, and we want the median around 0. Model is SIG. 
# Adj. R2= 0.7904
# Alpha (intercept)= -9.2950
# Beta (slope)= 3.2180

# Plot the regression line with the scatter plot
ggplot(data = short_data, mapping = aes(x = MAT, y = Margin_perc)) + 
  geom_point() + geom_smooth(method = "lm", color = "Brown", fill= "Brown") +
  geom_abline(slope= 3.218, intercept= -9.295)
# Positive relationship

################################################################################
######################### MODEL 2: NUMBER OF TEETH #############################

# Build linear regression models and note alpha, beta, adjusted R2
lm_Teeth_N <- lm(Teeth_N ~ MAT, data = short_data)
summary(lm_Teeth_N)
# Residuals are not great again, and we want the median around 0. Model is SIG. 
# Adj. R2= 0.7739
# Alpha (intercept)= 75.8621
# Beta (slope)= -1.8755

# Plot the regression line with the scatter plot
ggplot(data = short_data, mapping = aes(x = MAT, y = Teeth_N)) + 
  geom_point() + geom_smooth(method = "lm", color = "Black", fill= "Black") +
  geom_abline(slope= -1.8755, intercept= 75.8621)
# Negative relationship

################################################################################
######################### MODEL 3: TOOTH AREA ##################################

# Build linear regression models and note alpha, beta, adjusted R2
lm_Tooth_area <- lm(Tooth_area ~ MAT, data = short_data)
summary(lm_Tooth_area)
# Residuals are not great again, and we want the median around 0. Model is SIG. 
# Adj. R2= 0.429
# Alpha (intercept)= 2.04437
# Beta (slope)= -0.06224

# Plot the regression line with the scatter plot
ggplot(data = short_data, mapping = aes(x = MAT, y = Tooth_area)) + 
  geom_point() + geom_smooth(method = "lm", color = "Blue", fill= "Blue") +
  geom_abline(slope= -0.06224, intercept= 2.04437)
# Negative relationship

### Conclusions: Our data and modeling is similar to that of the papers/authors. 

################################################################################
#################### USING THE FULL DATA SET "long_data" #######################
library(dplyr)

# Produce summary statistics of teeth1, MAT, ToothArea grouped by Site
long_data %>% group_by(Site) %>% summarize(mean = mean(ToothArea, na.rm = TRUE))
long_data %>% group_by(Site) %>% summarize(mean = mean(teeth1, na.rm = TRUE))
long_data %>% group_by(Site) %>% summarize(mean = mean(MAT, na.rm = TRUE))

# Produce linear regression model and plot (use geom_jitter) with Dataweek5 and tooth_area and teeth2
lm_TA <- lm(ToothArea ~ MAT, data = long_data)
summary(lm_TA)
# Residuals are not great...
# Adj. R2= 0.009323
# Alpha (intercept)= 1.69387
# Beta (slope)= -0.03755

# 
ggplot(long_data) + geom_jitter(aes(x= MAT, y= ToothArea)) +
  geom_abline(intercept = 1.69387, slope = -0.03755, color = "purple", lwd= 1.0)


# Compare alpha, beta, R2, and p-values between the linear regression models with site means to all data
# Alpha= Similar enough
# Beta= Similar enough
# R2= LARGELY different between raw data and the means... must have been some filtering
# p-values= similar enough

################################ END DAY 10 ####################################
############################## START DAY 11 ####################################

################################################################################
# Date: 8 FEB 2023 (Wed)      ##################################################
# Week: 5                     ##################################################
# Day: 11                     ##################################################
################################################################################

# Load in packages #
library(performance)
library(ggstatsplot)
library(jtools)
library(ggstance)
library(broom.mixed)

# Build 'All' multiple linear regression model:
# This includes all variables. We will prune down later
lm_All <- lm(MAT ~ Shape + FerDiam + TA_BA + Teeth_N_IntPeri, data = datashort)
summary(lm_All)
check_model(lm_All) # SO NICE!!! Useful graph

# Build the fossil multiple linear regression model:
lm_foss <- lm(MAT ~ Margin_perc + PeriRatio + TA_BA, data = datashort)
summary(lm_foss)
check_model(lm_foss)
# Residuals good, adj R2 good

# Build a global model containing all variables
lm_glob <- lm(MAT ~ ., data = datashort)
summary(lm_glob)
# Residuals good, adj R2 good too

# Do a stepwise model selection using 'step'
lm_glob <- step(lm_glob, trace = FALSE)

# Check lowest AIC model
summary(lm_glob)
# Residuals not as good, but adj R2 good (better than others)
check_model(lm_glob)

### Looking at each of the different models ###
## Gives AIC, BIC ##
## Can compare ecah of your three models using this tool ##
ggcoefstats(lm_glob)
ggcoefstats(lm_All)
ggcoefstats(lm_foss)

# Compare models lm_All, lm_foss and lm_glob
plot_summs(lm_All, lm_glob, lm_foss, omit.coefs = NULL)

# Compare three models with anova
anova(lm_glob, lm_All, lm_foss) # marginally better
anova(lm_foss, lm_All, lm_glob)
## No major differences between our models ##
## Floss model may be the best, but it has the fewest variables, so it
# will likely be better due to that. 

lm_bad <- lm(MAT ~ PeriRatio + Shap, data = datashort)
summary(lm_bad)
# adj R2 not great (I've had models worse than this...)

anova(lm_bad, lm_glob) # global is better, but not by as much as you'd expect 
anova(lm_bad, lm_All, lm_glob, lm_foss)

#### How to interpret the ANOVA matrices with line 60
# Says that model 2 has a greater likelihood of being more significant than model 1

###################### END DAY 11- WORKED ON FINAL PROJECT #####################

############################## START DAY 12 ####################################
################################################################################
# Date: 13 FEB 2023 (Wed)     ##################################################
# Week: 5                     ##################################################
# Day: 12                     ##################################################
################################################################################

# Load in libraries/pkgs #
library(tidyverse)

# Add BIOM3 to Dataweek1
Dataweek6 <- Dataweek1 %>%
  mutate(BIOM3 = BIOM2^2,
         BIOM4 = CD1 * CD2 * Height)

##### Visualize the data with plots #####
# Scatter of BIOM2 and BIOM3
ggplot(Dataweek6) +
  geom_point(aes(x = BIOM2, y = BIOM3))

# Scatter of BIOM2 and BIOM4
ggplot(Dataweek6) +
  geom_point(aes(x = BIOM2, y = BIOM4))

# Scatter of BIOM3 and BIOM4
ggplot(Dataweek6) +
  geom_point(aes(x = BIOM3, y = BIOM4))

# These look different because one variable is transformed differently than the other
# Also, these do not look really natural in term of what our data set is... 

# How do you describe these relationships?
lm1 <- lm(BIOM2 ~ BIOM3, data = Dataweek6)
summary(lm1)
## The residuals for these do NOT look good... Median not close to 0
## The adj R2 is okay, but could be better

lm2 <- lm(BIOM2 ~ BIOM4, data = Dataweek6)
summary(lm2)
## These residuals are a little better and median closer to 0
## Adj R2 is better with the other model, so lm1 is a better fit

# Scatter of BIOM2 and BIOM3 with alpha and beta added
ggplot(Dataweek6) +
  geom_point(aes(x = BIOM2, y = BIOM3)) +
  geom_abline(intercept = 8.012e+02, slope = 2.02e-04)
## This does not look god at all for biological data. 
## Does the slope and intercept make sense for the data? You should be asking this when using your own data. 

################################################################################
# Option 1: transform BIOM3 (try: log, sqrt)

# Scatter of BIOM2 and log-transformed BIOM3)
ggplot(Dataweek6) +
  geom_point(aes(x = BIOM2, y = log(BIOM3, base = 2))) # Still doesn't look great...
## BIOM3 is a squared version of BIOM2, so we wouldn't expect it to work

# Scatter of BIOM2 and SQRT trans. of BIOM3
ggplot(Dataweek6) +
  geom_point(aes(x = BIOM2, y = sqrt(BIOM3)))
## Not the right function to use for this data, it's just a straight line. 

# Scatter of BIOM2 and SQRT trans. of BIOM4
ggplot(Dataweek6) +
  geom_point(aes(x = BIOM2, y = sqrt(BIOM4))) # Looks better

# Scatter of BIOM2 and log-trans of BIOM4
ggplot(Dataweek6) +
  geom_point(aes(x = BIOM2, y = log(BIOM4))) # Doesn't look that great...

### Testing the perfectly straight model... ###
lm3 <- lm(BIOM2 ~ sqrt(BIOM3), data = Dataweek6)                          
summary(lm3) # "Essentially perfect fit"... Something isn't right!

# scater of the perfect line with alpha and beta included
ggplot(Dataweek6) +
  geom_point(aes(x = BIOM2, y = sqrt(BIOM3))) +
  geom_abline(intercept = 0, slope = 1)

################################################################################
# Option2 : Fit non-metric spline
sim <- tibble(
  x = seq(0, 3.5 * pi, length = 50),
  y = 4 * sin(x) + rnorm(length(x)))

# Scatter of x and y
ggplot(sim, aes(x, y)) +
  geom_point()
## Cannot add a normal linear line, it will go through 0, 0 and make no sense
## Need to find a sine function that will add a line here

# Use a natural cubic spine with 1, 2, 3, 4, 5 degrees of freedom (ns(x, df = df))
# Running a linear model with 5 various DF
library(splines)
mod1 <- lm(y ~ ns(x, 1), data = sim)
mod2 <- lm(y ~ ns(x, 2), data = sim)
mod3 <- lm(y ~ ns(x, 3), data = sim)
mod4 <- lm(y ~ ns(x, 4), data = sim)
mod5 <- lm(y ~ ns(x, 5), data = sim)

# Plot in 5 graphs, one per model
library(modelr)
grid <- sim %>% 
  data_grid(x = seq_range(x, n = 50, expand = 0.1)) %>% 
  gather_predictions(mod1, mod2, mod3, mod4, mod5, .pred = "y")

ggplot(sim, aes(x, y)) + 
  geom_point() +
  geom_line(data = grid, colour = "red") +
  facet_wrap(~ model)
## Model 5 is the best fit based upon the above graph and line fitting the graph
## Model 1= linear line w/ 1 DF
## Model 2= 1 curve w/ 2 DF
## Model 3= 2 curves w/ 3 DF
## Model 4= 3 curves w/ 4 DF
## Model 5= 4 curves w/ 5 DF
################################################################################
# Option 3: non-linear model
?nls

# Source: https://www.r-bloggers.com/2016/02/first-steps-with-non-linear-regression-in-r/

### Looking at enzyme kinetics data ###
# Keep in mind when interpretting the values/results/estimations, etc. #

# Simulate some data for the Michaelis-Menten equation
set.seed(20160227)
x<-seq(0,50,1)
y<-((runif(1,10,20)*x)/(runif(1,0,10)+x))+rnorm(51,0,1)

# For simple models, nls finds good starting values for the parameters even if it throws a warning
m<-nls(y~a*x/(b+x))

# Get some estimation of goodness of fit (R2)
cor(y,predict(m)) # Good R2 vales

plot(x,y)
lines(x,predict(m),lty=2, col="red",lwd=3)

################################################################################
################## Showing examples if you do not start at 0,0 #################

# Simulate some data, this without a priori knowledge of the parameter value
y<-runif(1,5,15)*exp(-runif(1,0.01,0.05)*x)+rnorm(51,0,0.5)

# Visually estimate some starting parameter values
plot(x,y)

#from this graph set approximate starting values
a_start<-8 #param a is the y value when x=0
b_start<-2*log(2)/a_start #b is the decay rate

# Model names "m"
m <- nls(y~a*exp(-b*x),start=list(a=a_start,b=b_start))

# Get some estimation of goodness of fit (R2)
cor(y,predict(m)) # Prety good!

# Adding a line of fit
lines(x,predict(m),col="red",lty=2,lwd=3)

library(deSolve)

# Simulating some population growth from the logistic equation and estimating the parameters using nls
log_growth <- function(Time, State, Pars) {
  with(as.list(c(State, Pars)), {
    dN <- R*N*(1-N/K)
    return(list(c(dN)))
  })
}
# The parameters for the logistic growth
pars  <- c(R=0.2,K=1000)

# The initial numbers
N_ini  <- c(N=1)

# The time step to evaluate the ODE
times <- seq(0, 50, by = 1)

# The ODE
out   <- ode(N_ini, times, log_growth, pars)

# Add some random variation to it
N_obs<-out[,2]+rnorm(51,0,50)

# Numbers cannot go lower than 1
N_obs<-ifelse(N_obs<1,1,N_obs)

# Plot
plot(times,N_obs)

# Find the parameters for the equation
SS<-getInitial(N_obs~SSlogis(times,alpha,xmid,scale),data=data.frame(N_obs=N_obs,times=times))

# We used a different parametrization
K_start<-SS["alpha"]
R_start<-1/SS["scale"]
N0_start<-SS["alpha"]/(exp(SS["xmid"]/SS["scale"])+1)

# The formula for model "m"
log_formula<-formula(N_obs~K*N0*exp(R*times)/(K+N0*(exp(R*times)-1)))

# Fit the model "m"
m<-nls(log_formula,start=list(K=K_start,R=R_start,N0=N0_start))

# Estimated parameters for model "m"
summary(m)

# Get some estimation of goodness of fit (R2)
cor(N_obs,predict(m))

# Plot
lines(times,predict(m),col="red",lty=2,lwd=3)

################################## END DAY 12 ##################################
################################# START DAY 13 #################################
################################################################################
# Date: 15 FEB 2023 (Wed)     ##################################################
# Week: 6                     ##################################################
# Day: 13                     ##################################################
################################################################################

####################### GENERALIZED LINEAR MODELS! #############################

# Load in libraries/pkgs #
library(tidyverse)

# # Using dataweek1 ##
#Investigate the relationship between MAT and BIOM2

# Scatter of MAT and BIOM2
ggplot(Dataweek1) +
  geom_point(aes(x = MAT, y = BIOM2)) # Is there a trend? A: Difficult to tell...

# First test with linear regression model
lm1 <- lm(BIOM2 ~ MAT, data = Dataweek1)
summary(lm1)
# How much of the variation in BIOM2 is explained by MAT? What is the slope?
# Residuals are BAD!
# Slope= 63.892
# Variation= 0.3% of the variation is explained by this model. 

# Now fit a GLM 
glm1 <- glm(BIOM2 ~ MAT, family = gaussian, data = Dataweek1)
summary(glm1)

# Are there differences between lm1 and glm1?
# Slope, intercept, p-value, and residuals = SAME as other model, lm1
## Null model has no explanatory variables

# We noted that treatment had an effect on height and Kellogg is much warmer - include those in the glm
glm2 <- glm(BIOM2 ~ MAT + Treated + Site, family = gaussian, data = Dataweek1)
summary(glm2)

# Did this model improve on the previous? If so, why?
# Yes, this is because we have added more variables to this model. 
# The deviance values are slightly better as are the AIC values. 
# Treatment and site added made a difference in these values. 

# Let's compare to lm using multiple linear regression
lm2 <- lm(BIOM2 ~ MAT + Treated + Site, data = Dataweek1)
summary(lm2)

# Compare lm2 to glm2
# A: They are the same models, one is juts a linear model and one is GLM.
# Site and treatment have an effect

# Maybe other variable also explain variation in BIOM2, let's make a global model
glm3 <- glm(BIOM2 ~ MAT + Treated + Site + Species + Block + Lat + Long, family = gaussian, data = Dataweek1)
summary(glm3) # Residuals still not great here...
#A. MAT has the opp. affect as before. Treatment has a huge affect. Need to analyze species separately
#A. Blocks don't have a huge affect, or any at all in this model. 

# R2 for glm3
1 - glm3$deviance/glm3$null.deviance

# Some blocks seem to have an effect, but we are overparameterizing (i.e., adding too many parameters)

# Let's remove block and Long - that does not seem to have an effect on BIOM2
glm4 <- glm(BIOM2 ~ MAT + Treated + Site + Species + Lat, family = gaussian, data = Dataweek1)
summary(glm4) # Residuals worse in this model... AIC also higher 
# MAT not sig. 
# Negative trend

# R2 for glm4
1 - glm4$deviance/glm4$null.deviance

# What is the issue with this model? Why does MAT not have an effect on BIOM anymore? Is Lat the better predictor?
glm5 <- glm(BIOM2 ~ Treated + Site + Species + Lat, family = gaussian, data = Dataweek1)
summary(glm5) # The issue with this model is 

# R2 for glm5
1 - glm5$deviance/glm5$null.deviance

# Is there an interaction of Lat and Site? A: yes
# Looking at the specific interaction between lat and site
glm6 <- glm(BIOM2 ~ Treated + Species + Lat*Site, family = gaussian, data = Dataweek1)
summary(glm6)

# R2 for glm6
1 - glm6$deviance/glm6$null.deviance

# Compare glm1-6 - AIC is a good starting point
AIC(glm1, glm2, glm3, glm4, glm5, glm6)
## Looking for a change of 5 more more in AIC values. The absolute value does not matter, but the delta AIC needs to be >5

# Of these 6 models glm6 has the lowest AIC and therefore explains most variation of BIOM2 with the least number of parameters

library(performance)
?check_model
check_model(glm6)

# Lat: Site is correlated to Site, we can remove Lat:Site to see if this improves the model
glm7 <- glm(BIOM2 ~ Treated + Species + Lat + Site, family = gaussian, data = Dataweek1)
summary(glm7)
check_model(glm7) # VERY cool graph! 

# That was a step in the wrong direction (check the AIC) and glm can use data with enhanced co-linearity

################################## END DAY 13 ##################################
################################# START DAY 14 #################################
################################################################################
# Date: 22 FEB 2023 (Wed)     ##################################################
# Week: 7                     ##################################################
# Day: 15                     ##################################################
################################################################################
# Load in libraries #
library(tidyverse)
library(popbio)
library(drc)
library(readxl)

# Load in files, named bugstidy #
# Breaking up each treatment into separate data frames #
pip <- bugstidy[bugstidy$Trt == "pip",]
pipl <- bugstidy[bugstidy$Trt == "pipl",]
mix <- bugstidy[bugstidy$Trt == "mix",]
unripe <- bugstidy[bugstidy$Trt == "unripe",]
ripe <- bugstidy[bugstidy$Trt == "ripe",]

############# This is where Day 14 ended, starting here for Day 15 #############
library(drc) # Fitting dose-response models
# Look at the drc help - drm function
?drm

# Use one fungus and 2-3 treatments to fit model using drm 
# (drmX <- drm(Y ~ X, data = "your_data", fct = LL.3()))

drm1 <- drm(growth_unripe ~ mg_unripe, data = fungustidy, fct = LL.3())
drm2 <- drm(growth_ripe ~ mg_ripe, data = fungustidy, fct = LL.3())

# Plot the models and compare to fig3 (plot(drmX))
plot(drm1)
plot(drm2)

# These graphs are similar to Fig. 3 in the paper and shows that their data analyses
# is reproducible

################################################################################
################################ NEW CODE ######################################
###### POISSON DISTRIBUTIONS ######

# Poisson GLM
library(datasets)
library(help = "datasets")

# Load data warpbreaks
data_w <- warpbreaks
head(data_w) # First 6 columns in set
columns <- names(data_w)
columns # Renaming the columns 
# This data set looks at how many warp breaks occurred for different types of looms per loom, per fixed length of yarn.

# Investigate data structure and distribution
ls.str(warpbreaks)
hist(data_w$breaks)
summary(data_w$breaks)
var(data_w$breaks)

# Variance value is much larger than the mean leading to over-dispersion of the model

# glm
glm_poisson <- glm(breaks ~ wool + tension, family = poisson(link = "log"), data = data_w)
summary(glm_poisson)
# The residuals aren't bad considering the breadth fo the data (10-70) and median close to 0
# p-values state that each variable is significant in this model
# Looking at null and residual deviance we can tell our model is improved from the null model/deviance

# Since we have overdispersion (var >> mean), we can try a quasipoisson distribution
glm_qpoisson <- glm(breaks ~ wool + tension, family = quasipoisson(link = "log"), data = data_w)
summary(glm_qpoisson)

# Slopes/betas= same
# Standard errors (SE) different... nearly 2x what the original model is.. 
# Takes into account overdispersion
# More cautious approach by introducing larger SE

# A useful package to compare glm is arm
library(arm)

# Extract coefficients from first model using 'coef()'
coef1 = coef(glm_poisson)

# Extract coefficients from second model
coef2 = coef(glm_qpoisson)

# Extract standard errors from first model using 'se.coef()'
se.coef1 = se.coef(glm_poisson)

# Extract standard errors from second model
se.coef2 = se.coef(glm_qpoisson)

# Use 'cbind()' to combine values into one dataframe
models.both <- cbind(coef1, se.coef1, coef2, se.coef2, exponent = exp(coef1))

# Show dataframe
models.both
# This shows the back-transformed exponent
# Avg changing from one to another because this is a factorial data set


# Compare se for both models
# The exponent is needed to back transform coefficient - they were log transformed in the model!
# The difference (slope, beta) between wool A and wool B is -0.8138425 
# or there is an (1-0.8138425 = 0.186) 18.6% decrease in breaks with wool B compared to wool A assuming all other varibles are constant

# Using the model to predict
# Make a dataframe with new data
newdata = data.frame(wool = "B", tension = "M")

# use 'predict()' to run model on new data
predict(glm_qpoisson, newdata = newdata, type = "response")
predict(glm_poisson, newdata = newdata, type = "response")

#On average there will be 24 breaks with wool type B and tension level M

library(broom)
library(jtools)
library(ggstance)

# plot regression coefficients for glm_qpoisson
plot_summs(glm_qpoisson, scale = TRUE, exp = TRUE)

# plot regression coefficients for glm_qpoisson and glm_poisson
plot_summs(glm_poisson, glm_qpoisson, scale = TRUE, exp = TRUE)
# Model 1 has smaller SE, but Model 2 is more conservative
# May want to use model 2 because it is more conservative

# Scale helps if different variables have different scales, exp = TRUE to convert back from log
# Load Data_week7

# Investigate Lesion
ls.str(Data_week7)
hist(Data_week7$Lesion)
summary(Data_week7$Lesion)
var(Data_week7$Lesion)

# Investigate Leaf curl
hist(Data_week7$LeafCurl)
summary(Data_week7$LeafCurl)
var(Data_week7$LeafCurl)

# Investigate herbivory
hist(Data_week7$Herbivory)
summary(Data_week7$Herbivory)
var(Data_week7$Herbivory)

#Var << mean leading to under-dispersion of the model

#Run glm for each Lesion, LeafCurl and Herbivory and use step to produce optimal model
glm_Lesion <- glm(Lesion ~ Species + Site + Treated + Lat + Long + MAT + RainS, family = poisson(link = "log"), data = Data_week7)
summary(glm_Lesion) # Residuals should be much MUCH smaller than the range of your data!
step(glm_Lesion)
# Don't ever look at residuals by themselves, but always in the context of your data range


glm_LeafCurl <- glm(LeafCurl ~ Species + Site + Treated + Lat + Long + MAT + RainS, family = poisson(link = "log"), data = Data_week7)
summary(glm_LeafCurl)
step(glm_LeafCurl)

glm_Herbivory <- glm(Herbivory ~ Species + Site + Treated + Lat + Long + MAT + RainS, family = poisson(link = "log"), data = Data_week7)
summary(glm_Herbivory)
step(glm_Herbivory)

glm_Herbivory2 <- glm(Herbivory ~ Treated + Lat + MAT + RainS, family = poisson(link = "log"), data = Data_week7)
summary(glm_Herbivory2)

# Visualize glm_Herbivory2 
ggplot(data = glm_Herbivory2) + geom_point(mapping = aes(x = Herbivory, y = Lat))
ggplot(data = glm_Herbivory2) + geom_point(mapping = aes(x = Herbivory, y = MAT))
ggplot(data = glm_Herbivory2) + geom_point(mapping = aes(x = Herbivory, y = RainS))

################################# End Day 15 ###################################
################################# Start Day 16 #################################

################################################################################
# Date: 24 FEB 2023 (Fri)     ##################################################
# Week: 7                     ##################################################
# Day: 16                     ##################################################
################################################################################

############################ MIXED LINEAR MODELS ###############################
# Load in packages #
library(tidyverse)

# View the first 6 rows of data
head(dragons)
# This data looks at the intelligence of dragons based on their mountain range and body length

# look at variation of dragons
str(dragons)

# Check dragon intelligence (testScore) for normality
hist(dragons$testScore)  # Seems close to a normal distribution - good!

# Hist of dragons and body length
hist(dragons$bodyLength) # normal but not as good as intelligence

# Scale and center explanatory data 
dragons$bodyLength2 <- scale(dragons$bodyLength, center = TRUE, scale = TRUE)
#### How do centralizing and transforming data differ? 
# A: Only use scale and centralize if your data is near normal, or if you have wildly different units

# Hist of centralized and scaled body length data of dragons
hist(dragons$bodyLength2)

#Run linear regression analysis with testScore ~ bodyLength2
basic.lm <- lm(testScore ~ bodyLength2, data = dragons)
summary(basic.lm)
# The larger the dragon, the more intelligent

# Plot using ggplot
(prelim_plot <- ggplot(dragons, aes(x = bodyLength, y = testScore)) +
    geom_point() +
    geom_smooth(method = "lm")) 

# plot residuals vs fitted
plot(basic.lm, which = 1) # 119, 290, 294 outliers
# Making sure there is a straight line and that there is not a trend awya from the 0 line at either end
# The more data points, the less perfect it will look

# Check the Q-Q plot
plot(basic.lm, which = 2)
# Which tells the program which plot you want to look at (there are 4 total)

# We collected multiple samples from eight mountain ranges. It’s perfectly plausible that the data from within each mountain range are more similar to each other than the data from different mountain ranges: they are correlated.
# Intelligence by mountain range
boxplot(testScore ~ mountainRange, data = dragons) # 4 with high scores, 4 with low scores

# A different way to visualize is with a colored (by mountain range) scatter plot
(colour_plot <- ggplot(dragons, aes(x = bodyLength2, y = testScore, colour = mountainRange)) +
    geom_point(size = 2) +
    theme_classic() +
    theme(legend.position = "none")) # Looking at body length and test score, with the color being mountain range

#From the above plots, it looks like our mountain ranges vary both in the dragon body length AND in their test scores. This confirms that our observations from within each of the ranges aren’t independent. We can’t ignore that: as we’re starting to see, it could lead to a completely erroneous conclusion.

# Split plot by mountain range using facet wrap. Looking at the test scores and body length by mountain range
(split_plot <- ggplot(aes(bodyLength2, testScore), data = dragons) + 
    geom_point() + 
    facet_wrap(~ mountainRange) + # create a facet for each mountain range
    xlab("length") + 
    ylab("test score"))

# We could run linear regression by Site, but that would reduce sample # and increase chance of Type I Error
# Type 1 = False positive; Stating somehting is true when it isn't

# Instead, we include mountain range as a fixed effect
mountain.lm <- lm(testScore ~ bodyLength2 + mountainRange, data = dragons)
summary(mountain.lm)

# we are not interested in quantifying test scores for each specific mountain range:
# we just want to know whether body length affects test scores and we want to simply control for the variation coming from mountain ranges.
# This is what we refer to as “random factors” and so we arrive at mixed effects models.
# Fixed= Explanatory variable
# Random= A new variable that is treated as a correction factor.. Need at least 5 levels

library(lme4)

# A MLM will allow us to use all the data we have (higher sample size) and account for the correlations between data coming from the sites and mountain ranges
# DF is coming form fixed effects, NOT random effects

# A MLM will also estimate fewer parameters and avoid problems with multiple comparisons that we would encounter while using separate regressions
#Read about the difference of FIXED vs. RANDOM effects-- In the link from Dr. Kulheim's announcement from Wed 2/22

# FIXED are our explanatory variables - the variables we want to use to explain variation in our response variable
# RANDOM are grouping factors that influence the response variable (always categorical)

#RANDOM effects control for the effects of the variation they introduce

#Let's run a MLM
mixed.lmer <- lmer(testScore ~ bodyLength2 + (1|mountainRange), data = dragons)
summary(mixed.lmer)
plot(mixed.lmer)
qqnorm(resid(mixed.lmer))
qqline(resid(mixed.lmer)) 

# Add site as explicitly nested factor to mountain range
# We have 24 sites, and ecah has A, B, C, etc. but they are not the same at each mountain range
dragons <- within(dragons, sample <- factor(mountainRange:site))

# Do not do this
mixed.WRONG <- lmer(testScore ~ bodyLength2 + (1|mountainRange) + (1|site), data = dragons)  # treats the two random effects as if they are crossed
summary(mixed.WRONG)
#24 sites were sampled, not 3! Need 5 levels in a factor

# This is the correct way of including mountain range and site within mountain range (nested):
mixed.lmer2 <- lmer(testScore ~ bodyLength2 + (1|mountainRange) + (1|sample), data = dragons)  # the syntax stays the same, but now the nesting is taken into account
summary(mixed.lmer2)

# Plot this MLM:
(mm_plot <- ggplot(dragons, aes(x = bodyLength, y = testScore, colour = site)) +
    facet_wrap(~mountainRange, nrow=2) +   # a panel for each mountain range
    geom_point(alpha = 0.5) +
    theme_classic() +
    geom_line(data = cbind(dragons, pred = predict(mixed.lmer2)), aes(y = pred), linewidth = 1) +  # adding predicted line from mixed model 
    theme(legend.position = "none",
          panel.spacing = unit(2, "lines"))) # adding space between panels

################################## END DAY 16 ##################################
################################### START DAY 17 ###############################

################################################################################
# Date: 27 FEB 2023 (Mon)     ##################################################
# Week: 8                     ##################################################
# Day: 17                     ##################################################
################################################################################
# Load in packages and the data #
library(tidyverse)

# View th firts 6 rows of the data 
head(dragons)

# Check dragon intelligence (testScore) for normality
hist(dragons$testScore)  # seems close to a normal distribution - good!

# Scale and center explanatory data; view
dragons$bodyLength2 <- scale(dragons$bodyLength, center = TRUE, scale = TRUE)
hist(dragons$bodyLength2)

# Run linear regression analysis with testScore ~ bodyLength2
basic.lm <- lm(testScore ~ bodyLength2, data = dragons)
summary(basic.lm)

# Plot using ggplot
(prelim_plot <- ggplot(dragons, aes(x = bodyLength, y = testScore)) +
    geom_point() +
    geom_smooth(method = "lm"))

#plot residuals vs fitted
plot(basic.lm, which = 1)

#Check the Q-Q plot
plot(basic.lm, which = 2)

#We collected multiple samples from eight mountain ranges. It’s perfectly plausible that the data from within each mountain range are more similar to each other than the data from different mountain ranges: they are correlated.
boxplot(testScore ~ mountainRange, data = dragons)

#A different way to visualize is with a colored (by mountain range) scatter plot
(colour_plot <- ggplot(dragons, aes(x = bodyLength, y = testScore, colour = mountainRange)) +
    geom_point(size = 2) +
    theme_classic() +
    theme(legend.position = "none"))

#From the above plots, it looks like our mountain ranges vary both in the dragon body length AND in their test scores. This confirms that our observations from within each of the ranges aren’t independent. We can’t ignore that: as we’re starting to see, it could lead to a completely erroneous conclusion.

#Split plot by mountain range
(split_plot <- ggplot(aes(bodyLength, testScore), data = dragons) + 
    geom_point() + 
    facet_wrap(~ mountainRange) + # create a facet for each mountain range
    xlab("length") + 
    ylab("test score"))

#We could run linear regression by Site, but that would reduce sample # and increase chance of Type I Error

#Instead, we include mountain range as a fixed effect
mountain.lm <- lm(testScore ~ bodyLength2 + mountainRange, data = dragons)
summary(mountain.lm)

#we are not interested in quantifying test scores for each specific mountain range:
#we just want to know whether body length affects test scores and we want to simply control for the variation coming from mountain ranges.
#This is what we refer to as “random factors” and so we arrive at mixed effects models.

library(lme4)

#A MLM will allow us to use all the data we have (higher sample size) and account for the correlations between data coming from the sites and mountain ranges
#A MLM will also estimate fewer parameters and avoid problems with multiple comparisons that we would encounter while using separate regressions

#Read about the difference of FIXED vs. RANDOM effects
#FIXED are our explanatory variables - the variables we want to use to explain variation in our response variable
#RANDOM are grouping factors that influence the response variable (always categorical)

#RANDOM effects control for the effects of the variation they introduce

#Let's run a MLM
mixed.lmer <- lmer(testScore ~ bodyLength2 + (1|mountainRange), data = dragons)
summary(mixed.lmer)
plot(mixed.lmer)
qqnorm(resid(mixed.lmer))
qqline(resid(mixed.lmer)) 

#Add site as explicitly nested factor to mountain range
dragons <- within(dragons, sample <- factor(mountainRange:site))

#Do not do this
mixed.WRONG <- lmer(testScore ~ bodyLength2 + (1|mountainRange) + (1|site), data = dragons)  # treats the two random effects as if they are crossed
summary(mixed.WRONG)
#24 sites were sampled, not 3!

#This is the correct way of including mountain range and site within mountain range (nested):
mixed.lmer2 <- lmer(testScore ~ bodyLength2 + (1|mountainRange) + (1|sample), data = dragons)  # the syntax stays the same, but now the nesting is taken into account
summary(mixed.lmer2)

#Plot this MLM:
(mm_plot <- ggplot(dragons, aes(x = bodyLength, y = testScore, colour = site)) +
    facet_wrap(~mountainRange, nrow=2) +   # a panel for each mountain range
    geom_point(alpha = 0.5) +
    theme_classic() +
    geom_line(data = cbind(dragons, pred = predict(mixed.lmer2)), aes(y = pred), linewidth = 1) +  # adding predicted line from mixed model 
    theme(legend.position = "none",
          panel.spacing = unit(2, "lines"))  # adding space between panels
)

#Introducing random slopes
#So far all lines are parallel due to fitting of random-intercept models, which allows for variable intercepts, but constant slopes
#This assumes the same relationship between body length and intelligence.

#Random slope and random intercept model "Maybe the dragons in a very cold vs a very warm mountain range 
#have evolved different body forms for heat conservation and may therefore be smart even if they’re smaller than average."

mixed.ranslope <- lmer(testScore ~ bodyLength2 + (1 + bodyLength2|mountainRange/site), data = dragons) 
summary(mixed.ranslope)

# We mode the intelligence of the dragons as a function of body lenght, correcting for baseline difference in intelligence, and the 
#differing relationship between populations

#plot
(mm_plot <- ggplot(dragons, aes(x = bodyLength, y = testScore, colour = site)) +
    facet_wrap(~mountainRange, nrow=2) +   # a panel for each mountain range
    geom_point(alpha = 0.5) +
    theme_classic() +
    geom_line(data = cbind(dragons, pred = predict(mixed.ranslope)), aes(y = pred), size = 1) +  # adding predicted line from mixed model 
    theme(legend.position = "none",
          panel.spacing = unit(2, "lines"))  # adding space between panels
)

#Presentation of results
#Once you have a final model, you want to create nice output graphs and tables

library(ggeffects)
# Extract the prediction data frame from random slopes model
pred.mm <- ggpredict(mixed.ranslope, terms = c("bodyLength2"))  # this gives overall predictions for the model

#Plot predictions from the random slopes model and raw data ~ mountain range:
(ggplot(pred.mm) + 
    geom_line(aes(x = x, y = predicted)) +          # slope
    geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                fill = "lightgrey", alpha = 0.5) +  # error band
    geom_point(data = dragons,                      # adding the raw data (scaled values)
               aes(x = bodyLength2, y = testScore, colour = mountainRange)) + 
    labs(x = "Body Length (indexed)", y = "Test Score", 
         title = "Body length does not affect intelligence in dragons") + 
    theme_minimal()
)

#Plot the slopes by mountain range using the random slopes model
ggpredict(mixed.ranslope, terms = c("bodyLength2", "mountainRange"), type = "re", ci.lvl = NA) %>% 
  plot() +
  labs(x = "Body Length", y = "Test Score", title = "Effect of body size on intelligence in dragons") + 
  theme_minimal()

library(sjPlot)
library(glmmTMB)

# Visualise random effects 
(re.effects <- plot_model(mixed.ranslope, type = "re", show.values = TRUE))

# show summary
summary(mixed.ranslope)

#What you see are the the difference between the general intercept and the model value, not actual values

#Creating output tables
library(stargazer)

stargazer(mixed.lmer2, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

#Change "type =" so that you can print the table once you are happy with it.

stargazer(mixed.ranslope, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

# See tutorial part on model selection and p-values
# We do a likelihood test using anova
# Make a full model and reduced model without body length

full.lmer <- lmer(testScore ~ bodyLength2 + (1|mountainRange) + (1|sample), 
                  data = dragons, REML = FALSE) # Restricted max likelihood method
reduced.lmer <- lmer(testScore ~ 1 + (1|mountainRange) + (1|sample), 
                     data = dragons, REML = FALSE)

#Use anova to compare the two models
anova(reduced.lmer, full.lmer)  
# Shows that body length has no affect on intelligence
# change in AIC... Smaller AIC value is the better of two models; <2 NOT significant, >5 probably significant
# AIC takes # of parameters into account


# the two models are not significantly different

#Also look at delta AIC = a delta of 2 or less shows model quality is the same, 
#<5 = models are somewhat similar
#>10 = models are very different, the lower AIC is the better model

#Use Biology and your questions as a guide to model selection! 

################################# END DAY 17 ###################################
################################################################################
# Date: 20 MAR 2023 (Mon)     ##################################################
# Week: 10                    ##################################################
# Day: 19                     ##################################################
################################################################################
# Load in packages #
library(lme4)
library(VCA)

#Data = data_week10
#Data = acorn

# Make sure VCA recognizes data as dataframe. You can resave "as.data.frame" if you have issues
acorn <- as.data.frame(acorn)
data_week10 <- as.data.frame(data_week10)

# Response vars = log10_BIOM2, H_Cdav, massg
?VCA

### Running anovaVCA with all 3 variables ###
?anovaVCA

anova_acorn <- anovaVCA(Massg ~ Region/Population/Family/Individual, acorn)
anova_acorn
# The % total is what goes into the pie charts we are trying to create. 

anova_acorn2 <- anovaVCA(H_Cdav ~ Region/Population/Family/Inidividual, data_week10)
anova_acorn2

anova_acorn3 <- anovaVCA(log10_BIOM2 ~ Region/Population/Family/Inidividual, data_week10)
anova_acorn3

### Running remlVCA with all 3 variables ###
?remlVCA

reml1 <- remlVCA(Massg ~ Region/Population/Family, acorn)
reml1

reml2 <- remlVCA(H_Cdav ~ Region/Population/Family/Inidividual, data_week10)
reml2

reml3 <- remlVCA(log10_BIOM2 ~ Region/Population/Family/Inidividual, data_week10)
reml3

### Running lmer with all 3 variables ###
# First test which of the following affects variation in data: Site, Treated, Block
# If any, include in model as fixed effect.
MLM_acorn <- lmer(Massg ~ (1|Region) + (1|Region:Population) + (1|Population:Family) + 
                    (Family:Individual), data = acorn)
summary(MLM_acorn)
MLM_acorn

lm_acorn2 <-lm(H_Cdav ~ Region + Population + Family + Inidividual, data_week10)
summary(lm_acorn2)# Significant
lm_acorn2

lm_acorn3 <- lm(log10_BIOM2 ~ Region + Population + Family + Inidividual, data_week10)
summary(lm_acorn3) # Significant
lm_acorn3

MLM_HDAV <- lmew(H_Cdav ~ Site + + (1|Region) + (1|Region:Population))
# Does not explain much of the variance at all

############################## END DAY 20? #####################################
##### Worked on final projects for a few days #####
######################### START DAYS 21-22 #####################################
################################################################################
# Date: 27 MAR 2023 (Mon, Wed ##################################################
# Week: 11                    ##################################################
# Day: 21-22                  ##################################################
################################################################################
# Load data "iris" #
data(iris)
head(iris)
summary(iris)

# Run PCA on first four columns
myPr <- prcomp(iris[,1:4], scale = TRUE)
myPr
summary(myPr)

# Plot PCA
plot(myPr)
biplot(myPr)

# Plot first three PCs
library(plot3D)
scores <- as.data.frame(myPr$x)
head(scores)
plot3D::scatter3D(scores$PC1, scores$PC2, scores$PC3)
# Hard to interpret in a 3D form, especially with these colors. 

# Combine PCA with data
iris2 <- cbind(iris, myPr$x[,1:2])
head(iris2)

# Plot with ggplot
library(ggplot2)

ggplot(iris2, aes(PC1, PC2, col = Species, fill = Species)) +
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) +
  geom_point(shape = 21, col = "black")
# This graph shows that the setosa species is significantly different from the other two. 
# I. versicolor and I. virginica have more traits in common with each other than I. setosa. 
# Ellipse is a 95% CI around the data

################################################################################
# Example 2: (https://www.statology.org/principal-components-analysis-in-r/)
# Load in libraries # 
library(tidyverse)
# Load data
data("USArrests")
head(USArrests)

# Run PCA
myPCA <- prcomp(USArrests, scale = TRUE)
myPCA
summary(myPCA)

# Reverse signs for eigenvectors as R defaults with negative values
myPCA$rotation <- -1*myPCA$rotation 
#-1 switches the pos. to neg., and vice versa... Changes the rotation
myPCA
summary(myPCA)

# Display principal components
myPCA$rotation

# Reverse signs for eigenvalues
myPCA$x <- -1*myPCA$x # Changes the rotation
head(myPCA$x)

# Visualize, scale = 0 to represent the loading scores
biplot(myPCA, scale = 0)
plot(myPCA)
plot(myPCA, type = "l")

############################## START DAY 22 ####################################
################################################################################
# Example 3:  (https://sdsclub.com/the-ultimate-guide-on-principal-component-analysis-in-r/)
head(LifeCycleSavings)

PCA3 <- prcomp(LifeCycleSavings[,c(1:5)], center = TRUE, scale. = TRUE)
PCA3
summary(PCA3)
names(PCA3)
PCA3$center
PCA3$scale
PCA3$rotation
PCA3$x
biplot(PCA3)
plot(PCA3, type = "l") # If you don't put in type L, you get a bar chart #

# Use data week1
PCA4 <- prcomp(Dataweek1[,c(13:16)], scale = TRUE)
PCA4
summary(PCA4)
biplot(PCA4)
plot(PCA4)

# Combine PCA with data
Dataweek11 <- cbind(Dataweek1, PCA4$x[,1:2])
head(PCA4)

# Plot with ggplot
library(ggplot2)

ggplot(Dataweek11, aes(PC1, PC2, col = Species, fill = Species)) +
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) +
  geom_point(shape = 21, col = "black")

library(lme4)
mlm1 <- lmer(PC1 ~ MAT + RainS + (1|Species:Site:Treated), data = Dataweek11)
summary(mlm1)

# Load data saspect3
head(saspec3)
lm1 <- lm(terp3 ~ ., data = saspec3)
summary(lm1)


PCA <- prcomp(saspec3[,(2:117)])
PCA
summary(PCA)
plot(PCA)
biplot(PCA)
PCA$x

saspec4 <- cbind(saspec3, PCA$x[,(1:2)])
head(saspec4)

lm2 <- lm(terp3 ~ PC1 + PC2, data = saspec4)
summary(lm2)
################################ END DAYS 21-22 ################################
################################################################################
# Date: 3-5 APR 2023 (Mon.Wed0 #################################################
# Week: 12                    ##################################################
# Day: 23-24                  ##################################################
################################################################################
############################### ANOVA ##########################################
# load in libraries #
library(tidyverse)

# One-way ANOVA
# checking one response variable with one explanatory variable
anova1 <- aov(BIOM2 ~ MAT, data = Dataweek1)
summary(anova1)

# ANOVA with interaction of Site and Treated
anova2 <- aov(BIOM2 ~ Site:Treated, data = Dataweek1)
summary(anova2)
TukeyHSD(anova2) # Tukey's gives us a comparison of individual variables
# You can run ANOVA without doing Tukey's, but only if it's enough to just 
# say that there's a diff. and you don't care where that diff. is.

################################################################################
# Two-way ANOVA
# Checking two response variables with two explanatory variables
anova3 <- aov(BIOM2 ~ MAT + RainS, data = Dataweek1)
summary(anova3)

# Two-way ANOVA with interaction
anova4 <- aov(BIOM2 ~ MAT*RainS, data = Dataweek1)
summary(anova4)

################################################################################
# Load plant growth data
anova_data <- PlantGrowth
head(anova_data)

# Show some random samples from the dataframe using sample_n() from the dplyr package
set.seed(1234) # Sets a starting point rather than at the start of the data
sample_n(anova_data, 10)

# Show how many treatment levels there are
levels(anova_data$group)

# Levels are ordered alphabetically, you can re-order with this command if you like
anova_data$group <- ordered(anova_data$group,
                            levels = c("ctrl", "trt1", "trt2"))

# Get summary stats of data (mean, sd, count) using summary from dplyr
group_by(anova_data, group) %>%
  summarise(
    count = n(),
    mean = mean(weight, na.rm = TRUE),
    sd = sd(weight, na.rm = TRUE)
  )

# Plot data; Useful for my project! Very cool graph
boxplot(weight ~ group, data = anova_data,
        xlab = "Treatment", ylab = "Weight",
        frame = FALSE, col = c("#00AFBB", "#E7B800", "#FC4E07"))

# Compute the analysis of variance
res.aov <- aov(weight ~ group, data = anova_data)

# Summary of the analysis
summary(res.aov) # There is a significant difference overall. Let's run Tukey's to see where
TukeyHSD(res.aov) # TRT 2 and 1 are different. Cannot reject null hypo. 
# Don't need to run Tukey's if there is nothing significant

################################################################################
# Load in new library #
library(multcomp)

summary(glht(res.aov, linfct = mcp(group = "Tukey")))

# Instead of ANOVA, you can do paired t-test; Results show p-values in matrix form
# Outcomes are similar to pairwise t-test:
pairwise.t.test(anova_data$weight, anova_data$group,
                p.adjust.method = "BH")
# p-values have been adjusted with the Benjamini-Hochberg method

# 1. Homogeneity of variances
plot(res.aov, 1)

# 2. Normality
plot(res.aov, 2)

#Also supported by:
# Extract the residuals
aov_residuals <- residuals(object = res.aov )
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals )

################################################################################
#3. https://www.scribbr.com/statistics/anova-in-r/
#load data
crop.data <- read.csv("crop.data.csv", header = TRUE, colClasses = c("factor", "factor", "factor", "numeric"))
summary(crop.data)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Run one-way anova of yield by fertilizer
one.way <- aov(yield ~ fertilizer, data = crop.data)
summary(one.way) # Fertilizer has a sig. effect on yield; 46% of variance

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Run two-way anova of yield by fertilizer and planting density
two.way <- aov(yield ~ fertilizer + density, data = crop.data)
summary(two.way) # Both fertilizer and density explain more of the variance and are sig. 
# More of the variance is explained - the residual is smaller

############################### END DAY 23 #####################################
############################# START DAY 24 #####################################
# Add interaction between fertilizer and density
interaction <- aov(yield ~ fertilizer*density, data = crop.data)
summary(interaction) # Not sig., only explains a small amount of variation!

# Add block as an interaction
blocking <- aov(yield ~ fertilizer + density + block, data = crop.data)
summary(blocking) # block has no sig. effect on density or fertilizer; Only ~1% of variation explained

# Compare models with AIC 
library(AICcmodavg)
model.set <- list(one.way, two.way, interaction, blocking)
model.names <- c("one.way", "two.way", "interaction", "blocking")

aictab(model.set, modnames = model.names) # Two-way ANOVA is best, based on AIC

# Check for homoscedasticity / assumptions
par(mfrow=c(2,2))
plot(two.way)
par(mfrow=c(1,1))

# Alternative test (Tukey's)
tukey.two.way<-TukeyHSD(two.way)
tukey.two.way

# Plot results
tukey.plot.aov<-aov(yield ~ fertilizer:density, data=crop.data)
tukey.plot.test<-TukeyHSD(tukey.plot.aov)
plot(tukey.plot.test, las = 1)

# Make labelled graph
mean.yield.data <- crop.data %>%
  group_by(fertilizer, density) %>%
  summarise(
    yield = mean(yield)
  )

mean.yield.data$group <- c("a","b","b","b","b","c")
mean.yield.data

two.way.plot <- ggplot(crop.data, aes(x = density, y = yield, group=fertilizer)) +
  geom_point(cex = 1.5, pch = 1.0, position = position_jitter(w = 0.1, h = 0))
two.way.plot

two.way.plot <- two.way.plot +
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.2) +
  stat_summary(fun.data = 'mean_se', geom = 'pointrange') +
  geom_point(data=mean.yield.data, aes(x=density, y=yield))

two.way.plot

# ... OR ... 
two.way.plot <- two.way.plot +
  geom_text(data=mean.yield.data, label=mean.yield.data$group, vjust = -8, size = 5) +
  facet_wrap(~ fertilizer)

two.way.plot # A bit nicer to look at. facetwrap function breaks it up by fertilizer
################################## END DAY 24 ##################################