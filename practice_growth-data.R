# Practice script for fake growth data
# Script by Robert Johnson


library(tidyverse)

# override default options to always print out (i.e. return) all rows of a data frame
options(tibble.print_max = Inf)


# data
data_site = read_csv("data_site.csv")
data_animal = read_csv("data_animal.csv")
data_growth = read_csv("data_growth.csv")


##_Sorting and subsetting data

## selecting data by rows

#---
# filter()
#---

# View site data for all sites north of Osborn Dr.
data_site %>%
   filter(region=="north")

   # base #
   data_site[data_site$region=="north", ]


# View data only for species "tt"
data_animal %>%
   filter(sp=="tt")

   # base #
   data_animal[data_animal$sp=="tt", ]


# View data for all animals older than age 1
data_animal %>%
   filter(age>1)

   # base #
   data_animal[data_animal$age>1, ]


# View length and weight data for animals on the final measurement day

   #---
   # what is the value of the final measurement day?
   data_growth %>% pull(day) %>% unique()
   
      #base #
      unique(data_growth[["day"]])
   #---

data_growth %>%
   filter(day==38)

   # base #
   data_growth[data_growth$day==38, ]


## selecting data by columns

#---
# select()
#---

# select only the desired columns/variables
data_site %>%
   select(site_id, region, veg_type)

   # base #
   data_site[ , c("site_id", "region", "veg_type")]


# remove and undesired column, and keep the rest
data_site %>%
   select(-veg_type)

   # base #
   subset(data_site, select = -veg_type)


# select a range of adjacent columns
data_site %>%
   select(lat:veg_type)

   # base #
   subset(data_site, select = lat:veg_type)


# remove a range of adjacent columns
data_site %>%
   select(-lat:-long)

   # base #
   subset(data_site, select = -lat:-long)


## sorting or ordering tables by a column/variable

#---
# arrange()
#---

# arrange growth data by increasing weight
data_growth %>%
   arrange(weight)

   # base #
   data_growth[order(data_growth$weight), ]


# arrange by decreasing weight (using the desc() argument)
data_growth %>%
   arrange(desc(weight))

   # base #
   data_growth[order(data_growth$weight, decreasing=T), ]


##_NAs, missing values, and erroneous values

# convert erroneous or placeholder values to NA
   
#---
# na_if()
#---
   
# convert placeholder values of "-9999" in growth data to NA
data_growth = data_growth %>%
   na_if(-9999)
   

# convert missing or NA values to a specific value

#---
# replace_na()   (function from the 'tidyr' package (part of tidyverse))
#---

data_growth %>%
   replace_na(list(length = -9999, weight = -9999))

data_growth %>%
   replace_na(list(length = 0, weight = 0))


##_Adding new variables / columns to a data frame or altering existing variables
   
#---
# mutate()
#---
   
## add a new variable
# add a new column for animal body condition index (BCI = weight / (length)^3)
data_growth %>%
   mutate(BCI = weight / length^3)


## alter an existing variable
# convert precipitation data from millimeters to centimeters
data_site %>%
   mutate(precip = precip/10)
   

##_Computing summary values from data
   
## summarizing over an entire data frame
## i.e. compute a function over all rows in the data frame and return a single value (for each specified variable)
   
#---
# summarize()
#---

# calculate the mean temperature across all sites
data_site %>%
   summarize(mean(temp))


# calculate the mean temperature and precipitation across all sites
data_site %>%
   summarize(mean(temp), mean(precip))


## apply one function to multiple variables at the same time

# what is the mean value (across all animals) of the starting animal ages, lengths, and weights
data_animal %>%
   summarize_at(.vars = vars(age:start_weight),
                .funs = ~mean(., na.rm=T))

# calculate mean temperature and precipitation across all sites (as above)
data_site %>%
   summarize_at(.vars = vars(temp, precip),
                .funs = ~mean(., na.rm=T))


## apply multiple functions to multiple variables at the same time

# what are the mean and standard deviation (across all animals) of the starting animal ages, lengths, and weights
data_animal %>%
   summarize_at(.vars = vars(age:start_weight),
                .funs = list(~mean(., na.rm=T), ~sd(., na.rm=T)))


##_Work with groups/subsets of data rather than the entire data frame

## split data frame into groups based on some variable, apply function(s) to each group, and re-combine into a data frame

#---
# group_by()
#---

# count the number of individuals present of each animal species
data_animal %>%
   group_by(sp) %>%
   tally()

# count the number weight growth measurements for each individual excluding missing values
data_growth %>%
   filter(!(is.na(weight))) %>%
   group_by(animal_id) %>%
   tally()

# calculate the mean weight of all animals for each measurement day
data_growth %>%
   group_by(day) %>%
   summarize(mean(weight, na.rm=T))

# find the longest individual from each measurement day. Is it always the same individual?
data_growth %>%
   group_by(day) %>%
   filter(length == max(length, na.rm=T))


##_Join two data tables together

#---
# left_join()
# full_join()
#---

# add site description data to table of individual animal data 
left_join(data_animal, data_site, by = "site_id")

full_join(data_animal, data_site, by = "site_id")

# add data on the individual animals to the growth data
left_join(data_growth, data_animal, by = "animal_id") %>%
   arrange(site_id)

# add all three data tables together to be able to view all starting data, descriptive site data, and growth data together
left_join(data_growth, data_animal, by = "animal_id") %>%
   left_join(., data_site, by = "site_id") %>%
   arrange(animal_id)

left_join(data_growth, data_animal, by = "animal_id") %>%
   full_join(., data_site, by = "site_id") %>%
   arrange(animal_id)



###-----###

## Using dplyr functions to calculate growth rates from these data


# OPTION 1

# need to get starting length and weight data with growth data
# join the starting length and weight columns from data_animal to the data_growth table

# select only the needed columns
start = data_animal %>%
   select(animal_id, starts_with("start_"))

# join the tables together
gr_1 = left_join(data_growth, start, by="animal_id")

# calculate the mean daily growth rate for each animal from day 0 to the current measurement day
gr_1 = gr_1 %>%
   mutate(gr_length = (length - start_length) / day,
          gr_weight = (weight - start_weight) / day) %>%
   arrange(animal_id)


# OPTION 2

# add the starting length and weights of animals to the data_growth table as new data collected on day "0"
# i.e. add the data as new rows, rather than new columns

test = data_animal %>%
   # add a new "day" column for starting data
   mutate(day = rep(0, length(animal_id))) %>%
   # rename columns
   rename(length = start_length,
          weight = start_weight) %>%
   # select the desired columns
   select(animal_id, day, length, weight)

# combine the day-0 data with the growth data
gr_2 = bind_rows(data_growth, test)


### calculate the mean daily growth rate for each animal from day 0 to the current measurement day
gr_2_total = gr_2 %>%
   # arrange by measurement day and animal
   arrange(animal_id, day) %>%
   # compute within each individual animal
   group_by(animal_id) %>%
   # calculate growth rate based on the current measurement compared to the first measurement in the group (i.e. day 0)
   mutate(gr_length = (length - first(length)) / day,
          gr_weight = (weight - first(weight)) / day)


### calculate the growth rate for each growth interval
# i.e. the growth rate since the previous measurement
gr_2_each = gr_2 %>%
   arrange(animal_id, day) %>%
   group_by(animal_id) %>%
   # calculate growth rate based on the current measurement compared to the previous measurement
   mutate(gr_length = (length - lag(length)) / (day - lag(day)),
          gr_weight = (weight - lag(weight)) / (day - lag(day)))


