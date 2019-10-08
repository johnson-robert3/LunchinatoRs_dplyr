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



