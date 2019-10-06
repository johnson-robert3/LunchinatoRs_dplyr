# Practice script for fake growth data
# Script by Robert Johnson


library(tidyverse)


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


# View data only for species "tt"
data_animal %>%
   filter(sp=="tt")


# View data for all animals older than age 1
data_animal %>%
   filter(age>1)


# View the length and weight data for animals on the final measurement day

   # what is the value of the final measurement day?
   data_growth %>% pull(day) %>% unique()

data_growth %>%
   filter(day==38)


## selecting data by columns

#---
# select()
#---

# select only the desired columns/variables
data_site %>%
   select(site_id, region, veg_type)

# remove and undesired column, and keep the rest
data_site %>%
   select(-veg_type)

# select a range of adjacent columns
data_site %>%
   select(lat:veg_type)

# remove a range of adjacent columns
data_site %>%
   select(-lat:-long)


## sorting or ordering tables by a column/variable

#---
# arrange()
#---

# arrange growth data by increasing weight
data_growth %>%
   arrange(weight)

# arrange by decreasing weight (using the desc() argument)
data_growth %>%
   arrange(desc(weight))




