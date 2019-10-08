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

   # base
   data_site[data_site$region=="north", ]


# View data only for species "tt"
data_animal %>%
   filter(sp=="tt")

   # base
   data_animal[data_animal$sp=="tt", ]


# View data for all animals older than age 1
data_animal %>%
   filter(age>1)

   # base
   data_animal[data_animal$age>1, ]


# View length and weight data for animals on the final measurement day

   #---
   # what is the value of the final measurement day?
   data_growth %>% pull(day) %>% unique()
   
      #base
      unique(data_growth[["day"]])
   #---

data_growth %>%
   filter(day==38)

   # base
   data_growth[data_growth$day==38, ]


## selecting data by columns

#---
# select()
#---

# select only the desired columns/variables
data_site %>%
   select(site_id, region, veg_type)

   # base
   data_site[ , c("site_id", "region", "veg_type")]


# remove and undesired column, and keep the rest
data_site %>%
   select(-veg_type)

   # base
   subset(data_site, select = -veg_type)


# select a range of adjacent columns
data_site %>%
   select(lat:veg_type)

   # base
   subset(data_site, select = lat:veg_type)


# remove a range of adjacent columns
data_site %>%
   select(-lat:-long)

   # base
   subset(data_site, select = -lat:-long)


## sorting or ordering tables by a column/variable

#---
# arrange()
#---

# arrange growth data by increasing weight
data_growth %>%
   arrange(weight)

   # base
   data_growth[order(data_growth$weight), ]


# arrange by decreasing weight (using the desc() argument)
data_growth %>%
   arrange(desc(weight))

   # base
   data_growth[order(data_growth$weight, decreasing=T), ]




