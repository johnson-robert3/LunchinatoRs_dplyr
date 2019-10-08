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

test= data_growth %>%
   replace_na(list(length = -9999, weight = -9999))



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
   
#---
# summarize()
#---

# calculate the mean temperature across all sites
data_site %>%
   summarize(mean(temp))


# calculate the mean temperature and precipitation across all sites
data_site %>%
   summarize(mean(temp), mean(precip))



