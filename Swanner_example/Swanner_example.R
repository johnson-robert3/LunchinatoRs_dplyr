# Script for tidying Swanner example data

library(tidyverse)


# data
data_raw = read_csv("Swanner_example/GHM_R_data_raw.csv")

desired = read_csv("Swanner_example/GHM_R_bio.csv")


## Make the data longer

data = data_raw %>%
   # select all of the desired columns, including the organizing columns "date" and "depth"
   select(Date, Depth_m, chla_ugL, Turbidity_NTU, pH, d13C_DIC_permil) %>%
   # lengthen the data: column names will become a new single column (names), all data values will be put into a second column (values)
   pivot_longer(cols = chla_ugL:d13C_DIC_permil,
                # name of the new column created from column headers
                names_to = "Element",
                # name of the column created for data values
                values_to = "Value",
                # drop all rows where the data "value" is missing
                values_drop_na = T) %>%
   # sort the lengthened data by the new column created from column headers
   arrange(Element)


## Split the analytes and units into separate columns

data = data %>%
   # rename the DIC13 column, since it contains two underscores
   mutate(Element = replace(Element, .$Element=="d13C_DIC_permil", "d13CDIC_permil")) %>%
   # separate the character strings in the "Element" column at the underscore ("_")
   # units for pH will be populated with "NA" since there is no underscore in the Element name
   separate(col = Element,
            into = c("Element", "Unit"),
            sep = "_")


## Change the names of analytes and units to desired format

# can also use multiple calls to replace() rather than case_when()

data = data %>%
   mutate(Element = case_when(.$Element=="chla" ~ "chlorophyll_a",
                              .$Element=="Turbidity" ~ "turbidity",
                              .$Element=="d13CDIC" ~ "d13C_DIC",
                              .$Element=="pH" ~ "pH"),
          Unit = case_when(.$Unit=="ugL" ~ "ug_L",
                           .$Element=="pH" ~ "pH",
                           .$Unit=="permil" ~ "per_mil",
                           .$Unit=="NTU" ~ "NTU"))

