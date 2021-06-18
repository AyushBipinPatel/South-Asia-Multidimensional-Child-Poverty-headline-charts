### Generating the data for the Visualizations ###


# libr --------------------------------------------------------------------

library(here)
library(tidyverse)



# Data for: Age-specific Child Deprivation Levels in Nutrition and School Attendance  --------------------------------------------------------------

tibble(
  Country = c("Afghanistan","Bangladesh",
              "Bhutan", "India", "Maldives",
              "Nepal", "Pakistan"),
  perc_sch_age_ch_not_attn = c(37.7,11.4,10.1,
                               7.4,1.0,5.0,26.3),
  num_sch_age_ch_not_attn =c(3455991,3922776,16537,
                             17431407,751,319730,11592612),
  perc_pop_liv_ch_not_attn_sch = c(48.7,11.2,10.8,6.4,1.3,
                                   5.5,28.5),
  perc_ch_04_maln = c(NA,39.7,33.5,44.2,
                      18.6,37.9,39.1),
  num_ch_04_maln = c(NA,6540001,25792,51508525,
                     8239,1143351,10480891)
) -> data_result_a
