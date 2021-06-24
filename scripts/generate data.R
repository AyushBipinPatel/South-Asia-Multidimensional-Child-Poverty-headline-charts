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



# What proportion of deprived children live in HH that are MPI Poor -------


tibble(
  Country = c("Afghanistan","Bangladesh",
              "Bhutan", "India", "Maldives",
              "Nepal", "Pakistan"),
  perc_sch_age_ch_mpi_poor_not_attn = c(34,9.7,8.3,
                               6.5,0.1,4.5,23.4),
  num_sch_age_ch__mpi_poor_not_attn =c(3111348,3334075,13505,
                             15248224,100,286764,10339014),
  perc_sch_age_ch_not_attn_sch_live_mpi_poor_hh = c(90.1,85.0,
                                                    81.7,87.5,12.3,
                                   89.7,89.2),
  perc_ch_04_maln_mpi_poor = c(NA,30.8,24.2,27.7,
                      0.6,26.3,27.2),
  num_ch_04_maln = c(NA,5070224,18624,32250744,
                     285,792234,7296573)
) -> data_result_b
