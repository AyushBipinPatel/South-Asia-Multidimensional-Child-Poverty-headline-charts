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


# Gender Equity Among Poor and Deprived Children  ----------------------------------------------

tibble(
  Country = c("Afghanistan","Bangladesh",
              "Bhutan", "India", "Maldives",
              "Nepal", "Pakistan"),
  boys_sh_age_mpi_poor_not_attn = c(24.8,12.1,8.7,
                                    6.1,0.1,3.1,19.7),
  
  girls_sh_age_mpi_poor_not_attn = c(44.0,7.2,7.8,6.8,
                                     0.1,6.0,27.2),
  diff_stat_sig_95_more = c(T,T,F,T,F,T,T),
  boy_ch_u5_mpi_poor_malnourisherd = c(NA,30.6,24.2,
                                       27.6,0.6,25.5,26.6),
  girl_ch_u5_mpi_poor_malnourisherd = c(NA,31.0,24.3,
                                        27.8,0.7,27,27.8)
) -> data_results_c


# Intrahousehold inequality -----------------------------------------------

tibble(
  Country = c("Afghanistan","Bangladesh",
              "Bhutan", "India", "Maldives",
              "Nepal", "Pakistan"),
  perc_exp_sch_attn_intrhh = c(34.3,12.7,9.9,
                               8.1,0.3,7.0,22.4),
  perc_exp_ch_ntr_intrhh = c(NA,12.1,14.7,21.8,
                             16.1,17.4,33.7)
)-> data_results_d
