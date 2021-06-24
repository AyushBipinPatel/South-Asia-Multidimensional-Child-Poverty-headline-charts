### Script to generate chart for the result:

### B. What Proportion of Deprived Children Live in Households That Are MPI Poor?



# libs --------------------------------------------------------------------

library(here)
library(scales)
library(waffle)
library(ggrepel)
library(ggtext)
library(cowplot)

# sourcing data -----------------------------------------------------------

source(here("scripts/generate data.R"))



# generating the charts ---------------------------------------------------

### School aged children who are MPI poor and do not attend school

data_result_b %>% 
  mutate(
    Country = fct_reorder(
      as.factor(Country),
      perc_sch_age_ch_mpi_poor_not_attn
    )
  ) %>% 
  ggplot(aes(Country,
             perc_sch_age_ch_mpi_poor_not_attn))+
  geom_col(aes(fill = num_sch_age_ch__mpi_poor_not_attn))+
  geom_text(aes(label = paste(
    round(num_sch_age_ch__mpi_poor_not_attn/1000,2),
    "(K)"
  ),
  colour = num_sch_age_ch__mpi_poor_not_attn),
  nudge_y = 1)+
  geom_text(aes(label = paste(
    perc_sch_age_ch_mpi_poor_not_attn,"(%)"
  )),
  nudge_y = 2.3)+
  scale_fill_gradient(low = "#8eb584" ,
                      high = "#ff010d")+
  scale_color_gradient(low = "#8eb584",
                       high = "#ff010d",
                       label = comma)+
  scale_y_continuous(labels = percent_format(scale = 1))+
  guides(fill = FALSE,
         colour = guide_colorbar(
           title = "Number of school age children who are \nMPI poor and not attending school",
           title.position = "top",
           barwidth = 20
           #label.position = "bottom"
         ))+
  labs(
    title = "Out-of-School children in South Asia",
    subtitle = "There are __more school age children who are MPI poor and not attending school__<br>in <span style = 'color:#ff010d;'>India</span> than in <span style = 'color:#8eb584;'>Maldives</span>, <span style = 'color:#8eb584;'>Nepal</span>, <span style = 'color:#8eb584;'>Bhutan</span>, <span style = 'color:#e37400;'>Pakistan</span> and <span style = 'color:#c7a52c;'>Bangladesh</span> put together.",
    y = "School age children who are MPI poor and not attending school (%)",
    x = NULL
  )+
  theme(
    panel.background = element_rect(fill = "#ffffff"),
    panel.grid = element_line(colour = "grey"),
    legend.position = c(0.5,.9),
    legend.direction = "horizontal",
    legend.background = element_rect(fill = NA),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(size = 12),
    
  )-> chart_sch_attn_mpi_poor1


ggsave(
  here("charts/result_b_chart_sch_age_mpi_poor_perc_notattn.png"),
  device = "png",width = 14,height = 8,units = "in")


### Percentage of school age children not attending school who live in MPI Poor HH



data_result_b %>% 
  mutate(
    Country = fct_reorder(
      as.factor(Country),
      perc_sch_age_ch_not_attn_sch_live_mpi_poor_hh
    )
  ) %>% 
  ggplot(aes(Country,perc_sch_age_ch_not_attn_sch_live_mpi_poor_hh))+
  geom_col(fill = "#303960")+
  geom_text(aes(label = paste(
    perc_sch_age_ch_not_attn_sch_live_mpi_poor_hh,
    "(%)"
  )),
  nudge_y = 1.5)+
  coord_cartesian(ylim = c(0,100))+
  labs(
    title = "Out-of-School children in South Asia",
    subtitle = "School age children not attending school who live in MPI poor households",
    y = "School age children not attending school who live in MPI poor households (%)",
    x = NULL
  )+
  theme(
    panel.background = element_rect(fill = "#ffffff"),
    panel.grid = element_line(colour = "grey"),
    legend.position = c(0.5,.9),
    legend.direction = "horizontal",
    legend.background = element_rect(fill = NA),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(size = 12),
    
  ) -> chart_sch_age_not_attn_lives_mpi_poor_hh

ggsave(
  here("charts/result_b_chart_sch_age_not_attn_lives_mpi_poor_hh.png"),
  device = "png",width = 14,height = 8,units = "in")

### Malnourished and MPI poor

