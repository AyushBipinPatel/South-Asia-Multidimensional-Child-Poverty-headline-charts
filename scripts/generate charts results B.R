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


data_result_b %>% 
  mutate(
    Country = fct_reorder(
      as.factor(Country),
      perc_ch_04_maln_mpi_poor
    )
  ) %>% 
  ggplot(aes(Country,
             perc_ch_04_maln_mpi_poor))+
  geom_col(aes(fill = num_ch_04_maln))+
  geom_text(aes(label = paste(
    round(num_ch_04_maln/1000,2),
    "(K)"
  ),
  colour = num_ch_04_maln),
  nudge_y = 1)+
  geom_text(aes(label = paste(
    perc_ch_04_maln_mpi_poor,"(%)"
  )),
  nudge_y = 2.5)+
  scale_fill_gradient(low = "#8eb584" ,
                      high = "#ff010d")+
  scale_color_gradient(low = "#8eb584",
                       high = "#ff010d",
                       label = comma)+
  scale_y_continuous(labels = percent_format(scale = 1))+
  scale_x_discrete(limits = c("Maldives","Bhutan","Nepal",
                              "Pakistan","India","Bangladesh"))+
  guides(fill = FALSE,
         colour = guide_colorbar(
           title = "Number of children (aged 0-4) who are MPI poor and manourised",
           title.position = "top",
           barwidth = 20
           #label.position = "bottom"
         ))+
  labs(
    title = "MPI poor and malnourised children (aged 0-4) in South Asia",
    subtitle = "Number of MPI poor and malnourised children in <span style = 'color:#ff010d;'>India</span> __is equal to 2.44 times__ the sum of malnourised children <br>in <span style = 'color:#8eb584;'>Maldives</span>, <span style = 'color:#8eb584;'>Nepal</span>, <span style = 'color:#8eb584;'>Bhutan</span>, <span style = 'color:#ae9613;'>Pakistan</span> and <span style = 'color:#9ca442'>Bangladesh</span>",
    y = "MPI poor and Malnourised Children (%)",
    x = NULL
  )+
  theme(
    panel.background = element_rect(fill = "#ffffff"),
    panel.grid = element_line(colour = "grey"),
    legend.position = c(0.4,.94),
    legend.direction = "horizontal",
    legend.background = element_rect(fill = NA),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(size = 12),
    
  )+
  coord_cartesian(ylim = c(0,40))-> chart_ch_malnourish


ggsave(
  here("charts/result_b_chart_child_mpi_poor_and_malnourished.png"),
  device = "png",width = 14,height = 8,units = "in")


# putting the column charts together --------------------------------------


ggdraw()+
  draw_image(here("charts/result_b_chart_sch_age_mpi_poor_perc_notattn.png")) -> c1


ggdraw()+
  draw_image(here("charts/result_b_chart_sch_age_not_attn_lives_mpi_poor_hh.png")) -> c2

ggdraw()+
  draw_image(here("charts/result_b_chart_child_mpi_poor_and_malnourished.png")) -> c3


plot_grid(c1,c2,c3,align = "v",ncol = 1) -> grid_cols_charts

ggsave(here("charts/results_b_col_charts.png"),
       device = "png",
       width = 21,height = 29.7,units = "cm")

# waffles for results B

tibble(
  part = c("Lives with Out of School child","Does not live with Out of School child"),
  val  = c(1,9)
) %>% 
  ggplot(aes(fill = part, values= val),size = 0.25)+
  geom_waffle(n_rows = 3,
              colour = "white",size = 10)+
  scale_fill_manual(values = c("#8eb584","#ff010d"),name = "A person in South Asia")+
  guides(fill = guide_legend(title.position = "top"))+
  labs(
    title = "<span style = 'color:#ff010d'> One </span>in ten people in South Asia share their household with a<br>out of school child."
  )+
  theme(
    plot.background = element_rect(fill = "#ffffff"),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title.position = "plot",
    plot.title = element_markdown(),
    plot.subtitle = element_markdown(size = 12),
    legend.background = element_blank(),
    legend.position = "top",
    
  ) -> waffle_lives_with_out_sch_ch 

ggsave(here("charts/waffle_lives_with_out_sch_ch.png"),
       device = "png",width = 6, height = 6, units = "in")


ggdraw()+
  draw_image(here("charts/waffle_lives_with_out_sch_ch.png")) -> w1


tibble(
  part = c("Lives in MPI poor household","Does not live in MPI poor household"),
  val  = c(88,12)
) %>% 
  ggplot(aes(fill = part, values= val),size = 0.25)+
  geom_waffle(n_rows = 10,
              colour = "white",size = 10)+
  scale_fill_manual(values = c("#8eb584","#ff010d"),name = "Our of School Child")+
  guides(fill = guide_legend(title.position = "top"))+
  labs(
    title = "<span style = 'color:#ff010d'> 88% (32.3 million)</span> of out of school children live in MPI poor houeshold."
  )+
  theme(
    plot.background = element_rect(fill = "#ffffff"),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title.position = "plot",
    plot.title = element_markdown(),
    plot.subtitle = element_markdown(size = 12),
    legend.background = element_blank(),
    legend.position = "top",
    
  ) -> waffle_ch_lives_in_mpi_poor_hh

ggsave(here("charts/ch_lives_in_mpi_poor_hh.png"),
       device = "png",width = 6, height = 6, units = "in")


ggdraw()+
  draw_image(here("charts/ch_lives_in_mpi_poor_hh.png")) -> w2

plot_grid(w1,w2,align = "h",ncol = 2) -> grid_waffle_charts
