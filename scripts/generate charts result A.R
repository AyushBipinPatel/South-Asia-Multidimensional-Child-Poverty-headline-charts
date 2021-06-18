### Script to generate chart for the result:

### A. Age-specific Child Deprivation Levels in Nutrition and School Attendance


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

### School Age children not attending school

data_result_a %>% 
  mutate(
    Country = fct_reorder(
      as.factor(Country),
      perc_sch_age_ch_not_attn
    )
  ) %>% 
  ggplot(aes(Country,
             perc_sch_age_ch_not_attn))+
  geom_col(aes(fill = num_sch_age_ch_not_attn))+
  geom_text(aes(label = paste(
    round(num_sch_age_ch_not_attn/1000,2),
    "(K)"
  ),
  colour = num_sch_age_ch_not_attn),
  nudge_y = 1)+
  geom_text(aes(label = paste(
    perc_sch_age_ch_not_attn,"(%)"
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
           title = "Number of school age children not attending school",
           title.position = "top",
           barwidth = 18
           #label.position = "bottom"
         ))+
  labs(
    title = "School Attendance in South Asia",
    subtitle = "The __number of school age children not attending school__ in <span style = 'color:#ff010d;'>India</span> is __more than the <br>sum of number of school age children not attending school__ in <span style = 'color:#8eb584;'>Maldives</span>, <span style = 'color:#8eb584;'>Nepal</span>, <br><span style = 'color:#8eb584;'>Bhutan</span>, <span style = 'color:#e37400;'>Pakistan</span> and <span style = 'color:#c7a52c;'>Afghanistan</span>",
    y = "School age children not attending school (%)",
    x = NULL
  )+
  theme(
    panel.background = element_rect(fill = "#ffffff"),
    panel.grid = element_line(colour = "grey"),
    legend.position = c(0.5,.9),
    legend.direction = "horizontal",
    legend.background = element_rect(fill = NA),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(size = 12)
  ) -> chart_sch_attn1


ggsave(
  here("charts/result_a_chart_sch_attn_perc_notattn.png"),
  device = "png",width = 12,height = 8,units = "in")

### Percentage of Population living with a child who is not attending school

data_result_a %>% 
  mutate(
    Country = fct_reorder(
      as.factor(Country),
      perc_pop_liv_ch_not_attn_sch
    )
  ) %>% 
  ggplot(aes(Country,perc_pop_liv_ch_not_attn_sch))+
  geom_col(fill = "#303960")+
  geom_text(aes(label = paste(
    perc_pop_liv_ch_not_attn_sch,
    "(%)"
  )),
  nudge_y = 1.5)+
  labs(
    title = "School Attendance in South Asia",
    subtitle = "Percentage of Population living with a child not attending school",
    y = "Population living with a child not attending school (%)",
    x = NULL
  )+
  theme(
    panel.background = element_rect(fill = "#ffffff"),
    panel.grid = element_line(colour = "grey"),
    legend.position = c(0.5,.9),
    legend.direction = "horizontal",
    legend.background = element_rect(fill = NA),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(size = 12)
  ) -> chart_sch_attn2

ggsave(
  here("charts/result_a_chart_perc_pop_living_with_ch_not_sch_attn.png"),
  device = "png",width = 12,height = 8,units = "in")


### Malnourished children


data_result_a %>% 
  mutate(
    Country = fct_reorder(
      as.factor(Country),
      perc_ch_04_maln
    )
  ) %>% 
  ggplot(aes(Country,
             perc_ch_04_maln))+
  geom_col(aes(fill = num_ch_04_maln))+
  geom_text(aes(label = paste(
    round(num_ch_04_maln/1000,2),
    "(K)"
  ),
  colour = num_ch_04_maln),
  nudge_y = 1)+
  geom_text(aes(label = paste(
    perc_ch_04_maln,"(%)"
  )),
  nudge_y = 2.5)+
  scale_fill_gradient(low = "#8eb584" ,
                      high = "#ff010d")+
  scale_color_gradient(low = "#8eb584",
                       high = "#ff010d",
                       label = comma)+
  scale_y_continuous(labels = percent_format(scale = 1))+
  scale_x_discrete(limits = c("Maldives","Bhutan","Nepal",
                              "Pakistan","Bangladesh","India"))+
  guides(fill = FALSE,
         colour = guide_colorbar(
           title = "Number of Malnourised children (aged 0-4)",
           title.position = "top",
           barwidth = 20
           #label.position = "bottom"
         ))+
  labs(
    title = "Child (aged 0-4) Nutrition in South Asia",
    subtitle = "Number of malnourised children in <span style = 'color:#ff010d;'>India</span> __is equal to 2.83 times__ the sum of malnourised children <br>in <span style = 'color:#8eb584;'>Maldives</span>, <span style = 'color:#8eb584;'>Nepal</span>, <span style = 'color:#8eb584;'>Bhutan</span>, <span style = 'color:#ae9613;'>Pakistan</span> and <span style = 'color:#9ca442'>Bangladesh</span>",
    y = "Malnourised Children (%)",
    x = NULL
  )+
  theme(
    panel.background = element_rect(fill = "#ffffff"),
    panel.grid = element_line(colour = "grey"),
    legend.position = c(0.4,.94),
    legend.direction = "horizontal",
    legend.background = element_rect(fill = NA),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(size = 12)
  ) +
  coord_cartesian(ylim = c(0,50))-> chart_ch_malnourish


ggsave(
  here("charts/result_a_chart_child_nutrition.png"),
  device = "png",width = 12,height = 8,units = "in")


ggdraw()+
  draw_image(here("charts/result_a_chart_sch_attn_perc_notattn.png")) -> c1


ggdraw()+
  draw_image(here("charts/result_a_chart_perc_pop_living_with_ch_not_sch_attn.png")) -> c2

ggdraw()+
  draw_image(here("charts/result_a_chart_child_nutrition.png")) -> c3


plot_grid(c1,c2,c3,align = "v",ncol = 1)

ggsave(here("charts/results_a_col_charts"),
       device = "png",
       width = 21,height = 29.7,units = "cm")
