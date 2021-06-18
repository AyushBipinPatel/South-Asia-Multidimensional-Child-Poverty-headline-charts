### Script to generate chart for the result:

### A. Age-specific Child Deprivation Levels in Nutrition and School Attendance


# libs --------------------------------------------------------------------

library(here)
library(scales)
library(waffle)
library(ggrepel)
library(ggtext)
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
    y = "Schoolage children not attending school (%)",
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
  device = "png",width = 8,height = 8,units = "in")

