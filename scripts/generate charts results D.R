### Script to generate chart for the result:

### What Proportion of Poor and Deprived Children Experience Intrahousehold Inequalities?



# libs --------------------------------------------------------------------

library(here)
library(scales)
library(ggrepel)
library(ggtext)
library(gghighlight)
library(cowplot)

# sourcing data -----------------------------------------------------------

source(here("scripts/generate data.R"))


# The chart for school attendance intra-hh inequality

data_results_d %>% 
  mutate(
    Country = fct_reorder(
      as.factor(Country),
      perc_exp_sch_attn_intrhh
    )
  ) %>% 
  ggplot(aes(Country,perc_exp_sch_attn_intrhh))+
  geom_col(aes(fill = perc_exp_sch_attn_intrhh >= 11.2),
           alpha = 0.7)+
  geom_text(aes(
    label = paste(perc_exp_sch_attn_intrhh,
                  "%",
                  sep = "")
  ),
  nudge_y = 0.5,
  size = 6
            )+
  scale_fill_manual(values = c("#ffce99","#ff901a"))+
  guides(fill =F)+
  geom_hline(aes(yintercept = 11.2),
             colour = "#ff010d", 
             linetype = 3)+
  annotate(geom = "text",
           x = 3.5,
           y = 25,
           label = "In South Asia, overall, 11.2% of school-age children live in\nan MPI poor household with intrahousehold inequality in school attendance:\nat least one school-age child is attending school but another school-age child is not.",
           size = 6)+
  geom_curve(
    aes(
      x = 0.9,
      xend = 1.5,
      y = 11.2,
      yend = 25
    ),arrow = arrow(),
    curvature = -0.3
  )+
  scale_y_continuous(labels = scales::label_percent(scale = 1))+
  labs(
    x = NULL,
    y = "Percentage of school-age children residing in MPI poor HH with intrhousehold inequality in school attendance",
    title = "Proportion of Poor and Deprived Children facing intrahousehold inequalities in school attendance"
  )+
  theme(
    panel.background = element_rect(fill = "#FFFFFF",
                                    colour = NULL),
    plot.background = element_rect(fill = "#FFFFFF",
                                   colour = NULL),
    axis.line.x = element_line(colour = "grey"),
    panel.grid.major.y = element_line(linetype = "dotted",
                                      colour = "grey"),
    plot.title.position = "plot",
    plot.title = element_text(size = 15),
    axis.text.y = element_text(size = 14,face = "bold"),
    plot.subtitle = element_markdown(size = 14,lineheight = 1.5)
  ) -> chart_result_D_sch_attn


ggsave(
  here("charts/result_d_chart_intra_hh_ineq_sch_attn.png"),
  device = "png",width = 16,height = 10,units = "in")


# The chart for mal nutr intra-hh inequality

data_results_d %>% 
  mutate(
    Country = fct_reorder(
      as.factor(Country),
      perc_exp_ch_ntr_intrhh
    )
  ) %>% 
  ggplot(aes(Country,perc_exp_ch_ntr_intrhh))+
  geom_col(aes(fill = perc_exp_ch_ntr_intrhh >= 22.7),
           alpha = 0.7)+
  geom_text(aes(
    label = paste(perc_exp_ch_ntr_intrhh,
                  "%",
                  sep = "")
  ),
  nudge_y = 0.5,
  size = 6
  )+
  scale_fill_manual(values = c("#ffce99","#ff901a"))+
  guides(fill =F)+
  geom_hline(aes(yintercept = 22.7),
             colour = "#ff010d", 
             linetype = 3)+
  annotate(geom = "text",
           x = 3,
           y = 25,
           label = "In South Asia,considering both poor and non-poor children, a striking 22.7% of children aged 0-4 live in a\nhousehold riven by intrahousehold inequality in nutrition – in which some are and some are not malnourished.",
           size = 6)+
  geom_curve(
    aes(
      x = 0.35,
      xend = 0.8,
      y = 22.7,
      yend = 25
    ),arrow = arrow(),
    curvature = -0.3
  )+
  scale_y_continuous(labels = scales::label_percent(scale = 1))+
  scale_x_discrete(limits = c(
    "Bangladesh","Bhutan",
    "Maldives","Nepal",
    "India","Pakistan"
  ))+
  labs(
    x = NULL,
    y = "Percentage of children aged 0-4 residing in MPI poor HH with intrhousehold inequality in malnutrition",
    title = "Proportion of Poor and Deprived Children facing intrahousehold inequalities in malnutrition"
  )+
  theme(
    panel.background = element_rect(fill = "#FFFFFF",
                                    colour = NULL),
    plot.background = element_rect(fill = "#FFFFFF",
                                   colour = NULL),
    axis.line.x = element_line(colour = "grey"),
    panel.grid.major.y = element_line(linetype = "dotted",
                                      colour = "grey"),
    plot.title.position = "plot",
    plot.title = element_text(size = 15),
    axis.text.y = element_text(size = 14,face = "bold"),
    plot.subtitle = element_markdown(size = 14,lineheight = 1.5)
  )-> chart_result_D_malnutri


ggsave(
  here("charts/result_d_chart_intra_hh_ineq_malnutri.png"),
  device = "png",width = 16,height = 10,units = "in")

### Putting the images together



ggdraw()+
  draw_image(here("charts/result_d_chart_intra_hh_ineq_sch_attn.png")) -> c1


ggdraw()+
  draw_image(here("charts/result_d_chart_intra_hh_ineq_malnutri.png")) -> c2



plot_grid(c1,c2,align = "v",ncol = 1) -> grid_cols_charts

ggsave(here("charts/results_d_col_charts.png"),
       device = "png",
       width = 25,height = 29.7,units = "cm")


title <- ggdraw() + 
  draw_label(
    "What Proportion of Poor and Deprived Children Experience Intrahousehold Inequalities?",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

plot_grid(
  title, grid_cols_charts,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)

ggsave(here("charts/summary_chart_result_d.png"),
       device = "png",width = 12,height = 15, units = "in")

ggsave(here("charts/summary_chart_result_d.pdf"),
       device = "pdf",width = 12,height = 15, units = "in")
