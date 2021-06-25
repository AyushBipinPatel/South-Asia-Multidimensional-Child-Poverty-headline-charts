### Script to generate chart for the result:

### C. Gender Equity Among Poor and Deprived Children



# libs --------------------------------------------------------------------

library(here)
library(scales)
library(ggrepel)
library(ggtext)
library(cowplot)

# sourcing data -----------------------------------------------------------

source(here("scripts/generate data.R"))



# generating the charts ---------------------------------------------------

### School attendance and MPI poor children - gender difference


data_results_c %>% 
  mutate(
    Country = fct_reorder(
      as.factor(Country),
      (boys_sh_age_mpi_poor_not_attn - girls_sh_age_mpi_poor_not_attn)^2
    )
  ) %>% 
  ggplot()+
  geom_segment(aes(x = girls_sh_age_mpi_poor_not_attn,
                   xend = boys_sh_age_mpi_poor_not_attn,
                   y = Country,
                   yend = Country,
                   alpha = diff_stat_sig_95_more),
               size = 3, 
               colour = "#808080")+
  scale_alpha_discrete(range = c(0.25,1))+
  geom_point(aes(boys_sh_age_mpi_poor_not_attn,
                 Country,
                 colour = ifelse(.data$Country == "Maldives",
                                 "#BC923C",
                                 "#8eb584")), 
             alpha = 1,
             size = 6)+
  geom_text(aes(boys_sh_age_mpi_poor_not_attn,
                      Country,
                      label = paste(
                        boys_sh_age_mpi_poor_not_attn,
                        "%"
                      )
                      ),
                  colour = "#8eb584",
                  nudge_y = -0.25)+
  geom_point(aes(girls_sh_age_mpi_poor_not_attn,
                 Country,
                 colour = ifelse(.data$Country == "Maldives",
                                 "#BC923C",
                                 "#e37400")), 
             alpha = 1,
             size = 6)+
  geom_text(aes(girls_sh_age_mpi_poor_not_attn,
                      Country,
                      label = paste(
                        girls_sh_age_mpi_poor_not_attn,
                        "%"
                      )
  ),
  colour = "#e37400",
  nudge_y = 0.25)+
  scale_colour_manual(values = c("#8eb584","#BC923C","#e37400"))+
  guides(colour = FALSE, alpha = FALSE)+
  scale_x_continuous(labels = scales::label_percent(scale = 1))+
  geom_richtext(aes(x = 37, 
                    y = 5,
                    label = "A <span style = 'color:#666666;'>**Dark Grey**</span> line show that the difference between <span style = 'color:#8eb584;'>__Boys__</span><br> and <span style = 'color:#e37400;'>__Girls__</span> is statiscally significant at 95% or more."),
                color = "#4b3439", fill = NA,
                label.color = NA,
                size = 5.5)+
  geom_curve(aes(x = 9, 
                 xend = 28,
                 y = 5, yend = 4.9),arrow = arrow(),
             curvature = 0.25 )+
  geom_richtext(aes(x = 37, 
                    y = 3,
                    label = "A <span style = 'color:#cccccc;'>**Light Grey**</span> line show that the difference between <span style = 'color:#8eb584;'>__Boys__</span><br> and <span style = 'color:#e37400;'>__Girls__</span> **is not** statiscally significant at 95% or more."),
                color = "#4b3439", fill = NA,
                label.color = NA,
                size = 5.5)+
  geom_curve(aes(x = 8.3, 
                 xend = 28,
                 y = 3, yend = 2.9),arrow = arrow(),
             curvature = 0.25 )+
  labs(
    x = "School age children who are MPI poor and not attending school (%)",
    y = NULL,
    title = "Gender Equity among MPI poor and Out of School Children",
    subtitle = "<br><span style = 'color:#8eb584;'>__Boys__</span> are represented by <span style = 'color:#8eb584'>__Green__</span>. <br><span style = 'color:#e37400;'>__Girls__</span> are represented by <span style = 'color:#e37400;'>__Orange__</span>"
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
  ) -> chart_gen_equity_sch_attn


ggsave(
  here("charts/result_c_chart_gen_equity_sch_attn.png"),
  device = "png",width = 16,height = 8,units = "in")



### Children under 5 years of age who are MPI poor and malnourished


data_results_c %>% 
  mutate(
    Country = fct_reorder(
      as.factor(Country),
    (boy_ch_u5_mpi_poor_malnourisherd - girl_ch_u5_mpi_poor_malnourisherd)^2
    )
  ) %>% 
  ggplot()+
  geom_segment(aes(x = girl_ch_u5_mpi_poor_malnourisherd,
                   xend = boy_ch_u5_mpi_poor_malnourisherd,
                   y = Country,
                   yend = Country),
               size = 3, 
               colour = "#808080")+
  geom_point(aes(boy_ch_u5_mpi_poor_malnourisherd,
                 Country,
                 colour = ifelse(.data$Country == "Maldives",
                                 "#BC923C",
                                 "#8eb584")), 
             alpha = 1,
             size = 6)+
  geom_text(aes(boy_ch_u5_mpi_poor_malnourisherd,
                Country,
                label = paste(
                  boy_ch_u5_mpi_poor_malnourisherd,
                  "%"
                )
  ),
  colour = "#8eb584",
  nudge_y = -0.25)+
  geom_point(aes(girl_ch_u5_mpi_poor_malnourisherd,
                 Country,
                 colour = ifelse(.data$Country == "Maldives",
                                 "#BC923C",
                                 "#e37400")), 
             alpha = 1,
             size = 6)+
  geom_text(aes(girl_ch_u5_mpi_poor_malnourisherd,
                Country,
                label = paste(
                  girl_ch_u5_mpi_poor_malnourisherd,
                  "%"
                )
  ),
  colour = "#e37400",
  nudge_y = 0.25)+
  scale_colour_manual(values = c("#8eb584","#BC923C","#e37400"))+
  guides(colour = FALSE, alpha = FALSE)+
  scale_x_continuous(labels = scales::label_percent(scale = 1))+
  geom_richtext(aes(x = 8, y = 5.5,
                    label = "Gender inequality in child nutrition is less severe<br> 
                    than in school attendance"),
                label.colour = NA,
                fill = NA,
                size = 6.5)+
  scale_y_discrete(limits = c( "Maldives","Bhutan",
                               "India","Bangladesh",
                               "Pakistan","Nepal"
                              ))+
  labs(
    x = "Children (aged 0 - 4) who are MPI poor and malnourised (%)",
    y = NULL,
    title = "Gender Equity among MPI poor and malnourished children (aged 0 - 4 years)",
    subtitle = "<br><span style = 'color:#8eb584;'>__Boys__</span> are represented by <span style = 'color:#8eb584'>__Green__</span>. <br><span style = 'color:#e37400;'>__Girls__</span> are represented by <span style = 'color:#e37400;'>__Orange__</span>"
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
  ) -> chart_gen_equity_mal_nutri


ggsave(
  here("charts/result_c_chart_gen_equity_mal_nutri.png"),
  device = "png",width = 16,height = 8,units = "in")


### Putting the images together



ggdraw()+
  draw_image(here("charts/result_c_chart_gen_equity_sch_attn.png")) -> c1


ggdraw()+
  draw_image(here("charts/result_c_chart_gen_equity_mal_nutri.png")) -> c2



plot_grid(c1,c2,align = "v",ncol = 1) -> grid_cols_charts

ggsave(here("charts/results_b_col_charts.png"),
       device = "png",
       width = 25,height = 29.7,units = "cm")


title <- ggdraw() + 
  draw_label(
    "Gender Equity Among Poor and Deprived Children",
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

ggsave(here("charts/summary_chart_result_c.png"),
       device = "png",width = 12,height = 15, units = "in")

ggsave(here("charts/summary_chart_result_c.pdf"),
       device = "pdf",width = 12,height = 15, units = "in")
