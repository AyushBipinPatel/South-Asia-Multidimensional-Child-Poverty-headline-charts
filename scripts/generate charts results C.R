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
