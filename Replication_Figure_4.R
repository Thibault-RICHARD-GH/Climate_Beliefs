############################
############ Figure 3 ######
############################

### Define folder
setwd(".")

# Alternatively, define the folder manually. Ex: 
# folder <- "~/Documents/Project Capable - Code and picture"
# setwd(folder)

# Make sure that the code is in the same folder as the dataset

# The Kullback_Leibler distance is used to find the "best-fitting" 
# normal distribution at an aggregated level (in Figure 1 a)
Kullback_Leibler <- function(mu, log_sigma, xa, xb, xc, xd){
  
  sigma <- exp(log_sigma)
  
  pi_1 <- (pnorm((32.5-mu)/sigma) - pnorm((0-mu)/sigma))/(
    pnorm((100-mu)/sigma) - pnorm((0-mu)/sigma))
  pi_2 <- (pnorm((45-mu)/sigma) - pnorm((32.5-mu)/sigma))/(
    pnorm((100-mu)/sigma) - pnorm((0-mu)/sigma))
  pi_3 <- (pnorm((55 - mu)/sigma) - pnorm((45 - mu)/sigma))/(
    pnorm((100-mu)/sigma) - pnorm((0-mu)/sigma))
  pi_4 <- (pnorm((100-mu)/sigma) - pnorm((55 - mu)/sigma))/(
    pnorm((100-mu)/sigma) - pnorm((0-mu)/sigma))
  
  pi_a <- xa/100
  pi_b <- xb/100
  pi_c <- xc/100
  pi_d <- xd/100
  
  contri_a <- ifelse(xa==0, 0, (pi_a)*log(pi_a))
  contri_b <- ifelse(xb==0, 0, (pi_b)*log(pi_b))
  contri_c <- ifelse(xc==0, 0, (pi_c)*log(pi_c))
  contri_d <- ifelse(xd==0, 0, (pi_d)*log(pi_d))
  
  entropy <- contri_a + contri_b + contri_c + contri_d
  
  sum_P_x_log_Q_x <- pi_a*log(pi_1) + pi_b*log(pi_2) + pi_c*log(pi_3) + pi_d*log(pi_4)
  
  KL <- entropy - sum_P_x_log_Q_x + 10000*(mu< 0) + 10000*(mu>100) + 10000*(sigma<0.01)
  
  #### the sum "10000*(mu< 0) + 10000*(mu>100) + 10000*(sigma<0.01)" implicitly
  #### imposes constraints on the values of mu and sigma in the minimization program
  
  KL
  
}


### Packages used

{
  
  #install.packages("dplyr")
  #install.packages("stringr")
  #install.packages("tidyr")
  #install.packages("haven")
  #install.packages("readr")
  #install.packages("ggplot2")
  #install.packages("dplyr")
  #install.packages("haven")
  #install.packages("ggtext")
  #install.packages("stringr")
  #install.packages("grid")
  #install.packages("gridExtra")
  #install.packages("ggsignif")
  #install.packages("tidyverse")
  #install.packages("broom")
  
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(haven)
  library(readr)
  library(ggplot2)
  library(dplyr)
  library(haven)
  library(ggtext)
  library(stringr)
  library(grid)
  library(gridExtra)
  library(ggsignif)
  library(tidyverse)
  library(broom)
  
}

### Library

{
  
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(haven)
  library(readr)
  library(ggplot2)
  library(dplyr)
  library(haven)
  library(ggtext)
  library(stringr)
  library(grid)
  library(gridExtra)
  library(ggsignif)
  library(tidyverse)
  library(broom)
  
}

### Import relevant datasets 

### General dataset
  dt <- read_csv("General_population_cleaned.csv")
### Stakeholders
  dt_sh <- read_csv("Stakeholders_cleaned.csv")
  
  #
  
  dt_overall <- data.frame(vect_mean=c(mean(dt$bel_2030_1),
                                       mean(dt$bel_2030_2),
                                       mean(dt$bel_2030_3),
                                       mean(dt$bel_2030_4)),
                           vect_sd=c(sd(dt$bel_2030_1),
                                     sd(dt$bel_2030_2),
                                     sd(dt$bel_2030_3),
                                     sd(dt$bel_2030_4)),
                           vect_n=c(length(dt$bel_2030_1),
                                    length(dt$bel_2030_2),
                                    length(dt$bel_2030_3),
                                    length(dt$bel_2030_4)),
                           cat = c("2022 levels or less  <br> (≤32.5%)",
                                   "Small extra reduction <br> (32.5%, 45%]", 
                                   "Moderate extra reduction <br> (45%, 55%)", 
                                   "European Climate Law <br> (≥55%)"),
                           cat2 = c(1, 2, 3, 4),
                           cat3 = rep("General Public", 4),
                           group_order = rep(1, 4))
  
  dt_overall$min95 <- dt_overall$vect_mean - 1.96*dt_overall$vect_sd/sqrt(dt_overall$vect_n)
  dt_overall$max95 <- dt_overall$vect_mean + 1.96*dt_overall$vect_sd/sqrt(dt_overall$vect_n)
  
  # Stakeholders' second order beliefs
  dt_temp3 <- data.frame(vect_mean=c(mean(dt_sh$sec_bel_2030_1),
                                     mean(dt_sh$sec_bel_2030_2),
                                     mean(dt_sh$sec_bel_2030_3),
                                     mean(dt_sh$sec_bel_2030_4)),
                         vect_sd=c(sd(dt_sh$sec_bel_2030_1),
                                   sd(dt_sh$sec_bel_2030_2),
                                   sd(dt_sh$sec_bel_2030_3),
                                   sd(dt_sh$sec_bel_2030_4)),
                         vect_n=c(length(dt_sh$sec_bel_2030_1),
                                  length(dt_sh$sec_bel_2030_2),
                                  length(dt_sh$sec_bel_2030_3),
                                  length(dt_sh$sec_bel_2030_4)),
                         cat = c("2022 levels or less  <br> (≤32.5%)",
                                 "Small extra reduction <br> (32.5%, 45%]", 
                                 "Moderate extra reduction <br> (45%, 55%)", 
                                 "European Climate Law <br> (≥55%)"),
                         cat2 = c(1, 2, 3, 4),
                         cat3 = rep("Stakeholders' second-order beliefs", 4),
                         group_order = rep(2, 4))
  
  dt_temp3$min95 <- dt_temp3$vect_mean - 1.96*dt_temp3$vect_sd/sqrt(dt_temp3$vect_n)
  dt_temp3$max95 <- dt_temp3$vect_mean + 1.96*dt_temp3$vect_sd/sqrt(dt_temp3$vect_n)
  
  # Stakeholders  
  dt_temp2 <- data.frame(vect_mean=c(mean(dt_sh$bel_2030_1),
                                     mean(dt_sh$bel_2030_2),
                                     mean(dt_sh$bel_2030_3),
                                     mean(dt_sh$bel_2030_4)),
                         vect_sd=c(sd(dt_sh$bel_2030_1),
                                   sd(dt_sh$bel_2030_2),
                                   sd(dt_sh$bel_2030_3),
                                   sd(dt_sh$bel_2030_4)),
                         vect_n=c(length(dt_sh$bel_2030_1),
                                  length(dt_sh$bel_2030_2),
                                  length(dt_sh$bel_2030_3),
                                  length(dt_sh$bel_2030_4)),
                         cat =c("2022 levels or less  <br> (≤32.5%)",
                                "Small extra reduction <br> (32.5%, 45%]", 
                                "Moderate extra reduction <br> (45%, 55%)", 
                                "European Climate Law <br> (≥55%)"),
                         cat2 = c(1, 2, 3, 4),
                         cat3 = rep("Stakeholders", 4),
                         group_order = rep(3, 4))
  
  dt_temp2$min95 <- dt_temp2$vect_mean - 1.96*dt_temp2$vect_sd/sqrt(dt_temp2$vect_n)
  dt_temp2$max95 <- dt_temp2$vect_mean + 1.96*dt_temp2$vect_sd/sqrt(dt_temp2$vect_n)
  
  dt_overall <- rbind(dt_overall, dt_temp3, dt_temp2)
  
  dt_overall$cat3 <- factor(dt_overall$cat3, 
                            levels = c("Stakeholders",
                                       "Stakeholders' second-order beliefs",
                                       "General Public"))
  stakeholder_alpha <- c("General Public" = 0.8, "Experts" = 0.5, "Policymakers" = 0.3)
  # install.packages("ggpattern")
  library(ggpattern)
  
  
  p3 <- ggplot(dt_overall, aes(x = reorder(cat, cat2), y = vect_mean, fill = cat3)) +
    # Add grid lines first (at the back)
    geom_hline(yintercept = seq(0, 50, by = 10), color = "grey90", linetype = "dashed") +
    # Add the bars with position dodge
    geom_col(aes(alpha = cat3), position = position_dodge(0.9)) + 
    # Add alpha for the middle group only
    
    scale_alpha_manual(values = c("General Public" = 0.8, 
                                  "Stakeholders' second-order beliefs" = 0.3, 
                                  "Stakeholders" = 0.4),
                       guide = "none") +
    # Add bar outlines
    geom_col(position = position_dodge(0.9), alpha = 0.3, color = "black", size = 0.2) +
    scale_fill_manual(name = " ",
                      values = c("General Public" = "#3366CC", 
                                 "Stakeholders' second-order beliefs" = "#99CCFF", 
                                 "Stakeholders" = "#006633")) +
    # Add error bars on top
    geom_errorbar(aes(ymin = min95, ymax = max95), width = 0.2, 
                  position = position_dodge(0.9)) +
    # Customize theme
    theme_classic() +
    theme(strip.placement = "outside",
          plot.title   = element_text(face = "bold", size = 12),
          axis.text.x = element_markdown(angle = 0, hjust = 0.5, size = 12),
          axis.title.y = element_text(size = 12),
          legend.position = "bottom",
          legend.justification = "center",
          legend.text = element_text(size = 15),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    labs(x = " ", y = "Percent chance", title = " ") +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
    # Expand the plot margins to accommodate arrows
    coord_cartesian(clip = 'off')
  
  # I add the relevant stars here
  
  t.test(dt$bel_2030_1, dt_sh$sec_bel_2030_1)
  t.test(dt$bel_2030_1, dt_sh$bel_2030_1)
  t.test(dt_sh$bel_2030_1, dt_sh$sec_bel_2030_1)
  
  t.test(dt$bel_2030_2, dt_sh$sec_bel_2030_2)
  t.test(dt$bel_2030_2, dt_sh$bel_2030_2)
  t.test(dt_sh$bel_2030_2, dt_sh$sec_bel_2030_2)
  
  t.test(dt$bel_2030_3, dt_sh$sec_bel_2030_3)
  t.test(dt$bel_2030_3, dt_sh$bel_2030_3)
  t.test(dt_sh$bel_2030_3, dt_sh$sec_bel_2030_3)
  
  t.test(dt$bel_2030_4, dt_sh$sec_bel_2030_4)
  t.test(dt$bel_2030_4, dt_sh$bel_2030_4)
  t.test(dt_sh$bel_2030_4, dt_sh$sec_bel_2030_4)
  
  p3 <- p3 +
    geom_signif(
      annotations =  "***",
      xmin = 0.75,
      xmax = 0.95,
      y_position = 25,
      tip_length = 0.01,
      vjust = 0.5,
      textsize = 4
    ) +
    geom_signif(
      annotations =  "***",
      xmin = 1.05,
      xmax = 1.25,
      y_position = 32.5,
      tip_length = 0.01,
      vjust = 0.5,
      textsize = 4
    ) +
    geom_signif(
      annotations =  "***",
      xmin = 2.75,
      xmax = 2.95,
      y_position = 47.5,
      tip_length = 0.01,
      vjust = 0.5,
      textsize = 4
    ) +
    geom_signif(
      annotations =  "**",
      xmin = 3.05,
      xmax = 3.25,
      y_position = 37.5,
      tip_length = 0.01,
      vjust = 0.5,
      textsize = 4
    )  +
    geom_signif(
      annotations =  "**",
      xmin = 2.05,
      xmax = 2.25,
      y_position = 40,
      tip_length = 0.01,
      vjust = 0.5,
      textsize = 4
    ) + 
    geom_signif(
      annotations =  "***",
      xmin = 0.75,
      xmax = 1.25,
      y_position = 37.5,
      tip_length = 0.01,
      vjust = 0.5,
      textsize = 4
    ) + 
    geom_signif(
      annotations =  "***",
      xmin = 2.75,
      xmax = 3.25,
      y_position = 52.5,
      tip_length = 0.01,
      vjust = 0.5,
      textsize = 4
    )
  
  print(p3)
  ggsave("figure_3.png", width = 10, height = 6)
  
  
  
  
  

