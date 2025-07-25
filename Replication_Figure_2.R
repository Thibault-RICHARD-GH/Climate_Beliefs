####################################
############ Figure 1 Merged #######
####################################

### Define folder
setwd(".")

# Alternatively, define the folder manually. Ex: 
folder <- "~/Documents/Project Capable - Code and picture"
setwd(folder)


### I fix the bounds B_min and B_max 
### in measuring the non parametric average

# With +/- 10:

B_min <- (32.5 + (32.5 - 10))/2
B_max <- (55 + (55 + 10))/2

# Charge the packages

{
  #install.packages("glmnet")
  #install.packages("tidyverse")
  #install.packages("gridExtra")
  #install.packages("cowplot")
  #install.packages("dplyr")
  #install.packages("haven")
  #install.packages("eurostat")
  
library(glmnet)
library(tidyverse)
library(gridExtra)
library(cowplot)
library(dplyr)
library(haven)
library(eurostat)
  
}


# Data cleaning on the beliefs data

{

# I import the dataset for the general population
dt <- read_csv("General_population_cleaned.csv")
# I get the stakeholders
dt_sh <- read_csv("Stakeholders_cleaned.csv")

dt_sh <- dt_sh[is.na(dt_sh$stakeholder)==FALSE, ]
dt_sh$cntry <- NA
# I create a combined dataset for groups of stakeholders
#  (General Public, Policymakers, Experts)

dt_merged <- dt %>% 
  select(c(starts_with("bel_2030"), "cntry")) %>% 
  mutate(stakeholder="General Public") %>% 
  rbind(dt_sh %>% select(stakeholder, 
                         c(starts_with("bel_2030"), "cntry")))


}

# Calculate mu values for each stakeholder group
calculate_mu_for_group <- function(df) {
    

    vect_mu <- (df$bel_2030_1/100)*B_min +
               (df$bel_2030_2/100)*(32.5 + 45)/2 +
               (df$bel_2030_3/100)*(45 + 55)/2 +
               (df$bel_2030_4/100)*B_max
      
  return(vect_mu)
    
}

# Calculate mu values for each stakeholder group
calculate_entropy_for_group <- function(df) {
  
  contri_a <- ifelse(df$bel_2030_1==0, 0,
                     (df$bel_2030_1/100)*log(df$bel_2030_1/100))
  contri_b <- ifelse(df$bel_2030_2==0, 0, 
                     (df$bel_2030_2/100)*log(df$bel_2030_2/100))
  contri_c <- ifelse(df$bel_2030_3==0, 0,
                     (df$bel_2030_3/100)*log(df$bel_2030_3/100))
  contri_d <- ifelse(df$bel_2030_4==0, 0,
                     (df$bel_2030_4/100)*log(df$bel_2030_4/100))
  
  entropy <- -(contri_a + contri_b + contri_c + contri_d)/log(4)

  return(entropy)

}

# Process each stakeholder group
stakeholder_groups <- unique(dt_merged$stakeholder)
stakeholder_data <- data.frame()

for(group in stakeholder_groups) {
  
  group_data <- dt_merged[dt_merged$stakeholder == group, ]
  group_data$mu <- calculate_mu_for_group(group_data)
  group_data$entropy <- calculate_entropy_for_group(group_data)
  group_data$norm_entropy <- group_data$entropy
  group_data$emissions_2030 <- emission_1990 * (1 - group_data$mu/100)
  
  stakeholder_data <- rbind(stakeholder_data, group_data)
  
}

# Function to calculate mu for each observation
calculate_mu <- function(df) {
  
  
  vect_mu <- (df$bel_2030_1/100)*B_min +
    (df$bel_2030_2/100)*(32.5 + 45)/2 +
    (df$bel_2030_3/100)*(45 + 55)/2 +
    (df$bel_2030_4/100)*B_max
  
  return(vect_mu)
  
}

# Function to calculate entropy for each observation
calculate_entropy <- function(df) {
  
  contri_a <- ifelse(df$bel_2030_1==0, 0,
                     (df$bel_2030_1/100)*log(df$bel_2030_1/100))
  contri_b <- ifelse(df$bel_2030_2==0, 0, 
                     (df$bel_2030_2/100)*log(df$bel_2030_2/100))
  contri_c <- ifelse(df$bel_2030_3==0, 0,
                     (df$bel_2030_3/100)*log(df$bel_2030_3/100))
  contri_d <- ifelse(df$bel_2030_4==0, 0,
                     (df$bel_2030_4/100)*log(df$bel_2030_4/100))
  
  entropy <- -(contri_a + contri_b + contri_c + contri_d)/log(4)
 
  return(entropy)
  
}

# Calculate and add mu and entropy to dt_merged dataframe
dt_merged$mu <- calculate_mu(dt_merged)
dt_merged$norm_entropy <- calculate_entropy(dt_merged)

# Preparing the average by country
avg_bel_by_country <- dt_merged[dt_merged$stakeholder=="General Public",] %>%
  group_by(country_code = cntry) %>%  # Your variable for 'DE', 'FR', etc.
  summarise(avg_belief = mean(mu, na.rm = TRUE),
            sd_belief = sd(mu, na.rm = TRUE),
            n_belief = sum(is.na(mu)==FALSE),
            avg_nei = mean(norm_entropy, na.rm = TRUE),
            sd_nei = sd(norm_entropy, na.rm = TRUE),
            n_nei = sum(is.na(norm_entropy)==FALSE))

avg_bel_by_country$upper <- avg_bel_by_country$avg_belief + 
  1.96*avg_bel_by_country$sd_belief/(sqrt(avg_bel_by_country$n_belief))
avg_bel_by_country$lower <- avg_bel_by_country$avg_belief - 
  1.96*avg_bel_by_country$sd_belief/(sqrt(avg_bel_by_country$n_belief))

avg_bel_by_country$upper_nei <- avg_bel_by_country$avg_nei + 
  1.96*avg_bel_by_country$sd_nei/(sqrt(avg_bel_by_country$n_nei))
avg_bel_by_country$lower_nei <- avg_bel_by_country$avg_nei - 
  1.96*avg_bel_by_country$sd_nei/(sqrt(avg_bel_by_country$n_nei))

# preparing each figure separately
p1a <- ggplot(avg_bel_by_country, 
              aes(x = reorder(country_code, avg_belief), 
                  y = avg_belief)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                width = 0.1, color = "#3366CC", size = 1) +
  geom_point(size = 3, color = "#3366CC", alpha = 0.4) +
  geom_hline(yintercept = 55, linetype = "dashed", color = "black") + 
  geom_hline(yintercept = 32.5, linetype = "dashed", color = "black") +  
  geom_hline(yintercept = mean(stakeholder_stats$mean_mu[1]), 
             linetype = "dashed", color = "#006633") +  
  geom_hline(yintercept = mean(stakeholder_stats$mean_mu[3]), 
             linetype = "dashed", color = "#CC0000") +  
  annotate("text", x = Inf, y = 55,
           label = "EU targets 2030", 
           hjust = 1.1, vjust = -0.5, size = 4, color = "black") +
  annotate("text", x = Inf, y = 32.5, 
           label = "EU emissions reductions 2022",
           hjust = 1.1, vjust = -0.5, size = 4, color = "black") +
  annotate("text", x = Inf, 
           y = mean(dt_merged[dt_merged$stakeholder == "Experts", ]$mu), 
           label = "Experts' average",
           hjust = 1.1, vjust = -0.5, size = 4, color = "#006633") +
  annotate("text", x = Inf, 
           y = mean(dt_merged[dt_merged$stakeholder == "Policymakers", ]$mu), 
           label = "Policymakers' average", 
           hjust = 1.1, vjust = -0.5, size = 4, color = "#CC0000") +
  labs(
    title = " ",
    x = "Country",
    y = "Reduction from 1990 (%)"
  ) +
  coord_cartesian(ylim = c(30, 57.5)) +  
  scale_y_continuous(breaks = seq(30, 57.5, by = 5)) +
  theme_minimal() + 
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text.x = element_text(size = 11),  
    legend.position = "right"
  )


p1b <- ggplot(avg_bel_by_country, 
              aes(x = reorder(country_code, avg_belief), y = avg_nei)) +
  geom_errorbar(aes(ymin = lower_nei, ymax = upper_nei), width = 0.1, color = "#3366CC", size = 1) +
  geom_point(size = 3, color = "#3366CC", alpha = 0.4) +  # faded, small dot
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +  
  geom_hline(yintercept = mean(
    dt_merged[dt_merged$stakeholder=="Experts",]$norm_entropy), 
             linetype = "dashed", color = "#006633") +  
  geom_hline(yintercept = mean(
    dt_merged[dt_merged$stakeholder=="Policymakers",]$norm_entropy), 
             linetype = "dashed", color = "#CC0000") +

  annotate("text", x = Inf, y = 0.97, label = "Maximal uncertainty", hjust = 1.1, vjust = -0.5, size = 4, color = "black") +
  annotate("text", x = Inf, y = mean(dt_merged[dt_merged$stakeholder == "Experts", ]$norm_entropy -0.03), 
           label = "Experts' average", hjust = 1.1, vjust = -0.5, size = 4, color = "#006633") +
  annotate("text", x = Inf, y = mean(dt_merged[dt_merged$stakeholder == "Policymakers", ]$norm_entropy-0.03), 
           label = "Policymakers' average", hjust = 1.1, vjust = -0.5, size = 4, color = "#CC0000") +
  labs(
    title = " ",
    x = "Country",
    y = "Normalized Entropy"
  ) +
  coord_cartesian(ylim = c(0.4, 1.05)) +  
  scale_y_continuous(breaks = seq(0.4, 1.05, by = 0.1)) +
  theme_minimal() + 
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text.x = element_text(size = 11),  
    legend.position = "right"
  )


p1_combined <- plot_grid(
  p1a,
  p1b + theme(legend.position = "none"),
  nrow = 1,
  rel_widths = c(1, 1),
  labels = c("a  Average Central Expectation", 
             "b  Beliefs Uncertainty"),
  label_x = 0,   # left-align label positions
  hjust = 0,     # left-align label text
  align = "h",   # horizontal alignment
  axis = "tb"    # align top and bottom axes
)

ggsave("By_country.png", 
       plot = p1_combined, 
       width = 10, 
       height = 6, dpi = 300, bg = "white")
