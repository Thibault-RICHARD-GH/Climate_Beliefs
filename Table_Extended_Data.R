############################
############ Figure 3 ######
############################

### Define folder
setwd(".")

# Alternatively, define the folder manually. Ex: 
 folder <- "~/Documents/Project Capable - Code and picture"
 setwd(folder)

# Make sure that the code is in the same folder as the dataset

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

  # Computation mu
  {
    # Compute the probability of each events by translating 
    # percentage points in probabilities
    
    pi_1 <- dt$bel_2030_1/100
    pi_2 <- dt$bel_2030_2/100
    pi_3 <- dt$bel_2030_3/100
    pi_4 <- dt$bel_2030_4/100
    
    #### Estimation of mu (central expectation)
    
    # I fix the maximal and minimal bounds to compute the 
    # central expectations, with the hypothesis that the 
    # variable  is uniformally distributed within each bin
    
    B_min <- 22.5
    B_max <- 65
    
    # Computation of mu in percentage points
    
    dt$mu <- pi_1*(B_min + 32.5)/2 + 
      pi_2*(32.5 + 45)/2 + 
      pi_3*(45 + 55)/2 + 
      pi_4*(55 + B_max)/2 
    
    
    ### Estimation of the entropy
    
    # In the computation of the entropy, we compute for 
    # the bins 1, 2, 3, and 4 the formulma p_i*log(p_i)
    # separately, naming them "contri_i" for "contribution
    # to the entropy".
    
    contri_1 <- ifelse(dt$bel_2030_1==0, 0, (pi_1)*log(pi_1))
    contri_2 <- ifelse(dt$bel_2030_2==0, 0, (pi_2)*log(pi_2))
    contri_3 <- ifelse(dt$bel_2030_3==0, 0, (pi_3)*log(pi_3))
    contri_4 <- ifelse(dt$bel_2030_4==0, 0, (pi_4)*log(pi_4))
    
    # the entropy is the sum of the contributions above.
    dt$entropy <- contri_1 + contri_2 + contri_3 + contri_4
    
    # the normalized entropy is given by:
    dt$norm_entropy <- -dt$entropy/log(4)
  }
  
  # Computation mu among sh
  
  {
    # Compute the probability of each events by translating 
    # percentage points in probabilities
    
    pi_1 <- dt_sh$bel_2030_1/100
    pi_2 <- dt_sh$bel_2030_2/100
    pi_3 <- dt_sh$bel_2030_3/100
    pi_4 <- dt_sh$bel_2030_4/100
    
    #### Estimation of mu (central expectation)
    
    # I fix the maximal and minimal bounds to compute the 
    # central expectations, with the hypothesis that the 
    # variable  is uniformally distributed within each bin
    
    B_min <- 22.5
    B_max <- 65
    
    # Computation of mu in percentage points
    
    dt_sh$mu <- pi_1*(B_min + 32.5)/2 + 
      pi_2*(32.5 + 45)/2 + 
      pi_3*(45 + 55)/2 + 
      pi_4*(55 + B_max)/2 
    
    
    ### Estimation of the entropy
    
    # In the computation of the entropy, we compute for 
    # the bins 1, 2, 3, and 4 the formulma p_i*log(p_i)
    # separately, naming them "contri_i" for "contribution
    # to the entropy".
    
    contri_1 <- ifelse(dt_sh$bel_2030_1==0, 0, (pi_1)*log(pi_1))
    contri_2 <- ifelse(dt_sh$bel_2030_2==0, 0, (pi_2)*log(pi_2))
    contri_3 <- ifelse(dt_sh$bel_2030_3==0, 0, (pi_3)*log(pi_3))
    contri_4 <- ifelse(dt_sh$bel_2030_4==0, 0, (pi_4)*log(pi_4))
    
    # the entropy is the sum of the contributions above.
    dt_sh$entropy <- contri_1 + contri_2 + contri_3 + contri_4
    
    # the normalized entropy is given by:
    dt_sh$norm_entropy <- -dt_sh$entropy/log(4)
    
  }
  
  # Computation mu among sh of SOB
  
  {
    # Compute the probability of each events by translating 
    # percentage points in probabilities
    
    pi_1 <- dt_sh$sec_bel_2030_1/100
    pi_2 <- dt_sh$sec_bel_2030_2/100
    pi_3 <- dt_sh$sec_bel_2030_3/100
    pi_4 <- dt_sh$sec_bel_2030_4/100
    
    #### Estimation of mu (central expectation)
    
    # I fix the maximal and minimal bounds to compute the 
    # central expectations, with the hypothesis that the 
    # variable  is uniformally distributed within each bin
    
    B_min <- 22.5
    B_max <- 65
    
    # Computation of mu in percentage points
    
    dt_sh$mu_sob <- pi_1*(B_min + 32.5)/2 + 
      pi_2*(32.5 + 45)/2 + 
      pi_3*(45 + 55)/2 + 
      pi_4*(55 + B_max)/2 
    
    
    ### Estimation of the entropy
    
    # In the computation of the entropy, we compute for 
    # the bins 1, 2, 3, and 4 the formulma p_i*log(p_i)
    # separately, naming them "contri_i" for "contribution
    # to the entropy".
    
    contri_1 <- ifelse(dt_sh$sec_bel_2030_1==0, 0, (pi_1)*log(pi_1))
    contri_2 <- ifelse(dt_sh$sec_bel_2030_2==0, 0, (pi_2)*log(pi_2))
    contri_3 <- ifelse(dt_sh$sec_bel_2030_3==0, 0, (pi_3)*log(pi_3))
    contri_4 <- ifelse(dt_sh$sec_bel_2030_4==0, 0, (pi_4)*log(pi_4))
    
    # the entropy is the sum of the contributions above.
    dt_sh$entropy_sob <- contri_1 + contri_2 + contri_3 + contri_4
    
    # the normalized entropy is given by:
    dt_sh$norm_entropy_sob <- -dt_sh$entropy_sob/log(4)
    
  }
  
  dt_sh2 <- dt_sh[is.na(dt_sh$stakeholder)==FALSE, ]
  
star1_fun <- function(value, pvalue){
  value_char <- as.character(value)
  stars <- ifelse(pvalue<0.001, "***", 
                  ifelse(pvalue<0.01, "**",
                         ifelse(pvalue<0.05, "*", "")))
  paste0(value_char, stars)
}

star2_fun <- function(value, pvalue1, pvalue2){
  first_term <- star1_fun(value, pvalue1)
  stars2 <- ifelse(pvalue2<0.001, "+++", 
                  ifelse(pvalue2<0.01, "++",
                         ifelse(pvalue2<0.05, "+", "")))
  paste0(first_term, stars2)
}

N_pop_gen_pop <- nrow(dt)
N_pop_stake <- nrow(dt_sh)
N_pop_pm <- nrow(dt_sh2[dt_sh2$stakeholder=="Policymakers", ])
N_pop_experts <- nrow(dt_sh2[dt_sh2$stakeholder=="Experts", ])

line1 <- c(N_pop_gen_pop, N_pop_stake, N_pop_pm, N_pop_experts)
line1 <- as.character(line1)

ce_gen_pop <- round(mean(dt$mu), 1)
ce_stake <- round(mean(dt_sh$mu), 1)
ce_pm <- round(
  mean(dt_sh2[dt_sh2$stakeholder=="Policymakers", ]$mu), 1)
ce_experts <- round(
  mean(dt_sh2[dt_sh2$stakeholder=="Experts", ]$mu), 1)

pv_ce1 <- t.test(dt$mu, dt_sh$mu)$p.value
pv_ce2 <- t.test(dt$mu, 
                 dt_sh2[dt_sh2$stakeholder=="Policymakers", ]$mu)$p.value
pv_ce3 <- t.test(dt$mu, 
                 dt_sh2[dt_sh2$stakeholder=="Experts", ]$mu)$p.value
pv_ce4 <- t.test(dt_sh2[dt_sh2$stakeholder=="Policymakers", ]$mu,
                 dt_sh2[dt_sh2$stakeholder=="Experts", ]$mu)$p.value

line2 <- c(as.character(ce_gen_pop),
           star1_fun(ce_stake, pv_ce1),
           star1_fun(ce_pm, pv_ce2),
           star2_fun(ce_experts, pv_ce3, pv_ce4))

nei_gen_pop <- round(mean(dt$norm_entropy), 2)
nei_stake <- round(mean(dt_sh$norm_entropy), 2)
nei_pm <- round(
  mean(dt_sh2[dt_sh2$stakeholder=="Policymakers", ]$norm_entropy), 2)
nei_experts <- round(
  mean(dt_sh2[dt_sh2$stakeholder=="Experts", ]$norm_entropy), 2)

pv_nei1 <- t.test(dt$norm_entropy, dt_sh$norm_entropy)$p.value
pv_nei2 <- t.test(dt$norm_entropy, 
                 dt_sh2[dt_sh2$stakeholder=="Policymakers", ]$norm_entropy)$p.value
pv_nei3 <- t.test(dt$norm_entropy, 
                 dt_sh2[dt_sh2$stakeholder=="Experts", ]$norm_entropy)$p.value
pv_nei4 <- t.test(dt_sh2[dt_sh2$stakeholder=="Policymakers", ]$norm_entropy,
                 dt_sh2[dt_sh2$stakeholder=="Experts", ]$norm_entropy)$p.value

line3 <- c(as.character(nei_gen_pop),
           star1_fun(nei_stake, pv_nei1),
           star1_fun(nei_pm, pv_nei2),
           star2_fun(nei_experts, pv_nei3, pv_nei4))

conf_gen_pop <- round(mean(dt$bel_2030_conf), 2)
conf_stake <- round(mean(dt_sh$bel_2030_conf), 2)
conf_pm <- round(
  mean(dt_sh2[dt_sh2$stakeholder=="Policymakers", ]$bel_2030_conf), 2)
conf_experts <- round(
  mean(dt_sh2[dt_sh2$stakeholder=="Experts", ]$bel_2030_conf), 2)

pv_conf1 <- t.test(dt$bel_2030_conf, dt_sh$bel_2030_conf)$p.value
pv_conf2 <- t.test(dt$bel_2030_conf, 
                  dt_sh2[dt_sh2$stakeholder=="Policymakers", ]$bel_2030_conf)$p.value
pv_conf3 <- t.test(dt$bel_2030_conf, 
                  dt_sh2[dt_sh2$stakeholder=="Experts", ]$bel_2030_conf)$p.value
pv_conf4 <- t.test(dt_sh2[dt_sh2$stakeholder=="Policymakers", ]$bel_2030_conf,
                  dt_sh2[dt_sh2$stakeholder=="Experts", ]$bel_2030_conf)$p.value

line4 <- c(as.character(conf_gen_pop),
           star1_fun(conf_stake, pv_conf1),
           star1_fun(conf_pm, pv_conf2),
           star2_fun(conf_experts, pv_conf3, pv_conf4))

mu_sob_gen_pop <- "/"
mu_sob_stake <- round(mean(dt_sh$mu_sob), 1)
mu_sob_pm <- round(
  mean(dt_sh2[dt_sh2$stakeholder=="Policymakers", ]$mu_sob), 1)
mu_sob_experts <- round(
  mean(dt_sh2[dt_sh2$stakeholder=="Experts", ]$mu_sob), 1)

pv_mu_sob1 <- t.test(dt$mu, dt_sh$mu_sob)$p.value
pv_mu_sob2 <- t.test(dt$mu, 
                   dt_sh2[dt_sh2$stakeholder=="Policymakers", ]$mu_sob)$p.value
pv_mu_sob3 <- t.test(dt$mu, 
                   dt_sh2[dt_sh2$stakeholder=="Experts", ]$mu_sob)$p.value
pv_mu_sob4 <- t.test(dt_sh2[dt_sh2$stakeholder=="Policymakers", ]$mu_sob,
                   dt_sh2[dt_sh2$stakeholder=="Experts", ]$mu_sob)$p.value

line5 <- c(as.character(mu_sob_gen_pop),
           star1_fun(mu_sob_stake, pv_mu_sob1),
           star1_fun(mu_sob_pm, pv_mu_sob2),
           star2_fun(mu_sob_experts, pv_mu_sob3, pv_mu_sob4))

nei_sob_gen_pop <- "/"
nei_sob_stake <- round(mean(dt_sh$norm_entropy_sob), 2)
nei_sob_pm <- round(
  mean(dt_sh2[dt_sh2$stakeholder=="Policymakers", ]$norm_entropy_sob), 2)
nei_sob_experts <- round(
  mean(dt_sh2[dt_sh2$stakeholder=="Experts", ]$norm_entropy_sob), 2)

contri_entropy_1 <- mean(dt$bel_2030_1/100)*log(mean(dt$bel_2030_1/100))
contri_entropy_2 <- mean(dt$bel_2030_2/100)*log(mean(dt$bel_2030_2/100))
contri_entropy_3 <- mean(dt$bel_2030_3/100)*log(mean(dt$bel_2030_3/100))
contri_entropy_4 <- mean(dt$bel_2030_4/100)*log(mean(dt$bel_2030_4/100))

entropy <- contri_entropy_1 + contri_entropy_2 +  contri_entropy_3 + contri_entropy_4

global_nei <- -entropy/log(4)

pv_nei_sob1 <- t.test(dt_sh$norm_entropy_sob - global_nei)$p.value
pv_nei_sob2 <- t.test(dt_sh2[dt_sh2$stakeholder=="Policymakers", ]$norm_entropy_sob- global_nei)$p.value
pv_nei_sob3 <- t.test(dt_sh2[dt_sh2$stakeholder=="Experts", ]$norm_entropy_sob - global_nei)$p.value
pv_nei_sob4 <- t.test(dt_sh2[dt_sh2$stakeholder=="Policymakers", ]$norm_entropy_sob,
                     dt_sh2[dt_sh2$stakeholder=="Experts", ]$norm_entropy_sob)$p.value

line6 <- c(as.character(nei_sob_gen_pop),
           star1_fun(nei_sob_stake, pv_nei_sob1),
           star1_fun(nei_sob_pm, pv_nei_sob2),
           star2_fun(nei_sob_experts, pv_nei_sob3, pv_nei_sob4))


gdr_gen_pop <- round(mean(dt$female)*100, 1)
gdr_stake <- round(mean(dt_sh$female, na.rm = TRUE)*100, 1)
gdr_pm <- round(mean(dt_sh2[dt_sh2$stakeholder=="Policymakers", ]$female, na.rm = TRUE)*100, 1)
gdr_experts <- round(mean(dt_sh2[dt_sh2$stakeholder=="Experts", ]$female, na.rm = TRUE)*100, 1)


pv_gdr1 <- t.test(dt$female, dt_sh$norm_entropy_sob)$p.value
pv_gdr2 <- t.test(dt$female, 
                      dt_sh2[dt_sh2$stakeholder=="Policymakers", ]$female)$p.value
pv_gdr3 <- t.test(dt$female, 
                      dt_sh2[dt_sh2$stakeholder=="Experts", ]$female)$p.value
pv_gdr4 <- t.test(dt_sh2[dt_sh2$stakeholder=="Policymakers", ]$female,
                      dt_sh2[dt_sh2$stakeholder=="Experts", ]$female)$p.value

line7 <- c(as.character(gdr_gen_pop),
           star1_fun(gdr_stake, pv_gdr1),
           star1_fun(gdr_pm, pv_gdr2),
           star2_fun(gdr_experts, pv_gdr3, pv_gdr4))


age_1839_gen_pop <- round(mean(dt$age_1839)*100, 0)
age_1839_stake <- round(mean(dt_sh$age_1839, na.rm = TRUE)*100, 0)
age_1839_pm <- round(mean(dt_sh2[dt_sh2$stakeholder=="Policymakers", ]$age_1839, na.rm = TRUE)*100, 0)
age_1839_experts <- round(mean(dt_sh2[dt_sh2$stakeholder=="Experts", ]$age_1839, na.rm = TRUE)*100, 0)

pv_age_1839_1 <- t.test(dt$age_1839, dt_sh$age_1839)$p.value
pv_age_1839_2 <- t.test(dt$age_1839, 
                      dt_sh2[dt_sh2$stakeholder=="Policymakers", ]$age_1839)$p.value
pv_age_1839_3 <- t.test(dt$age_1839, 
                      dt_sh2[dt_sh2$stakeholder=="Experts", ]$age_1839)$p.value
pv_age_1839_4 <- t.test(dt_sh2[dt_sh2$stakeholder=="Policymakers", ]$age_1839,
                      dt_sh2[dt_sh2$stakeholder=="Experts", ]$age_1839)$p.value



age_4059_gen_pop <- round(mean(dt$age_4059)*100, 0)
age_4059_stake <- round(mean(dt_sh$age_4059, na.rm = TRUE)*100, 0)
age_4059_pm <- round(mean(dt_sh2[dt_sh2$stakeholder=="Policymakers", ]$age_4059, na.rm = TRUE)*100, 0)
age_4059_experts <- round(mean(dt_sh2[dt_sh2$stakeholder=="Experts", ]$age_4059, na.rm = TRUE)*100, 0)

pv_age_4059_1 <- t.test(dt$age_4059, dt_sh$age_4059)$p.value
pv_age_4059_2 <- t.test(dt$age_4059, 
                        dt_sh2[dt_sh2$stakeholder=="Policymakers", ]$age_4059)$p.value
pv_age_4059_3 <- t.test(dt$age_4059, 
                        dt_sh2[dt_sh2$stakeholder=="Experts", ]$age_4059)$p.value
pv_age_4059_4 <- t.test(dt_sh2[dt_sh2$stakeholder=="Policymakers", ]$age_4059,
                        dt_sh2[dt_sh2$stakeholder=="Experts", ]$age_4059)$p.value


age_6074_gen_pop <- round(mean(dt$age_6074)*100, 0)
age_6074_stake <- round(mean(dt_sh$age_6074, na.rm = TRUE)*100, 0)
age_6074_pm <- round(mean(dt_sh2[dt_sh2$stakeholder=="Policymakers", ]$age_6074, na.rm = TRUE)*100, 0)
age_6074_experts <- round(mean(dt_sh2[dt_sh2$stakeholder=="Experts", ]$age_6074, na.rm = TRUE)*100, 0)

pv_age_6074_1 <- t.test(dt$age_6074, dt_sh$age_6074)$p.value
pv_age_6074_2 <- t.test(dt$age_6074, 
                        dt_sh2[dt_sh2$stakeholder=="Policymakers", ]$age_6074)$p.value
pv_age_6074_3 <- t.test(dt$age_6074, 
                        dt_sh2[dt_sh2$stakeholder=="Experts", ]$age_6074)$p.value
pv_age_6074_4 <- t.test(dt_sh2[dt_sh2$stakeholder=="Policymakers", ]$age_6074,
                        dt_sh2[dt_sh2$stakeholder=="Experts", ]$age_6074)$p.value

line8 <- c(paste0(as.character(age_1839_gen_pop), "/",
                  as.character(age_4059_gen_pop), "/",
                  as.character(age_6074_gen_pop)), 
           paste0(as.character(star1_fun(age_1839_stake, pv_age_1839_1)), "/",
                  as.character(star1_fun(age_4059_stake, pv_age_4059_1)), "/",
                  as.character(star1_fun(age_6074_stake, pv_age_6074_1))),
           paste0(as.character(star1_fun(age_1839_pm, pv_age_1839_2)), "/",
                  as.character(star1_fun(age_4059_pm, pv_age_4059_2)), "/",
                  as.character(star1_fun(age_6074_pm, pv_age_6074_2))),
           paste0(as.character(star2_fun(age_1839_experts, pv_age_1839_3, pv_age_1839_4)), "/",
                  as.character(star2_fun(age_4059_experts, pv_age_4059_3, pv_age_4059_4)), "/",
                  as.character(star2_fun(age_6074_experts, pv_age_6074_3, pv_age_6074_4))))
           
educ_low_gen_pop <- round(mean(dt$educ_low)*100, 0)
educ_low_stake <- 0
educ_low_pm <- 0
educ_low_experts <- 0

pv_educ_low1 <- t.test(dt$educ_low - 0)$p.value
pv_educ_low2 <- t.test(dt$educ_low - 0)$p.value
pv_educ_low3 <- t.test(dt$educ_low - 0)$p.value
pv_educ_low4 <- 1

educ_mid_gen_pop <- round(mean(dt$educ_mid)*100, 0)
educ_mid_stake <- 0
educ_mid_pm <- 0
educ_mid_experts <- 0

pv_educ_mid1 <- t.test(dt$educ_mid - 0)$p.value
pv_educ_mid2 <- t.test(dt$educ_mid - 0)$p.value
pv_educ_mid3 <- t.test(dt$educ_mid - 0)$p.value
pv_educ_mid4 <- 1


educ_high_gen_pop <- round(mean(dt$educ_high)*100, 0)
educ_high_stake <- 100
educ_high_pm <- 100
educ_high_experts <- 100

pv_educ_high1 <- t.test(dt$educ_high - 100)$p.value
pv_educ_high2 <- t.test(dt$educ_high - 100)$p.value
pv_educ_high3 <- t.test(dt$educ_high - 100)$p.value
pv_educ_high4 <- 1

line9 <- c(paste0(as.character(educ_low_gen_pop), "/",
                  as.character(educ_mid_gen_pop), "/",
                  as.character(educ_high_gen_pop)), 
           paste0(as.character(star1_fun(educ_low_stake, pv_educ_low1)), "/",
                  as.character(star1_fun(educ_mid_stake, pv_educ_mid1)), "/",
                  as.character(star1_fun(educ_high_stake, pv_educ_high1))),
           paste0(as.character(star1_fun(educ_low_pm, pv_educ_low2)), "/",
                  as.character(star1_fun(educ_mid_pm, pv_educ_mid2)), "/",
                  as.character(star1_fun(educ_high_pm, pv_educ_high2))),
           paste0(as.character(star2_fun(educ_low_experts, pv_educ_low3, pv_educ_low4)), "/",
                  as.character(star2_fun(educ_mid_experts, pv_educ_mid3, pv_educ_mid4)), "/",
                  as.character(star2_fun(educ_high_experts, pv_educ_high3, pv_educ_high4))))


know_gen_pop <- round(mean(dt$fit_55_knowl), 2)
know_stake <- round(mean(dt_sh$fit_55_knowl, na.rm = TRUE), 2)
know_pm <- round(mean(dt_sh2[dt_sh2$stakeholder=="Policymakers", ]$fit_55_knowl, na.rm = TRUE), 2)
know_experts <- round(mean(dt_sh2[dt_sh2$stakeholder=="Experts", ]$fit_55_knowl, na.rm = TRUE), 2)

pv_know1 <- t.test(dt$fit_55_knowl, dt_sh$fit_55_knowl)$p.value
pv_know2 <- t.test(dt$fit_55_knowl, 
                        dt_sh2[dt_sh2$stakeholder=="Policymakers", ]$fit_55_knowl)$p.value
pv_know3 <- t.test(dt$fit_55_knowl, 
                        dt_sh2[dt_sh2$stakeholder=="Experts", ]$fit_55_knowl)$p.value
pv_know4 <- t.test(dt_sh2[dt_sh2$stakeholder=="Policymakers", ]$fit_55_knowl,
                        dt_sh2[dt_sh2$stakeholder=="Experts", ]$fit_55_knowl)$p.value

line10 <- c(as.character(know_gen_pop),
            star1_fun(know_stake, pv_know1),
            star1_fun(know_pm, pv_know2),
            star2_fun(know_experts, pv_know3, pv_know4))

worry_gen_pop <- round(mean(dt$clim_worry), 2)
worry_stake <- round(mean(dt_sh$clim_worry, na.rm = TRUE), 2)
worry_pm <- round(mean(dt_sh2[dt_sh2$stakeholder=="Policymakers", ]$clim_worry, na.rm = TRUE), 2)
worry_experts <- round(mean(dt_sh2[dt_sh2$stakeholder=="Experts", ]$clim_worry, na.rm = TRUE), 2)

pv_worry1 <- t.test(dt$clim_worry, dt_sh$clim_worry)$p.value
pv_worry2 <- t.test(dt$clim_worry, 
                   dt_sh2[dt_sh2$stakeholder=="Policymakers", ]$clim_worry)$p.value
pv_worry3 <- t.test(dt$clim_worry, 
                   dt_sh2[dt_sh2$stakeholder=="Experts", ]$clim_worry)$p.value
pv_worry4 <- t.test(dt_sh2[dt_sh2$stakeholder=="Policymakers", ]$clim_worry,
                   dt_sh2[dt_sh2$stakeholder=="Experts", ]$clim_worry)$p.value

line11 <- c(as.character(worry_gen_pop),
            star1_fun(worry_stake, pv_worry1),
            star1_fun(worry_pm, pv_worry2),
            star2_fun(worry_experts, pv_worry3, pv_worry4))

exp_gen_pop <- "/"
exp_stake <- round(mean(dt_sh$exp >2, na.rm = TRUE)*100, 1)
exp_pm <- round(mean(dt_sh2[dt_sh2$stakeholder=="Policymakers", ]$exp >2, na.rm = TRUE)*100, 1)
exp_experts <- round(mean(dt_sh2[dt_sh2$stakeholder=="Experts", ]$exp>2, na.rm = TRUE)*100, 1)

pv_exp4 <- t.test((dt_sh2[dt_sh2$stakeholder=="Policymakers", ]$exp>2),
                    (dt_sh2[dt_sh2$stakeholder=="Experts", ]$exp>2))$p.value

line12 <- c(as.character(exp_gen_pop),
            as.character(exp_stake),
            as.character(exp_pm),
            star2_fun(exp_experts, 1, pv_exp4))

focus_gen_pop <- "/"
focus_stake <- round(mean(dt_sh$focus_recoded, na.rm = TRUE), 2)
focus_pm <- round(mean(dt_sh2[dt_sh2$stakeholder=="Policymakers", ]$focus_recoded, na.rm = TRUE), 2)
focus_experts <- round(mean(dt_sh2[dt_sh2$stakeholder=="Experts", ]$focus_recoded, na.rm = TRUE), 2)

pv_focus4 <- t.test((dt_sh2[dt_sh2$stakeholder=="Policymakers", ]$focus_recoded),
                  (dt_sh2[dt_sh2$stakeholder=="Experts", ]$focus_recoded))$p.value

line13 <- c(as.character(focus_gen_pop),
            as.character(focus_stake),
            as.character(focus_pm),
            star2_fun(focus_experts, 1, pv_focus4))

eval_gen_pop <- "/"
eval_stake <- round(mean(dt_sh$evaluation_recoded, na.rm = TRUE), 2)
eval_pm <- round(mean(dt_sh2[dt_sh2$stakeholder=="Policymakers", ]$evaluation_recoded, na.rm = TRUE), 2)
eval_experts <- round(mean(dt_sh2[dt_sh2$stakeholder=="Experts", ]$evaluation_recoded, na.rm = TRUE), 2)

pv_eval4 <- t.test((dt_sh2[dt_sh2$stakeholder=="Policymakers", ]$evaluation_recoded),
                  (dt_sh2[dt_sh2$stakeholder=="Experts", ]$evaluation_recoded))$p.value

line14 <- c(as.character(eval_gen_pop),
            as.character(eval_stake),
            as.character(eval_pm),
            star2_fun(eval_experts, 1, pv_eval4))

names_variable <- c("N", "Central expectation",
                    "Uncertainty (Normalized entropy index)",
                    "Confidence (5-point Likert)",
                    "Central expectation - SOB",
                    "Uncertainty - SOB",
                    "Gender (Woman)",
                    "Age (18-39/40-59/60+)",
                    "Education (High/Medium/Low)",
                    "Knowledge about EU climate policy (7-point Likert)",
                    "Concern about climate change (5-point Likert)",
                    "Experience in their field (10+)",
                    "Work related to climate policy",
                    "Work include climate policy evaluation (5-point Likert)")



Extended_Table_1 <- matrix(data=NA, ncol = 5, nrow = 14)

Extended_Table_1[, 1] <- names_variable
for(k in seq(1:14)){
  linek <- get(paste0("line", as.character(k)))
  Extended_Table_1[k, 2:5] <- linek
}

print(Extended_Table_1)

print(Extended_Table_1[,2])
print(Extended_Table_1[,3])
print(Extended_Table_1[,4])
print(Extended_Table_1[,5])


###################
##### Table 2 #####
###################

pi_1 <- dt$bel_2030_1/100
pi_2 <- dt$bel_2030_2/100
pi_3 <- dt$bel_2030_3/100
pi_4 <- dt$bel_2030_4/100
  
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

n_obs <- nrow(dt)
vect_mu <- rep(NA, n_obs)

for(k in seq(1:n_obs)){
  
  
  temp_fun <- function(x){
    Kullback_Leibler(x[1], x[2],
              dt$bel_2030_1[k], dt$bel_2030_2[k], 
              dt$bel_2030_3[k], dt$bel_2030_4[k])
  }
  
  sum_optim <- optim(c(50, 0), temp_fun)
  vect_mu[k] <- sum_optim$par[1]
  
}

dt$mu_para <- vect_mu

# New bounds (previous are at 22.5 and 65)
B_min <- 27.5
B_max <- 60

# Computation of mu in percentage points

dt$mu_5 <- pi_1*(B_min + 32.5)/2 + 
  pi_2*(32.5 + 45)/2 + 
  pi_3*(45 + 55)/2 + 
  pi_4*(55 + B_max)/2 

# New bounds (previous are at 22.5 and 65)
B_min <- 12.5
B_max <- 75

# Computation of mu in percentage points

dt$mu_20 <- pi_1*(B_min + 32.5)/2 + 
  pi_2*(32.5 + 45)/2 + 
  pi_3*(45 + 55)/2 + 
  pi_4*(55 + B_max)/2 

# New bounds Triangular
B_min <- 0
B_max <- 100

# Computation of mu in percentage points

dt$mu_tri <- pi_1*(B_min + 2*32.5)/3 + 
  pi_2*(32.5 + 45)/2 + 
  pi_3*(45 + 55)/2 + 
  pi_4*(55*2 + B_max)/3 


data_for_cor <- dt[, c("mu_5", "mu", "mu_20", "mu_tri", "mu_para")]

round(cor(data_for_cor, method = "spearman"), 2)

###################

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
dt_temp3 <- data.frame(vect_mean=c(mean(dt_sh2$bel_2030_1[dt_sh2$stakeholder=="Policymakers"]),
                                   mean(dt_sh2$bel_2030_2[dt_sh2$stakeholder=="Policymakers"]),
                                   mean(dt_sh2$bel_2030_3[dt_sh2$stakeholder=="Policymakers"]),
                                   mean(dt_sh2$bel_2030_4[dt_sh2$stakeholder=="Policymakers"])),
                       vect_sd=c(sd(dt_sh2$bel_2030_1[dt_sh2$stakeholder=="Policymakers"]),
                                 sd(dt_sh2$bel_2030_2[dt_sh2$stakeholder=="Policymakers"]),
                                 sd(dt_sh2$bel_2030_3[dt_sh2$stakeholder=="Policymakers"]),
                                 sd(dt_sh2$bel_2030_4[dt_sh2$stakeholder=="Policymakers"])),
                       vect_n=c(length(dt_sh2$bel_2030_1[dt_sh2$stakeholder=="Policymakers"]),
                                length(dt_sh2$bel_2030_2[dt_sh2$stakeholder=="Policymakers"]),
                                length(dt_sh2$bel_2030_3[dt_sh2$stakeholder=="Policymakers"]),
                                length(dt_sh2$bel_2030_4[dt_sh2$stakeholder=="Policymakers"])),
                       cat = c("2022 levels or less  <br> (≤32.5%)",
                               "Small extra reduction <br> (32.5%, 45%]", 
                               "Moderate extra reduction <br> (45%, 55%)", 
                               "European Climate Law <br> (≥55%)"),
                       cat2 = c(1, 2, 3, 4),
                       cat3 = rep("Policymakers", 4),
                       group_order = rep(2, 4))

dt_temp3$min95 <- dt_temp3$vect_mean - 1.96*dt_temp3$vect_sd/sqrt(dt_temp3$vect_n)
dt_temp3$max95 <- dt_temp3$vect_mean + 1.96*dt_temp3$vect_sd/sqrt(dt_temp3$vect_n)

# Stakeholders  
dt_temp2 <- data.frame(vect_mean=c(mean(dt_sh2$bel_2030_1[dt_sh2$stakeholder=="Experts"]),
                                   mean(dt_sh2$bel_2030_2[dt_sh2$stakeholder=="Experts"]),
                                   mean(dt_sh2$bel_2030_3[dt_sh2$stakeholder=="Experts"]),
                                   mean(dt_sh2$bel_2030_4[dt_sh2$stakeholder=="Experts"])),
                       vect_sd=c(sd(dt_sh2$bel_2030_1[dt_sh2$stakeholder=="Experts"]),
                                 sd(dt_sh2$bel_2030_2[dt_sh2$stakeholder=="Experts"]),
                                 sd(dt_sh2$bel_2030_3[dt_sh2$stakeholder=="Experts"]),
                                 sd(dt_sh2$bel_2030_4[dt_sh2$stakeholder=="Experts"])),
                       vect_n=c(length(dt_sh2$bel_2030_1[dt_sh2$stakeholder=="Experts"]),
                                length(dt_sh2$bel_2030_2[dt_sh2$stakeholder=="Experts"]),
                                length(dt_sh2$bel_2030_3[dt_sh2$stakeholder=="Experts"]),
                                length(dt_sh2$bel_2030_4[dt_sh2$stakeholder=="Experts"])),
                       cat =c("2022 levels or less  <br> (≤32.5%)",
                              "Small extra reduction <br> (32.5%, 45%]", 
                              "Moderate extra reduction <br> (45%, 55%)", 
                              "European Climate Law <br> (≥55%)"),
                       cat2 = c(1, 2, 3, 4),
                       cat3 = rep("Experts", 4),
                       group_order = rep(3, 4))

dt_temp2$min95 <- dt_temp2$vect_mean - 1.96*dt_temp2$vect_sd/sqrt(dt_temp2$vect_n)
dt_temp2$max95 <- dt_temp2$vect_mean + 1.96*dt_temp2$vect_sd/sqrt(dt_temp2$vect_n)

dt_overall <- rbind(dt_overall, dt_temp3, dt_temp2)

dt_overall$cat3 <- factor(dt_overall$cat3, 
                          levels = c("Policymakers",
                                     "Experts",
                                     "General Public"))
stakeholder_alpha <- c("General Public" = 0.8, "Experts" = 0.8, "Policymakers" = 0.8)
# install.packages("ggpattern")
library(ggpattern)


p_ED <- ggplot(dt_overall, aes(x = reorder(cat, cat2), y = vect_mean, fill = cat3)) +
  # Add grid lines first (at the back)
  geom_hline(yintercept = seq(0, 50, by = 10), color = "grey90", linetype = "dashed") +
  # Add the bars with position dodge
  geom_col(aes(alpha = cat3), position = position_dodge(0.9)) + 
  # Add alpha for the middle group only
  
  scale_alpha_manual(values = c("General Public" = 0.6, 
                                "Experts" = 0.6, 
                                "Policymakers" = 0.6),
                     guide = "none") +
  # Add bar outlines
  geom_col(position = position_dodge(0.9), alpha = 0.3, color = "black", size = 0.2) +
  scale_fill_manual(name = " ",
                    values = c("General Public" = "#3366CC", 
                               "Policymakers" = "#CC0000", 
                               "Experts" = "#006633")) +
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

t.test(dt$bel_2030_1, dt_sh2$bel_2030_1[dt_sh2$stakeholder=="Policymakers"])
t.test(dt$bel_2030_1, dt_sh2$bel_2030_1[dt_sh2$stakeholder=="Experts"])
t.test(dt_sh2$bel_2030_1[dt_sh2$stakeholder=="Experts"], 
       dt_sh2$bel_2030_1[dt_sh2$stakeholder=="Policymakers"])

t.test(dt$bel_2030_2, dt_sh2$bel_2030_2[dt_sh2$stakeholder=="Policymakers"])
t.test(dt$bel_2030_2, dt_sh2$bel_2030_2[dt_sh2$stakeholder=="Experts"])
t.test(dt_sh2$bel_2030_2[dt_sh2$stakeholder=="Experts"], 
       dt_sh2$bel_2030_2[dt_sh2$stakeholder=="Policymakers"])

t.test(dt$bel_2030_3, dt_sh2$bel_2030_3[dt_sh2$stakeholder=="Policymakers"])
t.test(dt$bel_2030_3, dt_sh2$bel_2030_3[dt_sh2$stakeholder=="Experts"])
t.test(dt_sh2$bel_2030_3[dt_sh2$stakeholder=="Experts"], 
       dt_sh2$bel_2030_3[dt_sh2$stakeholder=="Policymakers"])

t.test(dt$bel_2030_4, dt_sh2$bel_2030_4[dt_sh2$stakeholder=="Policymakers"])
t.test(dt$bel_2030_4, dt_sh2$bel_2030_4[dt_sh2$stakeholder=="Experts"])
t.test(dt_sh2$bel_2030_4[dt_sh2$stakeholder=="Experts"], 
       dt_sh2$bel_2030_4[dt_sh2$stakeholder=="Policymakers"])


p_ED <- p_ED +
  geom_signif(
    annotations =  "***",
    xmin = 0.75,
    xmax = 1.25,
    y_position = 40,
    tip_length = 0.01,
    vjust = 0.5,
    textsize = 4
  ) + 
  geom_signif(
    annotations =  "***",
    xmin = 1.05,
    xmax = 1.25,
    y_position = 34,
    tip_length = 0.01,
    vjust = 0.5,
    textsize = 4
  ) + 
  geom_signif(
    annotations =  "***",
    xmin = 1.75,
    xmax = 1.95,
    y_position = 47,
    tip_length = 0.01,
    vjust = 0.5,
    textsize = 4
  ) + 
  geom_signif(
    annotations =  "*",
    xmin = 2.05,
    xmax = 2.25,
    y_position = 47,
    tip_length = 0.01,
    vjust = 0.5,
    textsize = 4
  ) + 
  geom_signif(
    annotations =  "***",
    xmin = 2.75,
    xmax = 3.25,
    y_position = 55,
    tip_length = 0.01,
    vjust = 0.5,
    textsize = 4
  ) + 
  geom_signif(
    annotations =  "***",
    xmin = 3.05,
    xmax = 3.25,
    y_position = 45,
    tip_length = 0.01,
    vjust = 0.5,
    textsize = 4
  ) + 
  geom_signif(
    annotations =  "*",
    xmin = 3.75,
    xmax = 3.95,
    y_position = 42,
    tip_length = 0.01,
    vjust = 0.5,
    textsize = 4
  ) + 
  geom_signif(
    annotations =  "*",
    xmin = 4.05,
    xmax = 4.25,
    y_position = 25,
    tip_length = 0.01,
    vjust = 0.5,
    textsize = 4
  )




print(p_ED)
ggsave("figure_ED.png", width = 10, height = 6)



