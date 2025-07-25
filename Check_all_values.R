############################
############ Figure 3 ######
############################

### Define folder
setwd(".")

# Alternatively, define the folder manually. Ex: 
# folder <- "~/Documents/Project Capable - Code and picture"
# setwd(folder)

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
  
  
  
  # Number of respondents among the general public:
  nrow(dt)
  # Number of respondents among policymakers
  nrow(dt_sh)
  
  # Mean mu
  mean(dt$mu)
  
  # Mean probability in bin 4
  mean(dt$bel_2030_4)
  
  # Mean probability in bin 1 and 2
  mean(dt$bel_2030_1 + dt$bel_2030_2)
  
  # Mean central expectation experts
  mean(dt_sh[
    (dt_sh$stakeholder=="Experts")&(is.na(dt_sh$stakeholder)==FALSE),]$mu)
  
  # Mean central expectation Policymakers
  mean(dt_sh[
    (dt_sh$stakeholder=="Policymakers")&(is.na(dt_sh$stakeholder)==FALSE),]$mu)
  
  # Mean Bin 4 Experts
  mean(dt_sh[
    (dt_sh$stakeholder=="Experts")&(is.na(dt_sh$stakeholder)==FALSE),]$bel_2030_4)
  
  # Mean Bin 4 Policymakers 
  mean(dt_sh[
    (dt_sh$stakeholder=="Policymakers")&(is.na(dt_sh$stakeholder)==FALSE),]$bel_2030_4)
  
  # Test significance diff bin 4
  
  vect_1 <- dt_sh[
    (dt_sh$stakeholder=="Experts")&(is.na(dt_sh$stakeholder)==FALSE),]$bel_2030_4
  
  vect_2 <- dt_sh[
    (dt_sh$stakeholder=="Policymakers")&(is.na(dt_sh$stakeholder)==FALSE),]$bel_2030_4
  
  test_stats <- t.test(vect_1, vect_2)
  test_stats$p.value
  
  # Test significance mu 
  vect_1 <- dt_sh[
    (dt_sh$stakeholder=="Experts")&(is.na(dt_sh$stakeholder)==FALSE),]$mu
  vect_2 <- dt$mu
  
  test_stats <- t.test(vect_1, vect_2)
  test_stats$p.value
  
  vect_1 <- dt_sh[
    (dt_sh$stakeholder=="Policymakers")&(is.na(dt_sh$stakeholder)==FALSE),]$mu
  vect_2 <- dt$mu
  
  test_stats <- t.test(vect_1, vect_2)
  test_stats$p.value
  
  # Test significance norm_entropy 
  vect_1 <- dt_sh[
    (dt_sh$stakeholder=="Experts")&(is.na(dt_sh$stakeholder)==FALSE),]$norm_entropy
  vect_2 <- dt$norm_entropy
  
  test_stats <- t.test(vect_1, vect_2)
  test_stats$p.value
  
  vect_1 <- dt_sh[
    (dt_sh$stakeholder=="Policymakers")&(is.na(dt_sh$stakeholder)==FALSE),]$norm_entropy
  vect_2 <- dt$norm_entropy
  
  test_stats <- t.test(vect_1, vect_2)
  test_stats$p.value
  
  ### Confidence
  
  mean(dt_sh[
    (dt_sh$stakeholder=="Policymakers")&(is.na(dt_sh$stakeholder)==FALSE),]$bel_2030_conf)
  mean(dt_sh[
    (dt_sh$stakeholder=="Experts")&(is.na(dt_sh$stakeholder)==FALSE),]$bel_2030_conf)
  mean(dt$bel_2030_conf)
  
  ### Central expectation of SOB
  
  vect_1 <- dt$mu  
  vect_2 <- dt_sh$mu_sob
  
  test_stats <- t.test(vect_1, vect_2)
  
  mean(dt_sh$mu_sob)
  mean(dt$mu)
  
  test_stats$p.value
  
  ### NEI on sob
  
  mean(dt_sh$norm_entropy_sob)
  
  contri_entropy_1 <- mean(dt$bel_2030_1/100)*log(mean(dt$bel_2030_1/100))
  contri_entropy_2 <- mean(dt$bel_2030_2/100)*log(mean(dt$bel_2030_2/100))
  contri_entropy_3 <- mean(dt$bel_2030_3/100)*log(mean(dt$bel_2030_3/100))
  contri_entropy_4 <- mean(dt$bel_2030_4/100)*log(mean(dt$bel_2030_4/100))
  
  entropy <- contri_entropy_1 + contri_entropy_2 + 
                  contri_entropy_3 + contri_entropy_4
  
  global_nei <- -entropy/log(4)
  global_nei
  
  ### t.test for each bin
  
  mean(dt$bel_2030_1) - mean(dt_sh$sec_bel_2030_1)
  test_stats <- t.test(dt$bel_2030_1, dt_sh$sec_bel_2030_1)
  test_stats$p.value
  
  mean(dt$bel_2030_2) - mean(dt_sh$sec_bel_2030_2)
  test_stats <- t.test(dt$bel_2030_2, dt_sh$sec_bel_2030_2)
  test_stats$p.value
  
  mean(dt$bel_2030_3) - mean(dt_sh$sec_bel_2030_3)
  test_stats <- t.test(dt$bel_2030_3, dt_sh$sec_bel_2030_3)
  test_stats$p.value
  
  ### Extended Data
  
  mean(dt$bel_2030_1) - mean(dt_sh$bel_2030_1)
  test_stats <- t.test(dt$bel_2030_1, dt_sh$bel_2030_1)
  test_stats$conf.int
  test_stats$p.value
  
  mean(dt$bel_2030_3) - mean(dt_sh$bel_2030_3)
  test_stats <- t.test(dt$bel_2030_3, dt_sh$bel_2030_3)
  test_stats$conf.int
  test_stats$p.value
  
  mean(dt_sh$mu)
  mean(dt$mu)
  test_stats <- t.test(dt$mu, dt_sh$mu)
  test_stats$p.value
  
  mean(dt_sh$norm_entropy)
  mean(dt$norm_entropy)
  test_stats <- t.test(dt$norm_entropy, dt_sh$norm_entropy)
  test_stats$p.value
  
  mean(dt_sh$bel_2030_conf)
  mean(dt$bel_2030_conf)
  test_stats <- t.test(dt$bel_2030_conf, dt_sh$bel_2030_conf)
  test_stats$p.value
  
  
  
  