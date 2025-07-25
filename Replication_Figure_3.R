####################################
############ Figure 2 Merged #######
####################################

rm(list = ls(all = TRUE))

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
  #install.packages("cowplot")
  
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
library(cowplot)

}

### Dataset for the general population :
dt <- read_csv("General_population_cleaned.csv")

###################################################################
#### Estimation of a "best guess" and of the normlized entropy  ###
###################################################################

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

##################
### Figure 2 a ###
##################

{
  
#############################################################
############### Selection process begins here ###############
#############################################################

# Create dummy variable for each country :
  
dt$DE <- ifelse(dt$cntry=="DE", 1, 0)
dt$AT <- ifelse(dt$cntry=="AT", 1, 0)
dt$DK <- ifelse(dt$cntry=="DK", 1, 0)
dt$FR <- ifelse(dt$cntry=="FR", 1, 0)
dt$GR <- ifelse(dt$cntry=="GR", 1, 0)
dt$HU <- ifelse(dt$cntry=="HU", 1, 0)
dt$IT <- ifelse(dt$cntry=="IT", 1, 0)
dt$NL <- ifelse(dt$cntry=="NL", 1, 0)
dt$PL <- ifelse(dt$cntry=="PL", 1, 0)
dt$SI <- ifelse(dt$cntry=="SI", 1, 0)
dt$ES <- ifelse(dt$cntry=="ES", 1, 0)
dt$SE <- ifelse(dt$cntry=="SE", 1, 0)

### First regression, the y-variable is mu, the central expectation:
dt$y <- dt$mu

# list of the regressors:
list_x_variables <- c("trust_eu", "trust_cntry",
                      "clim_worry", "sec_ord_clim_worry", 
                      "fit_55_knowl", "female",
                      "educ_high", "val_bio",
                      "age_1839","age_6074",
                      "educ_low", "educ_high", 
                      "low_inc", "high_inc", "rural", 
                      "poli_ori", "sec_ord_clim_worry",
                      "val_tot", "val_otc", "val_str", 
                      "val_sen", "val_con", 
                      "DE", "AT", "DK", "FR", "GR", "HU",
                      "IT", "NL", "PL", "SI", "ES", "SE")

# preparing the data for the first regression:
dt_subset <- dt[c("y", list_x_variables)]

#preparing the data for the second regression:
dt$y <- dt$norm_entropy*100
dt_subset2 <- dt[c("y", list_x_variables)]


### We create the backward selection algorithm:
backward_elimination <- function(data, alpha = 0.05) {
  # Start with full model
  current_formula <- formula(paste("y ~", paste(names(data)[-1], collapse = " + ")))
  current_model <- lm(current_formula, data = data)
  
  while(TRUE) {
    # Get p-values (excluding intercept)
    p_values <- summary(current_model)$coefficients[-1, 4]
    
    # If all p-values < alpha, stop
    if(all(p_values < alpha)) break
    
    # Find predictor with highest p-value
    worst_predictor <- names(which.max(p_values))
    
    # Remove it from formula
    current_formula <- update(current_formula, paste("~ . -", worst_predictor))
    
    # Refit model
    current_model <- lm(current_formula, data = data)
    
    # Safety check
    if(length(coef(current_model)) == 1) {
      warning("All predictors removed - only intercept remains")
      break
    }
  }
  
  return(current_model)
}

### Run backward elimination for the first and the second regression
lm_tot <- backward_elimination(dt_subset, alpha = 0.01)
lm_tot2 <- backward_elimination(dt_subset2, alpha = 0.01)

summary(lm_tot)
summary(lm_tot2)

### Creation of two datasets for the figure, one for each regression
tidy_lm1 <- tidy(lm_tot2, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    model = "Belief Uncertainty",
    color = ifelse(estimate < 0, "darkred", "darkblue") 
    #red if the value is negative, blue otherwise
  )

  tidy_lm2 <- tidy(lm_tot, conf.int = TRUE) %>%
    filter(term != "(Intercept)") %>%
    mutate(
      model = "Central Expectation",
      color = ifelse(estimate < 0, "darkred", "darkblue")
    )
  
  # We manually add the predictors significant in the second 
  # regression, but absent from the first. Their values are
  # by definition equal to 0.
  tidy_lm2 <- rbind(tidy_lm2, c("High education", 0, 0, 0, 1, 0, 0, 
                                  "Central Expectation", "darkgrey"))
  tidy_lm2 <- rbind(tidy_lm2, c("Biospheric Values", 0, 0, 0, 1, 0, 0, 
                                  "Central Expectation", "darkgrey"))
  tidy_lm2 <- rbind(tidy_lm2, c("Age 18–39", 0, 0, 0, 1, 0, 0, 
                                  "Central Expectation", "darkgrey"))
  
  # We check that the variables that should be numeric are numeric
  tidy_lm2$estimate <- as.numeric(tidy_lm2$estimate)
  tidy_lm2$std.error <- as.numeric(tidy_lm2$std.error)
  tidy_lm2$statistic <- as.numeric(tidy_lm2$statistic)
  tidy_lm2$p.value <- as.numeric(tidy_lm2$p.value)
  tidy_lm2$conf.low <- as.numeric(tidy_lm2$conf.low)
  tidy_lm2$conf.high <- as.numeric(tidy_lm2$conf.high)
  
  # We create one unique dataset for the figure
  # from the two dataset above
  
  tidy_both <- bind_rows(tidy_lm2, tidy_lm1) %>%
    mutate(
      significance = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01  ~ "**",
        p.value < 0.05  ~ "*",
        TRUE ~ ""
      )
    ) %>%
    # Rename the variables with more intuitive names
    mutate(
      term = fct_recode(term,
                        "Trust in EU Parliament" = "trust_eu",            # Trust EU
                        "Climate worry" = "clim_worry",                  # Climate wo
                        "Second-order climate worry" = "sec_ord_clim_worry", # Second-order climate
                        "Age 18–39" = "age_1839",                        # Age
                        "Biospheric Values" = "val_bio",                  # Biospheric
                        "High education" = "educ_high",                   # High Educ
                        "Germany" = "DE", 
                        "Austria" = "AT",
                        "France" = "FR"
      )
    ) %>%
    # Manually set the factor levels in the desired order
    mutate(
      term = factor(term, levels = c(
        "Germany", "Austria", "France",
        "High education",
        "Biospheric Values",
        "Knowledge about EU climate policy",
        "Age 18–39",
        "Second-order climate worry",
        "Climate worry",
        "Trust in EU Parliament")),
      term_y = as.numeric(fct_rev(term)),
      posi_star = estimate
    )
  
  # We drop the values of the country fixed effects, 
  # that are not of interest
  
  tidy_both <- tidy_both[tidy_both$term != "France", ]
  tidy_both <- tidy_both[tidy_both$term != "Austria", ]
  tidy_both <- tidy_both[tidy_both$term != "Germany", ]
  
  tidy_both$model <- factor(tidy_both$model, 
                            levels = rev(sort(unique(tidy_both$model))))
  
  # We save the plot under "p2a" for picture 2 a. 
  
  p2a <- ggplot(tidy_both, aes(x = estimate, y = term)) +
    geom_point(aes(color = color), size = 3) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, color = color), height = 0.2) +
    geom_text(
      aes(label = significance),
      nudge_y = 0.15,
      color = "black", size = 4, vjust = 0
    ) +
    facet_wrap(~model, scales = "free_x") +
    scale_color_identity() +
    scale_x_continuous(expand = expansion(mult = c(0.05, 0.1))) +  # add breathing room
    theme_minimal() +
    theme(
      strip.text = element_text(size = 13, face = "bold"),
      axis.text.y = element_text(size = 12, color = "black"),
      axis.text.x = element_text(size = 11, color = "black"),
      axis.title = element_text(size = 12),
      plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
      legend.position = "none",
      panel.spacing = unit(2, "lines"),
      panel.border = element_rect(color = "gray80", fill = NA, linewidth = 0.5)
    ) +
    labs(
      title = "Determinants of Beliefs",
      x = "Marginal Effect (Percentage Point)",
      y = NULL
    ) +
    coord_cartesian(clip = "off")

}

#############
# Figure b  #
#############
  
{

# We create a variable from -5 to 5 for the support
# given to five policies of the package "fit-for-55".
n_obs <- nrow(dt)

dt$sup_fit_55_pol1 <- dt$ETS_gen_support
dt$sup_fit_55_pol2 <- dt$ETS_II_transp
dt$sup_fit_55_pol3 <- dt$ETS_II_heat
dt$sup_fit_55_pol4 <- dt$CBAM
dt$sup_fit_55_pol5 <- dt$car_phase_exc

norm_pol <- function(x){
  output <- ifelse(x<4, -1,
                   ifelse(x==4, 0, 1))
  output 
}

sup_55 <- function(x1, x2, x3, x4, x5){
  output <- norm_pol(x1) + norm_pol(x2) + norm_pol(x3) +
    norm_pol(x4) + norm_pol(x5)
  output
}

dt$sup_fit_55 <-  sup_55(dt$sup_fit_55_pol1, 
                         dt$sup_fit_55_pol2,
                         dt$sup_fit_55_pol3,
                         dt$sup_fit_55_pol4,
                         dt$sup_fit_55_pol5)

# We create 3 terciles based on mu

ds <- dt %>%
  mutate(
    wm_tercile = ntile(mu, 3),
    tercile_label = factor(wm_tercile,
                           levels = 1:3,
                           labels = c("Low", "Mid", "High"))
  )

t.test(ds$sup_fit_55[ds$wm_tercile==1],
       ds$sup_fit_55[ds$wm_tercile==2])
t.test(ds$sup_fit_55[ds$wm_tercile==2],
       ds$sup_fit_55[ds$wm_tercile==3])
t.test(ds$sup_fit_55[ds$wm_tercile==1],
       ds$sup_fit_55[ds$wm_tercile==3])

# we create the summary dataset for the figure
summary_tbl <- ds %>%
  group_by(tercile_label) %>%
  summarise(
    mean_support = mean(sup_fit_55, na.rm = TRUE),
    n            = sum(!is.na(sup_fit_55)),
    sd           = sd(sup_fit_55, na.rm = TRUE),
    se           = sd / sqrt(n),
    t_crit       = qt(0.975, df = n - 1),
    ci_lower     = mean_support - t_crit * se,
    ci_upper     = mean_support + t_crit * se
  )

# We save the figure in p2b
p2b <- ggplot(summary_tbl, aes(x = tercile_label, y = mean_support, fill = tercile_label)) +
  geom_bar(stat = "identity", alpha = 0.8, width = 0.7) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, color = "black") +
  scale_fill_manual(values = c("steelblue","steelblue", "steelblue")) +
  labs(
    title = "Beliefs and Policy Support",
    x = "Terciles of Central Expectations",
    y = "Policies Supported (-5 to 5)",
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11)
  ) +
  geom_signif(
    annotations =  "***",
    xmin = 1.25,
    xmax = 1.75,
    y_position = 1.25,
    tip_length = 0.01,
    vjust = 0.5,
    textsize = 4
  ) +
  geom_signif(
    annotations =  "***",
    xmin = 2.25,
    xmax = 2.75,
    y_position = 1.75,
    tip_length = 0.01,
    vjust = 0.5,
    textsize = 4
  ) +
  geom_signif(
    annotations =  "***",
    xmin = 1.25,
    xmax = 2.75,
    y_position = 2,
    tip_length = 0.01,
    vjust = 0.5,
    textsize = 4
  )

}


############################
# Combining the two graphs #
############################

p2_combined <- plot_grid(
  p2a,
  p2b + theme(legend.position = "none"),
  nrow = 1,
  rel_widths = c(2, 1),
  labels = c("a", "b"),
  align = "h",   # horizontal alignment
  axis = "tb"    # align top and bottom axes
)

print(p2_combined)

ggsave("Beliefs_figure_2_final.png", 
       plot = p2_combined, width = 10, 
       height = 5, dpi = 300,
       bg = "white")
