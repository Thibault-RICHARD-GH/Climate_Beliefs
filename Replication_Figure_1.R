####################################
############ Figure 1 Merged #######
####################################

### Define folder
setwd(".")

# Alternatively, define the folder manually. Ex: 
folder <- "~/Documents/Project Capable - Code and picture"
setwd(folder)

### I create a function to detach
### all the packages (some functions
### are problematic in some packages)

detach_all_packages <- function() {
  base_packages <- c("package:stats", "package:graphics", "package:grDevices", 
                     "package:utils", "package:datasets", "package:methods", "package:base")
  
  # Find all attached packages
  attached_packages <- search()[grepl("^package:", search())]
  
  # Detach all non-base packages
  for (pkg in setdiff(attached_packages, base_packages)) {
    detach(pkg, character.only = TRUE, unload = TRUE)
  }
}

detach_all_packages()


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

#EU27 emissions
# Import data emissions

{
  
eurostat_eea_ghg <- get_eurostat("env_air_gge")

eurostat_emi <- eurostat_eea_ghg %>% 
  filter(airpol %in% c("CO2", "GHG") & geo=="EU27_2020" & src_crf=="TOTX4_MEMO" & unit=="MIO_T") %>% 
  mutate(year=year(TIME_PERIOD), e = ifelse(airpol=="CO2","co2", "kghg"), value=values) %>% 
  select(geo,year,e,value)

emi <- eurostat_emi %>% 
  filter(e=="kghg") %>% 
  mutate(Historical=value) %>% 
  select(year, Historical)

#Reference values
#2040 impact assessment

policy_emi <- data.frame(year=c(2030,2050), target=c(5640*(1-0.55), 0))
policy_emi <- data.frame(year=c(2030,2040,2050), target=c(4867.243*(1-0.55), 4867.243*(1-0.90), 0))
emi <- emi %>% full_join(policy_emi) 

emi$Historical[emi$year==2023] <- NA # THey are only preliminary estimation

}

# Data cleaning with the beliefs

{

# I import the dataset for the general population
dt <- read_csv("General_population_cleaned.csv")
# I get the stakeholders
dt_sh <- read_csv("Stakeholders_cleaned.csv")

dt_sh <- dt_sh[is.na(dt_sh$stakeholder)==FALSE, ]
#I create combined dataset for groups stakeholders (General Public, Policymakers, Experts)
dt_merged <- dt %>% 
  select(starts_with("bel_2030")) %>% 
  mutate(stakeholder="General Public") %>% 
  rbind(dt_sh %>% select(stakeholder, starts_with("bel_2030")))


}

# Define the color palette for stakeholder groups
stakeholder_colors <- c("General Public" = "#3366CC", 
                        "Experts" = "#006633", 
                        "Policymakers" = "#CC0000")
stakeholder_positions <- c("General Public" = 2030, 
                           "Experts" = 2030, 
                           "Policymakers" = 2030)
stakeholder_alpha <- c("General Public" = 0.8, 
                       "Experts" = 0.5, 
                       "Policymakers" = 0.3)

# Define the baseline emissions (1990) for calculating reductions
emission_1990 <- emi$Historical[emi$year==1990]

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

### I compute the average partition for each subgroup:
stakeholder_stats <- stakeholder_data %>%
  group_by(stakeholder) %>%
  summarize(
    mean_mu = mean(mu, na.rm = TRUE),
    sd_mu = sd(mu, na.rm = TRUE),
    mean_emissions = mean(emissions_2030, na.rm = TRUE),
    sd_emissions = sd(emissions_2030, na.rm = TRUE),
    m_bel_2030_1 = mean(bel_2030_1, na.rm = TRUE),
    m_bel_2030_2 = mean(bel_2030_2, na.rm = TRUE),
    m_bel_2030_3 = mean(bel_2030_3, na.rm = TRUE),
    m_bel_2030_4 = mean(bel_2030_4, na.rm = TRUE),
    n = n()
  )

# I compute for each group the best-fitting 
# distribution based on the average partition

for(i in 1:nrow(stakeholder_stats)) {
  
    fn_to_optim <- function(x) {
      Kullback_Leibler(x[1], x[2], 
                       stakeholder_stats$m_bel_2030_1[i], 
                       stakeholder_stats$m_bel_2030_2[i], 
                       stakeholder_stats$m_bel_2030_3[i], 
                       stakeholder_stats$m_bel_2030_4[i])
    }
    
    para <- optim(c(50, 0), fn_to_optim)
  
  stakeholder_stats$mean_emissions[i] <- (100-para$par[1])*emission_1990/100
  stakeholder_stats$sd_emissions[i] <- exp(para$par[2])*emission_1990/100
  
}


# Function to generate normal distribution points based on mean and sd
generate_normal_distribution <- function(mean_val, sd_val, stakeholder, 
                                         pos_x, max_density, 
                                         n_points = 10000) {
  
  # Ensure a range consistent with the truncattion in 0% and 100% in the estimates
  x_range <- seq(0, emission_1990, length.out = n_points)
  # Calculate density values
  density_vals <- dnorm(x_range, mean = mean_val, sd = sd_val)/(
    pnorm(emission_1990, mean = mean_val, sd = sd_val) - 
      pnorm(0, mean = mean_val, sd = sd_val))
  width_factor <- 8
  
  # Create a data frame with the results
  df <- data.frame(
    y = x_range,
    density = density_vals,
    x = pos_x,
    x_position = pos_x + (density_vals / max_density * width_factor),
    stakeholder = stakeholder
  )
  
  return(df)
}

# Generate normal distribution data for each stakeholder group
normal_dist_data <- list()

# First, I determine the maximal density for 
# each group for the appropriate scaling 

max_density_by_st <- dnorm(stakeholder_stats$mean_emissions, 
                           mean = stakeholder_stats$mean_emissions, 
                           sd = stakeholder_stats$sd_emissions)/(
                             pnorm(emission_1990, 
                                   mean = stakeholder_stats$mean_emissions, 
                                   sd = stakeholder_stats$sd_emissions) - 
                               pnorm(0, 
                                     mean = stakeholder_stats$mean_emissions, 
                                     sd = stakeholder_stats$sd_emissions))

# I keep as a constant the maximal density
# across all the three groups
max_density <- max(max_density_by_st)

# I generate the average distribution based on functions above

for(i in 1:nrow(stakeholder_stats)) {
  
  group <- stakeholder_stats$stakeholder[i]
  mean_val <- stakeholder_stats$mean_emissions[i]
  sd_val <- stakeholder_stats$sd_emissions[i]
  pos_x <- stakeholder_positions[group]
  
  normal_dist_data[[group]] <- generate_normal_distribution(mean_val, sd_val, 
                                                            group, pos_x, 
                                                            max_density)
  
}

# Combine all normal distribution data
all_normal_data <- do.call(rbind, normal_dist_data)

# Function to create overlaid density plots for mu and entropy by stakeholder
create_overlaid_distribution <- function(var_name, title, x_label, plot_type = "density") {
  if (plot_type == "density") {
    p <- ggplot(stakeholder_data, aes_string(x = var_name, fill = "stakeholder", color = "stakeholder")) +
      geom_density(alpha = 0.3) +
      geom_vline(data = aggregate(as.formula(paste0(var_name, "~ stakeholder")), 
                                  stakeholder_data, mean),
                 aes(xintercept = get(var_name), color = stakeholder),
                 linetype = "dotted", size = 1)
  } else {
    # For histograms, layer them with general public at bottom, then experts, then policymakers
    data_ordered <- stakeholder_data
    # Reverse the factor levels so policymakers are drawn last (on top)
    data_ordered$stakeholder <- factor(data_ordered$stakeholder, 
                                       levels = c("General Public", "Experts", "Policymakers"))
    
    p <- ggplot(data_ordered, aes_string(x = var_name, fill = "stakeholder", color = "stakeholder")) +
      geom_histogram(alpha = 0.6, position = "identity", bins = 30) +
      geom_vline(data = aggregate(as.formula(paste0(var_name, "~ stakeholder")), 
                                  data_ordered, mean),
                 aes(xintercept = get(var_name), color = stakeholder),
                 linetype = "dotted", size = 1)
  }
  
  # Add EU target line for mu plot
  if (var_name == "mu") {
    p <- p + geom_vline(xintercept = 55, color = "#444444", 
                        linetype = "solid", linewidth = 1) +
      scale_x_continuous(
        breaks = c(30, 40, 50, 55, 60),         # custom breaks
        limits = c(25, 60))
  }
  
  p + labs(
    title = title,
    x = x_label,
    y = ifelse(plot_type == "density", "Density", "Count")
  ) +
    scale_fill_manual(values = stakeholder_colors) +
    scale_color_manual(values = stakeholder_colors) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 11, face = "bold"),
      axis.title = element_text(size = 9),
      legend.position = "none",
      legend.title = element_blank()
    )
}

# Function to properly normalize the density values to ensure they integrate to 1
normalize_densities <- function(density_data) {
  # Split the data by stakeholder group
  stakeholder_groups <- unique(density_data$stakeholder)
  normalized_data <- list()
  
  for (group in stakeholder_groups) {
    # Get the data for this stakeholder
    group_data <- density_data[density_data$stakeholder == group, ]
    
    # Sort by y to ensure proper integration
    group_data <- group_data[order(group_data$y), ]
    
    # Calculate step size (difference between consecutive y values)
    dy <- diff(group_data$y)
    
    # Calculate the current approximate area using trapezoidal rule
    heights <- group_data$density
    avg_heights <- (heights[-1] + heights[-length(heights)]) / 2
    current_area <- sum(dy * avg_heights)
    
    # Scale the density values to make the area equal to 1
    scaling_factor <- 1 / current_area
    group_data$density <- group_data$density * scaling_factor
    
    # Recalculate x_position with normalized density values
    # This maintains the visual width but ensures proper normalization
    width_factor <- 8  # Adjust as needed for visual clarity
    #get the height based on sigma

    group_data$x_position <- group_data$x + (group_data$density / max_density * width_factor)
    
    # Store the normalized data
    normalized_data[[group]] <- group_data
  }
  
  # Combine all normalized data
  result <- do.call(rbind, normalized_data)
  return(result)
}

# Apply the normalization function to your data
normalized_normal_data <- normalize_densities(all_normal_data)

# I check that the average of the "x_position" is the 
# same for all the three groups, which implies that
# the areas under the density curves are the same
mean(normalized_normal_data$x_position[normalized_normal_data$stakeholder=="Experts"])
mean(normalized_normal_data$x_position[normalized_normal_data$stakeholder=="General Public"])
mean(normalized_normal_data$x_position[normalized_normal_data$stakeholder=="Policymakers"])


# Create the main emissions plot with normal distributions
p_main_normal <- ggplot() +
  # Historical emissions line
  geom_line(data = emi, aes(x = year, 
                            y = Historical, 
                            color="Historical"), 
            size = 1) +
  geom_line(data = emi %>% 
              filter(year %in% c(2022, 2030, 2040, 2050)) %>% 
              mutate(target=
                       ifelse(year==2022, Historical, target)), 
            aes(x = year, y = target, color="Interpolation"), 
            linetype="dashed", size = 1) +
  
  # Add the normal distribution curves for each stakeholder group
  geom_polygon(data = normalized_normal_data, 
               aes(x = x_position, y = y, group = stakeholder, fill = stakeholder, alpha = stakeholder)) +
  # Target points and line
  geom_point(data = emi[!is.na(emi$target), ], 
             aes(x = year, y = target, color="EU target"), 
             size = 3) +
  geom_point(data = emi[emi$year==2022, ], 
             aes(x = year, y = Historical, color="2022"), 
             size = 3) +
  
  # Add annotations for means
  annotate("text", x = 2027, y = emission_1990 * (1-0.55), 
           label = "55%", 
           hjust = 0, color = "#444444") +
  
  annotate("text", x = (2022 + 1), y = emission_1990 * (1-0.3072), 
           label = "32.5%", 
           hjust = 0, color = "#444444") +
  
  annotate("text", x = (2040 - 3.75), y = emission_1990 * (1-0.90), 
           label = "90%", 
           hjust = 0, color = "#444444") +
  
  annotate("text", x = (2050 - 4), y = 0, 
           label = "100%", 
           hjust = 0, color = "#444444") +
  
  # Add mean values for each stakeholder group
  geom_point(data = stakeholder_stats,
             aes(x = 2030, 
                 y = mean_emissions, 
                 color = stakeholder), 
             size = 3) +
  
  geom_text(data = stakeholder_stats,
            aes(x = 2027, y = mean_emissions,
                label = paste0(round(mean_mu), "%"), 
                color = stakeholder),
            hjust = 0, show.legend = FALSE) +
  
  # Formatting
  scale_fill_manual(values = stakeholder_colors) +
  scale_alpha_manual(values = stakeholder_alpha) +
  scale_color_manual(values = c(stakeholder_colors, "Historical"="grey20", "EU target"="black", 
                                "2022 (prelim.)"="grey50", "Interpolation"="grey70")) +
  labs(title = "EU Greenhouse Gas Emissions (1990-2050)",
       subtitle = "Historical data, targets, and belief distributions by stakeholder group",
       x = "",
       y = "EU27 GHG Emissions (MtCO2e)") +
  
  # Set x-axis limits to end exactly at 2050
  scale_x_continuous(limits = c(1990, 2050), breaks = seq(1990, 2050, by = 10)) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 11),
    legend.position = "right"
  ) + 
  labs(color="", fill="Groups") +
  guides(alpha="none")


# Modified function to create histograms with correct percentage scale
create_percentage_histogram <- function(var_name, title, x_label) {
  # Get range of the variable to set consistent bins across all groups
  var_min <- min(stakeholder_data[[var_name]], na.rm = TRUE)
  var_max <- max(stakeholder_data[[var_name]], na.rm = TRUE)
  
  # Number of bins to use
  num_bins <- 30
  
  # Calculate bin width
  bin_width <- (var_max - var_min) / num_bins
  
  # Create a list to store individual histograms for each stakeholder
  histograms <- list()
  max_percentage <- 0
  
  # Process each stakeholder group separately
  for (group in unique(stakeholder_data$stakeholder)) {
    # Filter data for this stakeholder group
    group_data <- stakeholder_data[stakeholder_data$stakeholder == group, ]
    
    # Calculate histogram with consistent bins
    hist_data <- hist(group_data[[var_name]], 
                      breaks = seq(var_min, var_max, length.out = num_bins + 1),
                      plot = FALSE)
    
    # Convert counts to percentages
    percentages <- hist_data$counts / sum(hist_data$counts) * 100
    
    # Record maximum percentage for annotation positioning
    max_percentage <- max(max_percentage, max(percentages, na.rm = TRUE))
    
    # Create data frame for plotting
    plot_data <- data.frame(
      x = hist_data$mids,
      percentage = percentages,
      stakeholder = group
    )
    
    histograms[[group]] <- plot_data
  }
  
  # Combine all histogram data
  plot_data <- do.call(rbind, histograms)
  
  # Reverse the factor levels so policymakers are drawn last (on top)
  plot_data$stakeholder <- factor(plot_data$stakeholder, 
                                  levels = c("General Public", "Experts", "Policymakers"))
  
  # Create histogram with percentage y-axis
  p <- ggplot(plot_data, aes(x = x, y = percentage, fill = stakeholder, color = stakeholder, alpha = stakeholder)) +
    geom_bar(stat = "identity", position = "identity", width = bin_width * 0.9) +
    # Add mean lines for each group
    geom_vline(data = aggregate(as.formula(paste0(var_name, "~ stakeholder")), 
                                stakeholder_data, mean),
               aes(xintercept = get(var_name), color = stakeholder),
               linetype = "dotted", size = 1) +
    # Set specific alpha values
    scale_alpha_manual(values = stakeholder_alpha)
  
  # Add EU target line for mu plot if applicable
  if (var_name == "mu") {
    p <- p + geom_vline(xintercept = 55, color = "#444444", 
                        linetype = "solid", linewidth = 1) +
      scale_x_continuous(
        breaks = c(30, 40, 50, 55, 60),         # custom breaks
        limits = c(25, 60)) #+
      #annotate("text", x = 56, y = max_percentage * 0.9, 
      #         label = "EU 2030", hjust = 0, color = "#444444") +
      #annotate("text", x = 55, y = -3, size = 2, 
      #         label = "55%", hjust = 0, color = "#444444")
  }
  
  # Format the plot
  p <- p + 
    scale_fill_manual(values = stakeholder_colors) +
    scale_color_manual(values = stakeholder_colors) +
    scale_y_continuous(name = "Percentage within group") +
    labs(
      title = title,
      x = x_label
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 12),
      legend.position = "none"  # Remove the legend completely
    )
  
  return(p)
}

# Update the function to use the new percentage histograms
generate_stakeholder_plots_percentage <- function() {
  # Create distribution plots with percentage scale
  p_mu <- create_percentage_histogram("mu", "Distributions of Central Expectations", 
                                      "Reduction from 1990 (%)")
  
  p_entropy <- create_percentage_histogram("norm_entropy", 
                                           "Distributions of Belief Uncertainty", 
                                           "Normalized Entropy")
  
  # Arrange the plots in a 2x2 grid with main plot spanning both columns
  combined_plot <- plot_grid(
    p_main_normal,
    plot_grid(p_mu, p_entropy, ncol = 2, labels = c("b", "c")),
    nrow = 2,
    rel_heights = c(2, 1),
    labels = c("a", "")
  )
  
  return(combined_plot)
}

# Create new combined plot with percentage histograms
combined_percentage_plot <- generate_stakeholder_plots_percentage()
print(combined_percentage_plot)
ggsave("Beliefs_figure_1_final.png", plot = combined_percentage_plot, width = 10, height = 8, dpi = 300, bg = "white")

