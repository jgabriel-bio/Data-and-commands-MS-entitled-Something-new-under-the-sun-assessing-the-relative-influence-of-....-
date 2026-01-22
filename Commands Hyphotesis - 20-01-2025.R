####Importing data####
data=read.csv2('data_complete.csv',h=T,stringsAsFactors = T,dec=',')

#Packages
library(ggplot2)
library(ggridges)
library(dplyr)
library(ggdist)
library(car)
library(glmmTMB)
library(lmtest)
library(cowplot)
library(tidyr)


####Figure males x females per month####

#mean + EP per month and sex

data_summary <- data_long %>%
  group_by(month, sex) %>%
  summarise(
    mean_n = mean(n_individuals, na.rm = TRUE),
    se_n = sd(n_individuals, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

tiff("Figure 1.tiff", 
     width = 2000, height = 1500, res = 300, compression = "lzw")
ggplot(
  data_summary,
  aes(
    x = month,
    y = mean_n,
    group = sex,
    color = sex
  )
) +
  geom_line(size = 1) +             
  geom_point(size = 3) +
  geom_errorbar(
    aes(
      ymin = mean_n - se_n,
      ymax = mean_n + se_n
    ),
    width = 0.2
  ) +
  scale_color_manual(
    values = c(
      "Male" = "blue",
      "Female" = "orange"
    )
  ) +
  labs(
    x = "Month",
    y = "Average number of individuals"
  ) +
  theme_classic() +
  theme(
    legend.title = element_blank(),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    axis.line = element_line(color = "black")
  )
dev.off()


####Figure males x females per hour with 0 values####

# Data frame with mean and variance of number of males---
View(data_summary_both)
data_summary_both <- data %>%
  group_by(hour) %>%
  summarise(
    n_males    = sum(!is.na(num_territories_occupied)),
    mean_males = mean(num_territories_occupied, na.rm = TRUE),
    sd_males   = sd(num_territories_occupied, na.rm = TRUE),
    
    n_females    = sum(!is.na(total_female_presence)),
    mean_females = mean(total_female_presence, na.rm = TRUE),
    sd_females   = sd(total_female_presence, na.rm = TRUE)
  ) %>%
  mutate(
    se_males   = sd_males / sqrt(n_males),
    se_females = sd_females / sqrt(n_females)
  )

#Creating figure
tiff("Figure 2.tiff", 
     width = 2000, height = 1500, res = 300, compression = "lzw")
ggplot() +
  
  # --- Males (orange) ---
  geom_line(
    data = data_summary_both,
    aes(x = hour, y = mean_males),
    color = "orange",
    size = 1
  ) +
  geom_point(
    data = data_summary_both,
    aes(x = hour, y = mean_males),
    color = "orange",
    size = 3
  ) +
  geom_errorbar(
    data = data_summary_both,
    aes(
      x = hour,
      ymin = mean_males - se_males,
      ymax = mean_males + se_males
    ),
    width = 0.20,
    color = "orange"
  ) +
  
  # --- Females (blue) ---
  geom_line(
    data = data_summary_both,
    aes(x = hour, y = mean_females),
    color = "blue",
    size = 1
  ) +
  geom_point(
    data = data_summary_both,
    aes(x = hour, y = mean_females),
    color = "blue",
    size = 3
  ) +
  geom_errorbar(
    data = data_summary_both,
    aes(
      x = hour,
      ymin = mean_females - se_females,
      ymax = mean_females + se_females
    ),
    width = 0.20,
    color = "blue"
  ) +
  
  labs(
    x = "Hour of the day",
    y = "Average number of individuals",
    title = ""
  ) +
  
  scale_x_continuous(breaks = unique(data_summary_both$hour)) +
  
  theme_classic() +
  theme(
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    axis.line = element_line(color = "black")
  )
dev.off()


####Testing the hypotheses####
summary(data)

#Hip. 1: Light radiation is the main determinant of territorial behavior investment by males
#Hip. 2: Temperature is the main determinant of mate search investment by females

#GHI represents - Clear sky GHI. Clear sky global irradiation on horizontal plane at ground level (Wh/m2) - A LIGHT RADIATION INTENSITY PROXY

#####Scaling the predictor variableS####
data$temperature1 <- scale(data$temperature)[,1]
data$GHI1 <- scale(data$GHI) [,1]

#Testing if explanatory variables are correlated
View(data)
cor(data[,c(14,15)]) 
#Results show a low correlation, as they are not high correlated I follow the test using them as separated variables 


#####Hypothesis 1: territorial investment by males####

###Testing VIF model###
model_vif <- lm(
  num_territories_occupied ~ 
    poly(temperature1, 2) +
    poly(GHI1, 2) +
    poly(temperature1, 2) * poly(GHI1, 2),
  data = data
)
vif_raw <- vif(model_vif)

# GVIF adjusted for comparasion
vif_adj <- vif_raw^(1 / (2 * attr(vif_raw, "Df")))

print(vif_raw)

#GVIF values are low (column of GVIF adjusted), therefore I can follow the analysis using the variables separated

#As I collected excess of zeros in the response variable (count), I decided to use a zero-inflated model

#Poisson model altered in zero (maintaining the random effect)

# Zero-inflated Poisson with random effects
model_zip_male <- glmmTMB(num_territories_occupied ~ poly(temperature1, 2) +  poly(GHI1, 2) + poly(temperature1, 2) * poly(GHI1, 2) + (1|date), 
                          ziformula = ~1,  # zero-inflation formula
                          family = poisson,
                          data = data)
summary(model_zip_male)

# Zero-inflated Poisson null model
model_zip_male_H0 <- glmmTMB(num_territories_occupied ~ 1 + (1|date), 
                             ziformula = ~1,  # zero-inflation formula
                             family = poisson,
                             data = data)

anova(model_zip_male, model_zip_male_H0, test="Chi") #p>0,05
#The model is better than the null model, therefore the variance of response variable is explained by one of the predictors
#I compared the models to identify the main determinant of this variation


# Zero-inflated Poisson without interaction
model_zip_male_v2 <- glmmTMB(num_territories_occupied ~ poly(temperature1, 2) + poly(GHI1, 2) + (1|date), 
                               ziformula = ~1,  # zero-inflation formula
                               family = poisson,
                               data = data)
summary(model_zip_male_v2)

anova(model_zip_male_v2, model_zip_male, test="Chi") #p=0,15, this value indicates that GHI * temperature is not important for the model


# Zero-inflated Poisson without light radiation
model_zip_male_v3 <- glmmTMB(num_territories_occupied ~ poly(temperature1, 2) +  (1|date), 
                             ziformula = ~1,  # zero-inflation formula
                             family = poisson,
                             data = data)


anova(model_zip_male_v2, model_zip_male_v3, test="Chi") #p=0,02, this value indicates that GHI is important for the model

# Zero-inflated Poisson without temperature
model_zip_male_v4 <- glmmTMB(num_territories_occupied ~ poly(GHI1, 2) + (1|date), 
                             ziformula = ~1,  # zero-inflation formula
                             family = poisson,
                             data = data)
anova(model_zip_male_v4, model_zip_male_v2, test="Chi") #p<0,0005, this value indicate that temperature explains number of territories occupied


######Measuring the effect of each variable#### 

model_zip_male_v4 <- glmmTMB(num_territories_occupied ~ poly(GHI1, 2) + poly(temperature1, 2) +(1|date), 
                             ziformula = ~1,  # zero-inflation formula
                             family = poisson,
                             data = data)

#Both temperature and GHI predicted the number of males in territories variance(). However, the temperature had a R2 of , while GHI had a R2 of.
summary(model_zip_male_v4)
#estimated coefficients for the linear and quadratic terms, along with their corresponding p-values and confidence intervals

# Coefficient table with confidence intervals
confint(model_zip_male_v4)

######R square of each variable####

# Function to calculate McFadden's pseudo R²
mcfadden_r2 <- function(model, null_model) {
  1 - as.numeric(logLik(model) / logLik(null_model))
}

# Null model (only intercept + random effect)
null_model <- glmmTMB(
  num_territories_occupied ~ 1 + (1 | date),
  ziformula = ~1,
  family = poisson,
  data = data
)

# Full model
model_zip_male <- glmmTMB(
  num_territories_occupied ~ poly(temperature1, 2) + poly(GHI1, 2) + (1 | date),
  ziformula = ~1,
  family = poisson,
  data = data
)

# Model with only temperature
model_temp <- glmmTMB(
  num_territories_occupied ~ poly(temperature1, 2) + (1 | date),
  ziformula = ~1,
  family = poisson,
  data = data
)

# Model with only GHI
model_GHI <- glmmTMB(
  num_territories_occupied ~ poly(GHI1, 2) + (1 | date),
  ziformula = ~1,
  family = poisson,
  data = data
)

# Calculate marginal R² for each block
r2_temp <- mcfadden_r2(model_temp, null_model)
r2_GHI  <- mcfadden_r2(model_GHI, null_model)
r2_full <- mcfadden_r2(model_zip_male, null_model)

# Output
data.frame(
  Predictor_Block = c("poly(temperature1, 2)", "poly(GHI1, 2)", "Full model"),
  McFadden_R2 = c(r2_temp, r2_GHI, r2_full)
)

#McFadden Pseudo R2 for temperature = 0.0169
#McFadden Pseudo R2 for GHI = 0.0109
#Full model R2 = 0.0225

######Figure of temperature#### 

#Model without temperature scaled to create the figure
# Zero-inflated Poisson without UV radiation
model_zip_male_v5 <- glmmTMB(num_territories_occupied ~ poly(temperature, 2) + (1|date), 
                             ziformula = ~1,  # zero-inflation formula
                             family = poisson,
                             data = data)

# Create predictions for plotting
temp_range <- seq(min(data$temperature, na.rm = TRUE), 
                  max(data$temperature, na.rm = TRUE), 
                  length.out = 40)
# Create prediction data frame
pred_data <- data.frame(
  temperature = temp_range,
  date = data$date[1]  # Use first date as reference
)

# Generate predictions (population-level, excluding random effects)
predictions <- predict(model_zip_male_v5, 
                       newdata = pred_data, 
                       type = "response",
                       re.form = NA)  # Exclude random effects for population curve

pred_data$predicted <- predictions

# Create the plot
plot1 <- ggplot() +
  # Add raw data points
  geom_point(data = data, 
             aes(x = temperature, y = num_territories_occupied),
             alpha = 0.4, size = 2, color = "orange") +
  
  # Add prediction line
  geom_line(data = pred_data, 
            aes(x = temperature, y = predicted),
            color = "orange", size = 1.2) +
  
  # Labels
  labs(x = "", 
       y = "Number of males in mating territories",
       title = "") +
  
  scale_y_continuous(breaks = seq(0, 20, 1)) +   # more y-axis values here
  scale_x_continuous(breaks = seq(15, 40, 2)) + # adjust min, max, step as needed
  
  # Custom theme without grid lines and black axes
  theme_classic() +
  theme(
    panel.grid = element_blank(),          # Remove all grid lines
    axis.line = element_line(color = "black", size = 0.5),
    axis.text = element_text(color = "black", size = 10),
    axis.title = element_text(color = "black", size = 12),
    # Move y-axis label further from axis
    axis.title.y = element_text(margin = margin(r = 15)),  # increase r for more distance
    # Move x-axis label further from axis
    axis.title.x = element_text(margin = margin(r = 15)),  # increase r for more distance
    plot.title = element_text(color = "black", size = 14, hjust = 0.5),
    axis.ticks = element_line(color = "black")
  )


# Display the plot
print(plot1)



######Figure of GHI####

#Model without GHI scaled to create the figure
# Zero-inflated Poisson without temperature
model_zip_male_v6 <- glmmTMB(num_territories_occupied ~ poly(GHI, 2) + (1|date), 
                             ziformula = ~1,  # zero-inflation formula
                             family = poisson,
                             data = data)

# Create predictions for plotting
ghi_range <- seq(min(data$GHI, na.rm = TRUE), 
                 max(data$GHI, na.rm = TRUE), 
                 length.out = 40)
# Create prediction data frame
pred_data <- data.frame(
  GHI = ghi_range,
  date = data$date[1]  # Use first date as reference
)

# Generate predictions (population-level, excluding random effects)
predictions <- predict(model_zip_male_v6, 
                       newdata = pred_data, 
                       type = "response",
                       re.form = NA)  # Exclude random effects for population curve

pred_data$predicted <- predictions

# Create the plot
plot2 <- ggplot() +
  # Add raw data points
  geom_point(data = data, 
             aes(x = GHI, y = num_territories_occupied),
             alpha = 0.4, size = 2, color = "orange") +
  
  # Add prediction line
  geom_line(data = pred_data, 
            aes(x = GHI, y = predicted),
            color = "orange", size = 1.2) +
  
  # Labels
  labs(x = "", 
       y = "",
       title = "") +
  
  scale_y_continuous(breaks = seq(0, 20, 1)) +   # more y-axis values here
  # More ticks on x-axis
  # Custom theme without grid lines and black axes
  theme_classic() +
  theme(
    panel.grid = element_blank(),          # Remove all grid lines
    axis.line = element_line(color = "black", size = 0.5),
    axis.text = element_text(color = "black", size = 10),
    axis.title = element_text(color = "black", size = 12),
    # Move x-axis label further from axis
    axis.title.x = element_text(margin = margin(r = 15)),  # increase r for more distance
    plot.title = element_text(color = "black", size = 14, hjust = 0.5),
    axis.ticks = element_line(color = "black")
  )

# Display the plot
print(plot2)

#####Hyphotesis 2: female mate search####

###Testing VIF model###
model_vif_2 <- lm(
  total_female_presence ~ 
    poly(temperature1, 2) +
    poly(GHI1, 2) +
    poly(temperature1, 2) * poly(GHI1, 2),
  data = data
)
vif_raw <- vif(model_vif_2)

# GVIF adjusted for comparasion
vif_adj <- vif_raw^(1 / (2 * attr(vif_raw, "Df")))

print(vif_raw)

#GVIF values are low (column of GVIF adjusted), therefore I can follow the analysis using the variables separated

#As I collected excess of zeros in the response variable (count), I decided to use a zero-inflated model

# Zero-inflated Poisson with random effects
model_zip_female <- glmmTMB(total_female_presence ~ poly(temperature1, 2) +  poly(GHI1, 2) + poly(temperature1, 2) * poly(GHI1, 2) + (1|date), 
                            ziformula = ~1,  # zero-inflation formula
                            family = poisson,
                            data = data)
summary(model_zip_female)

# Zero-inflated Poisson null model
model_zip_female_H0 <- glmmTMB(total_female_presence ~ 1 + (1|date), 
                               ziformula = ~1,  # zero-inflation formula
                               family = poisson,
                               data = data)

anova(model_zip_female, model_zip_female_H0, test="Chi") #p>0,005
#The model is better than the null model, therefore the variance of response variable is explained by one of the predictors
#I compared the models to identify the main determinant of this variation

# Zero-inflated Poisson without interaction
model_zip_female_v2 <- glmmTMB(total_female_presence ~ poly(temperature1, 2) + poly(GHI1, 2) + (1|date), 
                               ziformula = ~1,  # zero-inflation formula
                               family = poisson,
                               data = data)


anova(model_zip_female_v2, model_zip_female, test="Chi") #p=0,48, this value indicates that GHI * temperature is not important for the model

# Zero-inflated Poisson without temperature
model_zip_female_v3 <- glmmTMB(total_female_presence ~ poly(GHI1, 2) + (1|date), 
                               ziformula = ~1,  # zero-inflation formula
                               family = poisson,
                               data = data)
anova(model_zip_female_v3, model_zip_female, test="Chi") #Temperature is important
#p<0,0005, this value indicate that temperature explains number of territories occupied

# Zero-inflated Poisson without GHI
model_zip_female_v4 <- glmmTMB(total_female_presence ~ poly(temperature1, 2) + (1|date), 
                               ziformula = ~1,  # zero-inflation formula
                               family = poisson,
                               data = data)
summary(model_zip_female_v4)
anova(model_zip_female_v4, model_zip_female, test="Chi") #GHI is not important
#p=0,32, this value indicate that temperature explains number of territories occupied


######Measuring the effect of each variable#### 

#Model with both ghi and temperature
model_zip_female_v5 <- glmmTMB(total_female_presence ~ poly(temperature1, 2) + poly(GHI1, 2) + (1|date), 
                               ziformula = ~1,  # zero-inflation formula
                               family = poisson,
                               data = data)

summary(model_zip_female_v5)
#estimated coefficients for the linear and quadratic terms, along with their corresponding p-values and confidence intervals
#B linear effect = -10.59 - B quadratic effect = -24.66

# Coefficient table with confidence intervals
confint(model_zip_female_v5)

######R square of each variable####

# Function to calculate McFadden's pseudo R²
mcfadden_r2 <- function(model, null_model) {
  1 - as.numeric(logLik(model) / logLik(null_model))
}

# Null model (only intercept + random effect)
null_model <- glmmTMB(
  total_female_presence ~ 1 + (1 | date),
  ziformula = ~1,
  family = poisson,
  data = data
)

# Full model
model_zip_male <- glmmTMB(
  total_female_presence ~ poly(temperature1, 2) + poly(GHI1, 2) + (1 | date),
  ziformula = ~1,
  family = poisson,
  data = data
)

# Model with only temperature
model_temp <- glmmTMB(
  total_female_presence ~ poly(temperature1, 2) + (1 | date),
  ziformula = ~1,
  family = poisson,
  data = data
)

# Model with only GHI
model_GHI <- glmmTMB(
  total_female_presence ~ poly(GHI1, 2) + (1 | date),
  ziformula = ~1,
  family = poisson,
  data = data
)

# Calculate marginal R² for each block
r2_temp <- mcfadden_r2(model_temp, null_model)
r2_GHI  <- mcfadden_r2(model_GHI, null_model)
r2_full <- mcfadden_r2(model_zip_male, null_model)

# Output
data.frame(
  Predictor_Block = c("poly(temperature1, 2)", "poly(GHI1, 2)", "Full model"),
  McFadden_R2 = c(r2_temp, r2_GHI, r2_full)
)


######Figure of temperature#### 

#Model without temperature escalted to create the figure
# Zero-inflated Poisson without UV radiation
model_zip_female_v4 <- glmmTMB(total_female_presence ~ poly(temperature, 2) + (1|date), 
                               ziformula = ~1,  # zero-inflation formula
                               family = poisson,
                               data = data)

# Create predictions for plotting
temp_range <- seq(min(data$temperature, na.rm = TRUE), 
                  max(data$temperature, na.rm = TRUE), 
                  length.out = 40)
# Create prediction data frame
pred_data <- data.frame(
  temperature = temp_range,
  date = data$date[1]  # Use first date as reference
)

# Generate predictions (population-level, excluding random effects)
predictions <- predict(model_zip_female_v4, 
                       newdata = pred_data, 
                       type = "response",
                       re.form = NA)  # Exclude random effects for population curve

pred_data$predicted <- predictions

# Create the plot
plot3 <- ggplot() +
  # Add raw data points
  geom_point(data = data, 
             aes(x = temperature, y = total_female_presence),
             alpha = 0.4, size = 2, color = "steelblue") +
  
  # Add prediction line
  geom_line(data = pred_data, 
            aes(x = temperature, y = predicted),
            color = "blue", size = 1.2) +
  
  # Labels
  labs(x = "Temperature (°C)", 
       y = "Number of females searching for territories",
       title = "") +
  
  scale_y_continuous(breaks = seq(0, 20, 1)) +   # more y-axis values here
  scale_x_continuous(breaks = seq(15, 40, 2)) + # adjust min, max, step as needed
  
  # Custom theme without grid lines and black axes
  theme_classic() +
  theme(
    panel.grid = element_blank(),          # Remove all grid lines
    axis.line = element_line(color = "black", size = 0.5),
    axis.text = element_text(color = "black", size = 10),
    axis.title = element_text(color = "black", size = 12),
    # Move y-axis label further from axis
    axis.title.y = element_text(margin = margin(r = 15)),  # increase r for more distance
    # Move x-axis label further from axis
    axis.title.x = element_text(margin = margin(t = 15)),  # increase r for more distance
    plot.title = element_text(color = "black", size = 14, hjust = 0.5),
    axis.ticks = element_line(color = "black")
  )


# Display the plot
print(plot3)


######Figure of GHI####

# Create the plot
plot4 <- ggplot() +
  # Add raw data points
  geom_point(data = data, 
             aes(x = GHI, y = total_female_presence),
             alpha = 0.4, size = 2, color = "steelblue") +
  # Labels
  labs(x = "Global horizontal UV irradiance (Wh/m²)", 
       y = "",
       title = "") +
  
  scale_y_continuous(breaks = seq(0, 20, 1)) +   # more y-axis values here
  
  # Custom theme without grid lines and black axes
  theme_classic() +
  theme(
    panel.grid = element_blank(),          # Remove all grid lines
    axis.line = element_line(color = "black", size = 0.5),
    axis.text = element_text(color = "black", size = 10),
    axis.title = element_text(color = "black", size = 12),
    # Move y-axis label further from axis
    axis.title.y = element_text(margin = margin(r = 15)),  # increase r for more distance
    # Move x-axis label further from axis
    axis.title.x = element_text(margin = margin(t = 15)),  # increase r for more distance
    plot.title = element_text(color = "black", size = 14, hjust = 0.5),
    axis.ticks = element_line(color = "black")
  )

# Display the plot
print(plot4)

#######Saving figures############

# Combine the four plots in a 2x2 grid
combined_plot <- plot_grid(
  plot1, plot2,
  plot3, plot4,
  labels = c("a)", "b)", "c)", "d)"),
  label_size = 14
)


# Save as TIFF
ggsave("Figure 2.tiff",
       combined_plot,
       width = 10,      # largura total em polegadas
       height = 10,     # altura total em polegadas
       dpi = 300,
       compression = "lzw")

