### Cluster entry

# Make path of R script the working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(data.table)
library(stargazer)
library(lmtest)
library(sandwich)
library(fixest)

# dataframe from 01_data_prep.ipynb
df <- fread("../outputs/data_entry_regression_2022_to_2023_clusters.csv")

# normalize
df$rel_density <- scale(df$rel_density)
df$pci <- scale(df$pci)

df$group <- factor(df$group, levels = c("Systems_Low_Level_Programming", "Machine_Learning_Embedded_Systems", "Scripting_Automation", "Specialized_Research", "Web_Application_Development"))

# On Curvilinear Effects (polinominal)
# The coefficient is negative and sig. (-0.021**).
# This indicates a curvilinear relationship between relatedness density and entry into new programming language sectors. 

# models
summary(m1 <- lm(entry01 ~ as.factor(iso2_code) + as.factor(group), data = df))
summary(m2 <- lm(entry01 ~ rel_density + as.factor(iso2_code) + as.factor(group), data = df))
summary(m3 <- lm(entry01 ~ rel_density + pci + as.factor(iso2_code) + as.factor(group), data = df))
summary(m4 <- lm(entry01 ~ rel_density + I(rel_density^2) + pci + as.factor(iso2_code) + as.factor(group), data = df))

levels(df$group)

stargazer(
  m1, m2, m3, m4,
  type = "html",
  omit = c("iso2_code", "language"),
  add.lines = list(
    c("Country FE", "Yes", "Yes", "Yes", "Yes"),
    c("Language FE", "No", "No", "Yes", "Yes")
  ),
  out = "../outputs/new_entry_2022_2023_clusters.html"
)


# Plot curvilinear effect
library(ggplot2)
library(gridExtra)
library(dplyr)

plots_list <- list()

for(group_name in levels(df$group)) {
  group_data <- df %>% 
    filter(group == group_name)
  
  model_group <- lm(entry01 ~ rel_density + I(rel_density^2), data = group_data)
  plot_data <- data.frame(
    rel_density = seq(from = min(group_data$rel_density, na.rm = TRUE), 
                      to = max(group_data$rel_density, na.rm = TRUE), 
                      length.out = 100)
  )
  plot_data$rel_density_sq <- plot_data$rel_density^2
  plot_data$predicted_entry <- predict(model_group, newdata = plot_data)
  
  plot <- ggplot(plot_data, aes(x = rel_density, y = predicted_entry)) +
    geom_line() +
    labs(title = paste(group_name),
         x = "Relatedness Density",
         y = "Predicted Likelihood of Entry") +
    theme_minimal()
  plots_list[[group_name]] <- plot
}

grid_plot <- grid.arrange(
  grobs = plots_list,
  ncol = 3, 
  nrow = 2
)

#Curvilinear Relationship Between Relatedness Density and Entry Likelihood Across Software Groups
ggsave("../outputs/curvilinear_plots.pdf", grid_plot, width = 12, height = 6)