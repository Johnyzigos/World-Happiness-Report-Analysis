install.packages("tidyverse")
library(tidyverse)
# Load required library for plotting
library(ggplot2)

# Load the World Happiness Report dataset
happiness_data <- read.csv("C:/Users/johny/Documents/Προγραμματισμός/MY PROJECTS DATA SCIENCE/happiness report analysis in R/happiness_report.csv", header = TRUE, sep = ",")

#Check the class of data
class(happiness_data)

# View the first few rows of the dataset
head(happiness_data, 8)

# Summary statistics of the dataset
summary(happiness_data)

# Create a bar plot
ggplot(happiness_data, aes(x = Country.or.region, y = Score)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "life expectancy by Country", x = "Country", y = "Score") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Fit a linear regression model
model <- lm(`Score` ~ `Freedom.to.make.life.choices`, data = happiness_data)

# Summary of the linear regression model
summary(model)

# Plot the linear regression line
ggplot(happiness_data, aes(x = `Freedom.to.make.life.choices`, y = `Score`)) +
  geom_point(color = "green") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Linear Regression: Happiness Score vs Freedom",
       x = "Freedom", y = "Happiness Score")

# Fit a linear regression model
model_health_score <- lm(Score ~ `Healthy.life.expectancy`, data = happiness_data)

# Summary of the linear regression model
summary(model_health_score)

# Plot the linear regression line
ggplot(happiness_data, aes(x = `Healthy.life.expectancy`, y = Score)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Linear Regression: Happiness Score vs Healthy Life Expectancy",
       x = "Healthy Life Expectancy", y = "Happiness Score")

#The world happiness!
mean(happiness_data$Score)
var(happiness_data$Score)

library(dplyr)

# Filter the dataset for the top 10 ranked countries
top_10_countries <- happiness_data %>%
  top_n(10, wt = Score)

# Create a scatter plot for the top 10 ranked countries
ggplot(top_10_countries, aes(x = `Social.support`, y = Score, color = Country.or.region)) +
  geom_point() +
  labs(title = "Scatter Plot: Social Support vs Happiness Score (Top 10 Countries)",
       x = "Social Support",
       y = "Happiness Score",
       color = "Country or Region") +
  theme_minimal()

library(corrplot)

# Calculate the correlation matrix
correlation_matrix <- cor(happiness_data[, c("Score", "Social.support", "Freedom.to.make.life.choices", "Healthy.life.expectancy", "Generosity", "Perceptions.of.corruption")])

# Visualize the correlation matrix using corrplot
corrplot(correlation_matrix, method = "circle", type = "upper", tl.pos = "lt", tl.col = "black", tl.cex = 0.8)

# Create a new variable high and low social support
happiness_data <- mutate(happiness_data, Social_Support_Group = ifelse(`Social.support` >= mean(`Social.support`, na.rm = TRUE), "High", "Low"))

# Perform a t-test to compare the average happiness scores between groups
t_test_result <- t.test(happiness_data$Score ~ happiness_data$Social_Support_Group)
t_test_result

#Create a world map
install.packages("sf")
library(sf)
world_map <- st_read("C:/Users/johny/Documents/Προγραμματισμός/MY PROJECTS DATA SCIENCE/happiness report analysis in R/ne_50m_admin_0_map_units/ne_50m_admin_0_map_units.shp")

# Merge the datasets based on the country names
merged_data <- merge(world_map, happiness_data, by.x = "ADMIN", by.y = "Country.or.region", all.x = TRUE)

str(merged_data)

# Create a world map with colored regions based on happiness scores
ggplot(merged_data) +
  geom_sf(aes(fill = Score), color = "white", size = 0.1) +
  scale_fill_viridis_c(name = "Happiness Score", option = "C", direction = -1) +  # Reverse color direction
  theme_minimal() +
  labs(title = "World Happiness Map",
       subtitle = "Colored regions based on happiness scores",
       fill = "Happiness Score") +
  theme(legend.position = "bottom")
