####Importing Libraries####
library(tidyverse)
library(openxlsx)
library(gtsummary)
library(lmtest)   # For regression models
library(psych)    # For descriptive statistics
####Importing dataset####
df <- read.xlsx("D:\\Shihab Study\\Shuvo vai du projects\\FInal Data file.xlsx")


#Objective 2: Analyzing Climate Change Effects on Health and Livelihoods
names(df)
#Have.you.or.any.household.members.experienced.any.health.problems.related.to.climate.change.in.the.last.3.years?.(If.yes,.please.specify)
#While.residing.in.the.slum,.have.you.been.affected.by.any.climate.change-related.events.such.as.floods,.storms,.or.sea-level.rise.in.the.last.3.years?
#Have.you.faced.any.economic.losses.or.income.reductions.due.to.the.loss.of.essential.services.(e.g.,.transportation,.electricity,.water).due.to.climate.change.in.the.last.3.years?
#Have.you.been.migrated.here.due.to.negative.effect.of.climate.change?
colnames(df)[colnames(df) == "While.residing.in.the.slum,.have.you.been.affected.by.any.climate.change-related.events.such.as.floods,.storms,.or.sea-level.rise.in.the.last.3.years?"] <- "climate_experience"

colnames(df)[colnames(df) == "Have.you.or.any.household.members.experienced.any.health.problems.related.to.climate.change.in.the.last.3.years?.(If.yes,.please.specify)"] <- "ClimateChangeImpact"

colnames(df)[colnames(df) == "Have.you.faced.any.economic.losses.or.income.reductions.due.to.the.loss.of.essential.services.(e.g.,.transportation,.electricity,.water).due.to.climate.change.in.the.last.3.years?"] <- "income_loss"
colnames(df)[colnames(df) == "Have.you.been.migrated.here.due.to.negative.effect.of.climate.change?"] <- "migration"
table_health_climate <- table(df$ClimateChangeImpact, df$climate_experience)
chisq.test(table_health_climate)
# Cross-tabulation with percentages using prop.table()
prop_table_health_climate <- prop.table(table_health_climate) * 100
print(prop_table_health_climate)  # Print percentage table




df$ClimateChangeImpact <- as.factor(df$ClimateChangeImpact)
# Logistic regression: Does climate change predict health issues?
logit_model <- glm(ClimateChangeImpact ~ climate_experience + income_loss + migration,
                   data = df, family = binomial)
summary(logit_model)

# Calculate odds ratios
odds_ratios <- exp(coef(logit_model))

# Display odds ratios
print(odds_ratios)
#Objective 3: Access to Essential Services
selected_vars <- c(
  "Have.you.faced.any.difficulties.in.accessing.food.in.the.last.3.years.due.to.climate.change?",
  "Do.you.have.access.to.financial.services.(e.g.,.banking,.microfinance)?",
  "Have.you.faced.any.difficulties.in.accessing.education.in.the.last.3.years.due.to.climate.change?",
  "Have.you.or.anyone.in.your.household.experienced.any.difficulties.in.accessing.safe.drinking.water.as.a.result.of.climate.change.in.the.last.3.years?",
  "income_loss"
)

# Loop through variables and generate frequency tables
for (var in selected_vars) {
  cat("\nFrequency Table for:", var, "\n")
  freq_table <- table(df[[var]])  # Create frequency table
  print(freq_table)  # Print counts
  
  cat("\nPercentage Table:\n")
  print(round(prop.table(freq_table) * 100, 2))  # Print percentages
  cat("\n-------------------------\n")
}


#Objective 4: Community Resilience & Adaptation
#Have.you.made.any.changes.in.your.daily.practices.to.adapt.to.the.negative.impacts.of.climate.change.(e.g.,.water.conservation,.using.alternative.energy.sources)?
#Have.you.taken.any.measures.to.prepare.for.future.climate-related.events.(e.g.,.extreme.weather.events,.sea.level.rise)?

colnames(df)[colnames(df) == "Have.you.made.any.changes.in.your.daily.practices.to.adapt.to.the.negative.impacts.of.climate.change.(e.g.,.water.conservation,.using.alternative.energy.sources)?"] <- "adaptation_for_climate_change"
colnames(df)[colnames(df) == "Have.you.taken.any.measures.to.prepare.for.future.climate-related.events.(e.g.,.extreme.weather.events,.sea.level.rise)?"] <- "adaptation_savings"

df$adaptation_for_climate_change <- ifelse(df$adaptation_for_climate_change == "Yes", 1, 0)
df$adaptation_savings <- ifelse(df$adaptation_savings == "Yes", 1, 0)

resilience_factors <- df %>% select(adaptation_for_climate_change, adaptation_savings)
kmeans_result <- kmeans(scale(resilience_factors), centers = 2)
df$resilience_cluster <- kmeans_result$cluster

table(df$`Have.you.or.anyone.in.your.household.experienced.any.difficulties.in.accessing.safe.drinking.water.as.a.result.of.climate.change.in.the.last.3.years?`)



# Extract relevant variables and create a new dataframe
resilience_df <- df %>%
  select(income_loss) %>%  # Select the variable for plotting
  mutate(resilience_cluster = kmeans_result$cluster)  # Add cluster labels from K-means

# Convert resilience_cluster to a factor (for better visualization)
resilience_df$resilience_cluster <- as.factor(resilience_df$resilience_cluster)

resilience_summary <- resilience_df %>%
  group_by(resilience_cluster, income_loss) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Percentage = round(Count / sum(Count) * 100, 1))  # Calculate percentages
# Plot the resilience strategies using ggplot
ggplot(resilience_summary, aes(x = resilience_cluster, y = Count, fill = as.factor(income_loss))) +
  geom_bar(stat = "identity", position = "dodge") +  # Use identity stat to plot precomputed counts
  geom_text(aes(label = paste0(Percentage, "%")), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 5) +  # Add percentage labels
  labs(
    title = "Resilience Clusters vs. Income Loss (Percentage View)",
    x = "Resilience Cluster",
    y = "Count",
    fill = "Income Loss"
  ) +
  theme_minimal()

#Objective 5: Identifying Priority Needs
# Define selected variables
selected_vars <- c(
  "In.your.opinion,.what.factors.make.your.community.more.vulnerable.to.the.impacts.of.climate.change?./Lack.of.access.to.essential.services.(e.g.,.healthcare,.water)",
  "In.your.opinion,.what.factors.make.your.community.more.vulnerable.to.the.impacts.of.climate.change?./Lack.of.economic.resources",
  "In.your.opinion,.what.factors.make.your.community.more.vulnerable.to.the.impacts.of.climate.change?./Limited.access.to.information.and.resources",
  "adaptation_for_climate_change",
  "adaptation_savings",
  "Have.you.received.any.information.or.training.on.how.to.prepare.for.and.respond.to.future.climate-related.events?",
  "Have.you.been.involved.in.any.community.planning.or.decision-making.processes.related.to.climate.change.adaptation.in.the.last.3.years?",
  "Have.you.noticed.any.changes.in.the.quality.or.availability.of.healthcare.services.in.your.community.as.a.result.of.climate.change.in.the.last.3.years?",
  "Have.you.participated.in.any.community-led.initiatives.to.address.the.negative.impacts.of.climate.change.in.the.last.3.years?",
  "Have.you.received.any.support.or.assistance.from.government.or.non-government.organizations.to.address.the.negative.impacts.of.climate.change.in.the.last.3.years?"
)

# Create a new dataframe with selected variables
climate_df <- df %>% select(all_of(selected_vars))
table(climate_df$`In.your.opinion,.what.factors.make.your.community.more.vulnerable.to.the.impacts.of.climate.change?./Lack.of.access.to.essential.services.(e.g.,.healthcare,.water)`)
# Function to calculate frequency and percentage
calculate_freq_percentage <- function(var) {
  freq_table <- table(climate_df[[var]])  # Count occurrences
  percent_table <- round(prop.table(freq_table) * 100, 2)  # Convert to percentages
  return(data.frame(Category = names(freq_table), Count = as.vector(freq_table), Percentage = as.vector(percent_table)))
}

# Apply the function to all selected variables
result_list <- lapply(selected_vars, calculate_freq_percentage)

# Combine all results into a single dataframe
final_freq_df <- bind_rows(result_list, .id = "Variable")











