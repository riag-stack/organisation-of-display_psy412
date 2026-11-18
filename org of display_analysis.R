install.packages(c("readxl", "dplyr", "purrr", "writexl"))
library(readxl)
library(dplyr)
library(purrr)
library(writexl)

data_folder <- choose.dir(
  caption = "Select your org of display_data folder"
)

files <- list.files(
  data_folder,
  pattern = "\\.xlsx$",
  full.names = TRUE
)

print(basename(files))

length(files)

excel_sheets(files[1])
first_data <- read_excel(files[1])

names(first_data)
first_clean <- first_data %>%
  filter(!is.na(block)) %>%
  mutate(
    participant_id = "data01"
  ) %>%
  select(
    participant_id,
    target,
    config,
    orientation,
    reflection,
    block,
    trial_in_block,
    response.keys,
    response.corr,
    response.rt
  )

head(first_clean, 10)
dim(first_clean)

all_data <- map_dfr(files, function(file) {
  
  participant_data <- read_excel(file, sheet = 1)
  
  participant_data %>%
    filter(!is.na(block)) %>%
    mutate(
      participant_id = sub(
        "\\.xlsx$", "",
        basename(file)
      )
    ) %>%
    select(
      participant_id,
      target,
      config,
      orientation,
      reflection,
      block,
      trial_in_block,
      response.keys,
      response.corr,
      response.rt
    )
})

dim(all_data)


# Overall accuracy check

all_data %>%
  count(response.corr)

#Accuracy for each participant

participant_accuracy <- all_data %>%
  group_by(participant_id) %>%
  summarise(
    correct_trials = sum(response.corr),
    total_trials = n(),
    accuracy_percent = mean(response.corr) * 100
  )

print(participant_accuracy)

#Overall accuracy across all participants

overall_accuracy <- all_data %>%
  summarise(
    total_correct = sum(response.corr),
    total_trials = n(),
    overall_accuracy_percent = mean(response.corr) * 100
  )

print(overall_accuracy)

#Accuracy by target and configuration

accuracy_by_condition <- all_data %>%
  group_by(target, config) %>%
  summarise(
    correct_trials = sum(response.corr),
    total_trials = n(),
    accuracy_percent = mean(response.corr) * 100,
    .groups = "drop"
  )

print(accuracy_by_condition)

#Accuracy by orientation and reflection

accuracy_by_orientation <- all_data %>%
  group_by(orientation, reflection) %>%
  summarise(
    correct_trials = sum(response.corr),
    total_trials = n(),
    accuracy_percent = mean(response.corr) * 100,
    .groups = "drop"
  )

print(accuracy_by_orientation)

# Prepare reaction-time data
# Keep only correct responses

correct_rt_data <- all_data %>%
  filter(response.corr == 1)

# Check the number of correct trials

nrow(correct_rt_data)

# Mean reaction time by target and configuration
# Only correct responses are included

mean_rt_by_condition <- correct_rt_data %>%
  group_by(target, config) %>%
  summarise(
    mean_rt = mean(response.rt),
    sd_rt = sd(response.rt),
    n = n(),
    .groups = "drop"
  )

print(mean_rt_by_condition)

# Mean reaction time for each participant
# By target and configuration

participant_rt <- correct_rt_data %>%
  group_by(participant_id, target, config) %>%
  summarise(
    mean_rt = mean(response.rt),
    sd_rt = sd(response.rt),
    correct_trials = n(),
    .groups = "drop"
  )

print(participant_rt, n = Inf)

# Descriptive statistics for reaction times

correct_rt_data %>%
  summarise(
    minimum_rt = min(response.rt),
    first_quartile = quantile(response.rt, 0.25),
    median_rt = median(response.rt),
    third_quartile = quantile(response.rt, 0.75),
    maximum_rt = max(response.rt),
    mean_rt = mean(response.rt),
    sd_rt = sd(response.rt)
  )

# Final reaction time dataset for primary analysis

final_rt_data <- correct_rt_data

# Check number of trials
dim(final_rt_data)

# Calculate participant-level mean reaction times

participant_rt_means <- final_rt_data %>%
  group_by(participant_id, target, config) %>%
  summarise(
    mean_rt = mean(response.rt),
    correct_trials = n(),
    .groups = "drop"
  )

print(participant_rt_means, n = Inf)

# Overall reaction time summary by condition

condition_rt_summary <- participant_rt_means %>%
  group_by(target, config) %>%
  summarise(
    condition_mean = mean(.data$mean_rt),
    condition_sd = stats::sd(.data$mean_rt),
    participants = dplyr::n(),
    .groups = "drop"
  )

print(condition_rt_summary)

# Two-way repeated-measures ANOVA for reaction time

rt_anova <- aov(
  mean_rt ~ target * config +
    Error(participant_id / (target * config)),
  data = participant_rt_means
)

summary(rt_anova)

# Calculate participant-level accuracy by condition

participant_accuracy_means <- all_data %>%
  group_by(participant_id, target, config) %>%
  summarise(
    accuracy = mean(response.corr) * 100,
    total_trials = n(),
    .groups = "drop"
  )

print(participant_accuracy_means, n = Inf)
accuracy_summary <- participant_accuracy_means %>%
  group_by(target, config) %>%
  summarise(
    mean_accuracy = mean(accuracy),
    sd_accuracy = sd(accuracy),
    participants = n(),
    .groups = "drop"
  )

print(accuracy_summary)

# Two-way repeated-measures ANOVA for accuracy

accuracy_anova <- aov(
  accuracy ~ target * config +
    Error(participant_id / (target * config)),
  data = participant_accuracy_means
)

summary(accuracy_anova)

# Mean reaction time for incorrect responses

incorrect_rt_data <- all_data %>%
  filter(response.corr == 0)

incorrect_rt_summary <- incorrect_rt_data %>%
  summarise(
    incorrect_trials = n(),
    mean_error_rt = mean(response.rt),
    sd_error_rt = sd(response.rt),
    median_error_rt = median(response.rt)
  )

print(incorrect_rt_summary)

# Correlation between error rate and RT across conditions

condition_error_rt <- all_data %>%
  group_by(target, config, orientation, reflection) %>%
  summarise(
    error_rate = mean(response.corr == 0) * 100,
    mean_rt = mean(response.rt),
    .groups = "drop"
  )

# Pearson correlation

error_rt_correlation <- cor.test(
  condition_error_rt$error_rate,
  condition_error_rt$mean_rt,
  method = "pearson"
)

print(condition_error_rt)
print(error_rt_correlation)

# eight display-position conditions

all_data <- all_data %>%
  mutate(
    target_position = interaction(
      orientation,
      reflection,
      sep = "_"
    )
  )


all_data %>%
  group_by(target_position) %>%
  summarise(
    total_trials = n(),
    accuracy = mean(response.corr) * 100,
    error_rate = mean(response.corr == 0) * 100,
    .groups = "drop"
  )

# Mean RT for each participant in each position

position_rt_means <- all_data %>%
  filter(response.corr == 1) %>%
  group_by(participant_id, target_position) %>%
  summarise(
    mean_rt = mean(response.rt),
    .groups = "drop"
  )

# Repeated-measures ANOVA

position_rt_anova <- aov(
  mean_rt ~ target_position +
    Error(participant_id / target_position),
  data = position_rt_means
)

summary(position_rt_anova)

# Mean RT for position and configuration

position_config_rt <- all_data %>%
  filter(response.corr == 1) %>%
  group_by(
    participant_id,
    target_position,
    config
  ) %>%
  summarise(
    mean_rt = mean(response.rt),
    .groups = "drop"
  )

# Repeated-measures ANOVA

position_config_anova <- aov(
  mean_rt ~ target_position * config +
    Error(participant_id / (target_position * config)),
  data = position_config_rt
)

summary(position_config_anova)

# Effect sizes (Partial Eta Squared)

effect_sizes <- data.frame(
  Analysis = c(
    "Accuracy: Target",
    "Accuracy: Configuration",
    "Accuracy: Target × Configuration",
    "Reaction Time: Target",
    "Reaction Time: Configuration",
    "Reaction Time: Target × Configuration",
    "Orientation-Reflection: RT",
    "Orientation-Reflection × Configuration: Position",
    "Orientation-Reflection × Configuration: Configuration",
    "Orientation-Reflection × Configuration: Interaction"
  ),
  F_value = c(
    0.633, 17.71, 3.483,
    0.374, 0.011, 0.003,
    0.689,
    1.026, 0.013, 0.956
  ),
  df_effect = c(
    1, 1, 1,
    1, 1, 1,
    7,
    7, 1, 7
  ),
  df_error = c(
    19, 19, 19,
    19, 19, 19,
    133,
    133, 19, 133
  )
)

effect_sizes$partial_eta_squared <- with(
  effect_sizes,
  (F_value * df_effect) /
    ((F_value * df_effect) + df_error)
)

print(effect_sizes) 

# Accuracy by Target and Stimulus Configuration

accuracy_matrix <- xtabs(
  accuracy_percent ~ target + config,
  data = accuracy_by_condition
)

barplot(
  accuracy_matrix,
  beside = TRUE,
  col = c("lightblue", "lightpink"),
  main = "Mean Accuracy by Stimulus Configuration",
  xlab = "Stimulus Configuration",
  ylab = "Accuracy (%)",
  ylim = c(0, 100),
  names.arg = c("Inline", "Outline"),
  border = "black"
)

legend(
  "topleft",
  legend = c("F Target", "T Target"),
  fill = c("lightblue", "lightpink"),
  title = "Target Identity",
  bty = "n"
)

# Reaction Time by Target Identity

rt_graph_data <- participant_rt_means %>%
  group_by(target) %>%
  summarise(
    mean_rt = mean(mean_rt) * 1000,
    .groups = "drop"
  )

rt_positions <- barplot(
  rt_graph_data$mean_rt,
  names.arg = c("F Target", "T Target"),
  col = c("lightblue", "lightpink"),
  main = "Mean Correct Reaction Time by Target Identity",
  xlab = "Target Letter",
  ylab = "Mean Correct Reaction Time (ms)",
  ylim = c(0, max(rt_graph_data$mean_rt) * 1.2),
  border = "black",
  las = 1
)

