
df <- finalSelectedData

library(ggplot2)
library(tidyr)
library(dplyr)

library(viridis)

df <- df %>%
  mutate(cohort_name = case_when(
    cohort_name %in% c("Lung Cancer Prediction  O 2 - 2 primary lung cancer diagnosis") ~ "Lung cancer",
    cohort_name %in% c("[Bipolar MDD Pred] outcome") ~ "Bipolar",
    cohort_name %in% c("[HJ] Dementia (Phenotype February)") ~ "Dementia",
    TRUE ~ "Other"
  )) %>%
  mutate(model_type = case_when(
    model_type %in% c("fitXgboost") ~ "Gradient Boosting",
    model_type %in% c("logistic") ~ "Logistic Regression",
    model_type %in% c("ResNet") ~ "ResNet",
    model_type %in% c("Transformer") ~ "Transformer",
    TRUE ~ "Other"
  )) %>%
  mutate(cdm_source_abbreviation = case_when(
    cdm_source_abbreviation %in% c("cdm_optum_ehr_v2541") ~ "OPEHR",
    cdm_source_abbreviation %in% c("cdm_optum_extended_ses_v2559") ~ "OPSES",
    cdm_source_abbreviation %in% c("DeepLearningComparison_CDMPv535.dbo") ~ "AUSOM",
    cdm_source_abbreviation %in% c("DeepLearningComparison_IPCI") ~ "IPCI",
    TRUE ~ "Other"
  ))

################################################################################
# Discrimination performance - PCP - Outcome
################################################################################

# Plotting, now using `condition` for color coding and a combined group identifier
ggplot(df, aes(x = cdm_source_abbreviation, y = value,
               group = paste(model_type, analysis_id, sep = "_"),
               color = cohort_name)) +  # Color by the condition
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Discrimination performance",
       x = "Database", y = "AUROC") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_cartesian(ylim = c(0.5, 1.0)) +
  scale_y_continuous(expand = c(0, 0))# +
  # scale_color_manual(values = c("Dementia" = "blue", "Bipolar" = "red", "Lung Cancer" = "green", "Other" = "grey50"))  # Customize the colors as needed

################################################################################
# Discrimination performance - PCP - Model
################################################################################
# Add a new column to 'df' that combines 'model_type' and 'analysis_id'
df <- df %>%
  mutate(combined_identifier = paste(model_type, analysis_id, sep = "_"))

# Use this new 'combined_identifier' for the group aesthetic
ggplot(df, aes(x = cdm_source_abbreviation, y = value, group = combined_identifier, color = model_type)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Discrimination performance",
       x = "Database", y = "AUROC") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_cartesian(ylim = c(0.5, 1.0)) +
  scale_y_continuous(expand = c(0, 0))# +

################################################################################
# Discrimination performance - Grid
################################################################################

ggplot(df, aes(x = value, y = model_type)) +
  geom_point() +
  theme_minimal() +
  theme(panel.spacing.x = unit(15, "pt")) +  # Increase spacing between columns
  facet_grid(cohort_name ~ cdm_source_abbreviation) +
  scale_x_continuous(limits = c(0.5, 1.0), expand = c(0, 0), labels = function(x) sprintf("%.1f", x)) +
  # scale_y_discrete(expand = c(0, 0)) +
  labs(x = "Value", y = "Model Type", title = "Grid of Plots by Database and Outcome")

################################################################################
# Discrimination performance - Heatmap
################################################################################

all_combinations <- expand.grid(
  cdm_source_abbreviation = unique(df$cdm_source_abbreviation),
  cohort_name = unique(df$cohort_name),
  model_type = unique(df$model_type)
)

complete_data <- merge(all_combinations, df, by = c("cdm_source_abbreviation", "cohort_name", "model_type"), all.x = TRUE)

# Cleanup any potential NA columns if they were originally in 'df' and aren't needed:
complete_data$value <- ifelse(is.na(complete_data$value), NA, complete_data$value)

ggplot(complete_data, aes(x = cdm_source_abbreviation, y = cdm_source_abbreviation, fill = value)) +
  geom_tile(na.rm = FALSE) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  # scale_fill_gradient(low = "white", high = "blue") +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_fill_viridis(name="AUROC", limits = c(0.5, 1.0), option = "D") +
  labs(title = "Discrimination performance",
       x = "Database", y = "Model Type", fill = "AUROC") +
  coord_fixed(ratio = 1) +
  theme(legend.position = "right") +
  facet_grid(cohort_name ~ model_type)