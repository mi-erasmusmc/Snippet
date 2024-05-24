
#df <- finalSelectedData
df <- dplyr::bind_rows(finalSelectedData, finalValidationData)

library(ggplot2)
library(tidyr)
library(dplyr)

library(viridis)

df <- df %>%
  mutate(cohort_definition_id = case_when(
    cohort_definition_id %in% c(298) ~ "Lung cancer",
    cohort_definition_id %in% c(10461) ~ "Bipolar",
    cohort_definition_id %in% c(6243) ~ "Dementia",
    TRUE ~ "Other"
  )) %>%
  mutate(model_type = case_when(
    model_type %in% c("fitXgboost") ~ "Gradient boosting",
    model_type %in% c("Xgboost") ~ "Gradient boosting",
    model_type %in% c("logistic") ~ "Logistic regression",
    model_type %in% c("ResNet") ~ "ResNet",
    model_type %in% c("Transformer") ~ "Transformer",
    TRUE ~ "Other"
  )) %>%
  mutate(database_meta_data_id = case_when(
    database_meta_data_id %in% c(-1410378758) ~ "OPEHR",
    database_meta_data_id %in% c(1463865175) ~ "OPSES",
    database_meta_data_id %in% c(-795494529) ~ "AUSOM",
    database_meta_data_id %in% c(2139891622) ~ "IPCI",
    database_meta_data_id %in% c(1484143096) ~ "Stanford",
    database_meta_data_id %in% c(528172452) ~ "Columbia",
    TRUE ~ "Other"
  )) %>%
  mutate(validation_database_meta_data_id = case_when(
    validation_database_meta_data_id %in% c(-1410378758) ~ "OPEHR",
    validation_database_meta_data_id %in% c(1463865175) ~ "OPSES",
    validation_database_meta_data_id %in% c(-795494529) ~ "AUSOM",
    validation_database_meta_data_id %in% c(2139891622) ~ "IPCI",
    validation_database_meta_data_id %in% c(1484143096) ~ "Stanford",
    validation_database_meta_data_id %in% c(528172452) ~ "Columbia",
    TRUE ~ "Other"
  ))

################################################################################
# Discrimination performance - PCP - Outcome
################################################################################

# Plotting, now using `condition` for color coding and a combined group identifier
ggplot(df, aes(x = database_meta_data_id, y = value,
               group = paste(model_type, analysis_id, sep = "_"),
               color = cohort_definition_id)) +  # Color by the condition
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
ggplot(df, aes(x = database_meta_data_id, y = value, group = combined_identifier, color = model_type)) +
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
  facet_grid(cohort_definition_id ~ database_meta_data_id) +
  scale_x_continuous(limits = c(0.5, 1.0), expand = c(0, 0), labels = function(x) sprintf("%.1f", x)) +
  # scale_y_discrete(expand = c(0, 0)) +
  labs(x = "Value", y = "Model Type", title = "Grid of Plots by Database and Outcome")

################################################################################
# Discrimination performance - Heatmap
################################################################################

all_combinations <- expand.grid(
  database_meta_data_id = unique(df$database_meta_data_id),
  cohort_definition_id = unique(df$cohort_definition_id),
  model_type = unique(df$model_type),
  validation_database_meta_data_id = unique(df$validation_database_meta_data_id)
)

complete_data <- merge(all_combinations, df, by = c("database_meta_data_id", "validation_database_meta_data_id","cohort_definition_id", "model_type"), all.x = TRUE)

# Cleanup any potential NA columns if they were originally in 'df' and aren't needed:
complete_data$value <- ifelse(is.na(complete_data$value), NA, complete_data$value)

color_data <- complete_data %>%
  mutate(text_color = ifelse(value > 0.75 | value < 0.5, "black", "white"))

ggplot(complete_data, aes(x = validation_database_meta_data_id, y = database_meta_data_id, fill = value)) +
  geom_tile(na.rm = FALSE) +
  geom_text(aes(label = ifelse(is.na(value), NA, sprintf("%.2f", value))), color = color_data$text_color,
            vjust = 0.5, hjust = 0.5, size = 3, na.rm = TRUE) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  # scale_fill_gradient(low = "white", high = "blue") +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_fill_viridis(name="AUROC", limits = c(0.5, 1.0), option = "D", na.value = "#F0F0F0") +
  labs(title = NULL,
       x = "validation_database_meta_data_id database", y = "Development database", fill = "AUROC") +
  coord_fixed(ratio = 1) +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.text = element_text(hjust = 0.5),  # Centering text within the legend keys
        legend.title = element_text(hjust = 0.5),  # Horizontal justification (centered)
        legend.key.width = grid::unit(2, "cm"),  # Adjust key width
        legend.key.height = grid::unit(0.3, "cm"),  # Adjust key width
        legend.spacing.x = grid::unit(1, "cm"),  # Adjust spacing between keys
        legend.box.margin = margin(6, 6, 6, 6)) +
  guides(fill = guide_colorbar(nrow = 1, byrow = TRUE, title.position = "top", label.position = "bottom")) +
  facet_grid(cohort_definition_id ~ model_type)

