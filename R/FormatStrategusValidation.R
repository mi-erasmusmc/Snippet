library(dplyr)

strategusOutputPath <- "~/Git/DeepLearningComparison/output"

getAllAuc <- function(strategusOutputPath) {

  combinedData <- NULL
  subdirs <- list.files(strategusOutputPath, full.names = TRUE)

  for (dir in subdirs) {
    folder <- basename(dir)
    print(basename(folder))

    allFiles <- list.files(dir, pattern = "performances.csv", full.names = TRUE, recursive = TRUE)

    for(performancesFilePath in allFiles) {
      # Assume the same naming pattern and location for database_details.csv and database_meta_data.csv
      directoryPath <- dirname(performancesFilePath)

      evaluationStatisticPath <- file.path(directoryPath, "evaluation_statistics.csv")
      databaseDetailsPath <- file.path(directoryPath, "database_details.csv")
      cohorts <- file.path(directoryPath, "cohorts.csv")


      # databaseMetaDataPath <- file.path(directoryPath, "database_meta_data.csv")
      # evalDataPath <- file.path(directoryPath, "evaluation_statistics.csv")
      modelDesign <- file.path(directoryPath, "model_designs.csv")
      # cohorts <- file.path(directoryPath, "cohorts.csv")
      modelSettings <- file.path(directoryPath, "model_settings.csv")


      # Read models.csv
      performancesData <- read.csv(performancesFilePath)
      evaluationStatisticData <- read.csv(evaluationStatisticPath)
      evaluationStatisticData <- evaluationStatisticData[evaluationStatisticData$metric == "AUROC", ]

      databaseDetails <- read.csv(databaseDetailsPath)
      cohorts <- read.csv(cohorts)
      modelSettings <- read.csv(modelSettings)

      # databaseMetaData <- read.csv(databaseMetaDataPath)
      # evalData <- read.csv(evalDataPath)
      modelDesign <- read.csv(modelDesign)

      finalValidationData <- merge(performancesData, evaluationStatisticData, by = "performance_id")
      finalValidationData <- merge(finalValidationData, databaseDetails, by.y = "database_id", by.x = "validation_database_id")
      colnames(finalValidationData)[colnames(finalValidationData) == "database_meta_data_id"] <- "validation_database_meta_data_id"
      finalValidationData <- merge(finalValidationData, databaseDetails, by.y = "database_id", by.x = "development_database_id")
      colnames(finalValidationData)[colnames(finalValidationData) == "database_meta_data_id"] <- "database_meta_data_id"

      finalValidationData$outcome_id <- NULL
      finalValidationData$target_id <- NULL


      finalValidationData <- merge(finalValidationData, modelDesign, by = "model_design_id")
      finalValidationData <- merge(finalValidationData, modelSettings, by = "model_setting_id")
      finalValidationData <- merge(finalValidationData, cohorts, by.x = "outcome_id", by.y = "cohort_id")





      # evalData <- evalData |>
      #   dplyr::filter(metric == "AUROC", evaluation == "Test")

      # finalModelData <- merge(finalModelData, evalData, by.x = "model_id", by.y = "performance_id")

      # Combine with previous iterations' data
      if(is.null(combinedData)) {
        combinedData <- finalValidationData
      } else {
        combinedData <- rbind(combinedData, finalValidationData)
      }
    }
  }

  finalSelectedData <- combinedData %>%
    select(database_meta_data_id, validation_database_meta_data_id, model_design_id, model_type, metric, value, cohort_definition_id, cohort_name)
}

finalValidationData <- getAllAuc(strategusOutputPath)
#finalSelectedData$validation <- finalSelectedData$cdm_source_abbreviation
