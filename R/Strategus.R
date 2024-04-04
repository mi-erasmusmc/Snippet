library(dplyr)

strategusOutputPath <- "~/Desktop/ohdsi-dlc-download/models/"

getAllAuc <- function(strategusOutputPath) {

  combinedData <- NULL
  subdirs <- list.files(strategusOutputPath, full.names = TRUE)

  for (dir in subdirs) {
    folder <- basename(dir)
    print(basename(folder))

    allFiles <- list.files(dir, pattern = "models.csv", full.names = TRUE, recursive = TRUE)

    for(modelFilePath in allFiles) {
      # Assume the same naming pattern and location for database_details.csv and database_meta_data.csv
      directoryPath <- dirname(modelFilePath)
      databaseDetailsPath <- file.path(directoryPath, "database_details.csv")
      databaseMetaDataPath <- file.path(directoryPath, "database_meta_data.csv")

      # Read models.csv
      modelData <- read.csv(modelFilePath)
      databaseDetails <- read.csv(databaseDetailsPath)
      databaseMetaData <- read.csv(databaseMetaDataPath)

      # Merge operations
      # First, merge models data with database details
      enrichedData <- merge(modelData, databaseDetails, by = "database_id")

      # Next, merge the result with database meta data
      finalModelData <- merge(enrichedData, databaseMetaData, by.y = "database_id", by.x = "database_meta_data_id")

      # Combine with previous iterations' data
      if(is.null(combinedData)) {
        combinedData <- finalModelData
      } else {
        combinedData <- rbind(combinedData, finalModelData)
      }
    }
  }

  finalSelectedData <- combinedData %>%
    select(cdm_source_abbreviation, model_id, analysis_id, model_design_id, model_type)

}
