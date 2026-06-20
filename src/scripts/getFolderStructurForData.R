# Create data folder structure and explanation file

folders <- c(
  "data",
  file.path("data", "rawData"),
  file.path("data", "processedData"),
  file.path("data", "metaData"),
  file.path("data", "csl")
)

for (folder in folders) {
  if (!dir.exists(folder)) {
    dir.create(folder, recursive = TRUE)
    message("Created folder: ", folder)
  } else {
    message("Folder already exists: ", folder)
  }
}

# Define explanation file path
explanation_file <- file.path("data", "dataFolderStrukturExplanation.md")

# Define explanation text in English
explanation_text <- c(
  "# Data Folder Structure Explanation",
  "",
  "This folder contains the main data structure for the project.",
  "",
  "## rawData",
  "",
  "The `rawData` folder contains unprocessed raw data. For example, this can include original GPX files that have not been edited or transformed.",
  "",
  "## processedData",
  "",
  "The `processedData` folder contains processed data. These files are often intermediate versions or temporary outputs that are used later for further analysis or processing.",
  "",
  "## metaData",
  "",
  "The `metaData` folder contains metadata. For example, this can include lists of hidden coordinates or other supporting information that describes or documents the data.",
  "",
  "## csl",
  "",
  "files for citation"
)

# Create explanation file only if it does not already exist
if (!file.exists(explanation_file)) {
  writeLines(explanation_text, explanation_file)
  message("Created explanation file: ", explanation_file)
} else {
  message("Explanation file already exists: ", explanation_file)
}
