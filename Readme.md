# PTED Semester Project

**Comparison of travel behaviour of two individuals with different tracking methods**

This repository contains the Quarto report and analysis scripts for a semester project in the course **Patterns and Trends in Environmental Data**. The project compares mobility patterns from two tracking approaches:

- **Google Timeline / Location History** data for Amelia
- **GPS Logger** data for Christopher

The analysis focuses on how different tracking methods affect the quality, processing effort and interpretation of movement trajectories.

## Project aim

The project analyses personal movement data as spatio-temporal trajectories. It compares daily mobility indicators, movement radius, time spent outside home, commuting time to ZHAW and the use of different road or transport infrastructure types.

The main research questions are:

- Which road types are most frequently used based on the recorded trajectories?
- How far is the average or maximum movement radius from home?
- How far do the individuals travel on average and at what speed?
- How long are the individuals outside their home on average?
- How long is the commuting time between home and ZHAW?
- How do Google Timeline and GPS Logger data differ in quality, accuracy and processing effort?

## Repository structure

```text
PTED-Semester-Project/
├── chapters/                  # Quarto book chapters
│   ├── abstract.qmd
│   ├── introduction.qmd
│   ├── methods.qmd
│   ├── results.qmd
│   ├── discussion.qmd
│   ├── appendix.qmd
│   └── references/            # Bibliography files
├── docs/                      # Rendered Quarto output
├── figure/                    # Cover image and report figures
├── src/                       # Analysis code
│   ├── R/                     # Reusable R functions
│   ├── scripts/               # Workflow scripts for GPS processing
│   ├── C++/                   # C++ helper code used from R
│   ├── data_processing_Amelia.R
│   ├── comparison_visuals.R
│   └── draft.R
├── _quarto.yml                # Main Quarto project configuration
├── _quarto-html.yml           # HTML rendering profile
├── _quarto-pdf.yml            # PDF rendering profile
├── index.qmd                  # Quarto book entry page
├── render.R                   # R script to render HTML and PDF outputs
├── Semesterproject.Rproj      # RStudio project file
└── README.md
```

## Data and privacy

The project uses personal location data. Raw data is therefore **not intended to be published** in the repository. Full reproduction of the analysis requires the raw and processed data files to be available locally in the expected folder structure.

Expected local data paths include, for example:

```text
data/rawData/Trackingdata_Amelia/Zeitachse_google_timeline.json
data/rawData/Trackingdata_Christopher/20260312-172538.gpx
data/processedData/
data/metaData/
```

Location data can reveal home addresses, routines and frequently visited places. Do not commit raw tracking files, exact home coordinates or sensitive intermediate data to a public repository.

## Requirements

The project is written mainly in **R** and rendered with **Quarto**. RStudio is optional but recommended because the repository includes an `.Rproj` file.

Install Quarto separately from <https://quarto.org/> and install the required R packages:

```r
install.packages(c(
  "quarto",
  "sf",
  "jsonlite",
  "dplyr",
  "purrr",
  "tidyr",
  "stringr",
  "readr",
  "tmap",
  "ggplot2",
  "lubridate",
  "osmextract",
  "osmdata",
  "leaflet",
  "data.table",
  "scales",
  "Rcpp"
))
```

Some spatial packages, especially `sf`, may require system libraries such as GDAL, GEOS and PROJ depending on your operating system.

## How to use the project

### 1. Clone the repository

```bash
git clone https://github.com/elsench1/PTED-Semester-Project.git
cd PTED-Semester-Project
```

Alternatively, download the repository as a ZIP file and open `Semesterproject.Rproj` in RStudio.

### 2. Add the local data files

Place the raw tracking data and required metadata in the expected local folders. The scripts currently use fixed relative paths such as `data/rawData/...`, `data/processedData/...` and `data/metaData/...`.

Create missing folders if needed:

```r
dir.create("data/rawData", recursive = TRUE, showWarnings = FALSE)
dir.create("data/processedData", recursive = TRUE, showWarnings = FALSE)
dir.create("data/metaData", recursive = TRUE, showWarnings = FALSE)
dir.create("chapters/plots", recursive = TRUE, showWarnings = FALSE)
```

### 3. Run the analysis scripts

The Google Timeline workflow for Amelia can be started with:

```r
source("src/data_processing_Amelia.R")
```

The GPS Logger workflow for Christopher is split into several steps:

```r
source("src/scripts/prepareChristophersData.R")
source("src/scripts/TrackMatchingChristopher.R")
source("src/scripts/create_christopher_plot_inputs.R")
```

The comparison plots can then be generated with:

```r
source("src/comparison_visuals.R")
```

Several scripts save intermediate `.rds` files to `data/processedData/` and figures to `chapters/plots/`.

### 4. Render the Quarto report

To render the default HTML report from the terminal:

```bash
quarto render
```

To render a specific profile:

```bash
quarto render --profile html
quarto render --profile pdf
```

To render both HTML and PDF using the provided R script:

```r
source("render.R")
```

The rendered output is written to the `docs/` folder.

### 5. View the report

After rendering, open the HTML output locally from the `docs/` folder, for example:

```text
docs/index.html
```

The PDF version is also generated in the rendered output folder when the PDF profile is used.

## Main analysis steps

The analysis workflow includes:

1. Importing Google Timeline JSON data and GPS Logger GPX data.
2. Cleaning timestamps, coordinates and obvious tracking errors.
3. Transforming coordinates to the Swiss coordinate system EPSG:2056.
4. Segmenting movement and stationary phases.
5. Estimating relevant anchor locations such as home and ZHAW.
6. Matching movement points to OpenStreetMap transport infrastructure.
7. Calculating mobility indicators, including:
   - daily travelled distance,
   - average travel speed,
   - maximum distance from home,
   - time spent outside home,
   - commuting time between home and ZHAW,
   - share of travel time by road or transport type.
8. Creating plots and maps for the Quarto report.
9. Comparing the results between Google Timeline and GPS Logger data.

## Important notes

- The scripts depend on local input data that is not included in the public repository.
- Some scripts use fixed file paths. If your local folder structure differs, update the paths before running the workflow.
- OpenStreetMap matching may take time and can create cached processed files.
- GPS data may contain noise, gaps and unrealistic jumps, so preprocessing is an important part of the workflow.
- Google Timeline data can be sparse and less precise, which can affect distance, speed and road type matching.

## Authors

- Amelia Heid
- Christopher Elsener

## Course context

This repository was created as a semester project for **Patterns and Trends in Environmental Data** as part of the **MSc Environment and Natural Resources** at ZHAW.

## License and reuse

This project is intended for educational use. Please contact the authors before reusing project-specific data, figures or analysis code. Personal location data should not be redistributed.
