# UFCompMetrics-Carbon-Avian

<<<<<<< Updated upstream
This is the working repository for an analysis of urban forest composition metrics used in studying urban forest management for avian or carbon outcomes.
=======
This is the repository for an analysis of urban forest composition metrics used in studying urban forest management for avian or carbon outcomes.
## Requirements

- R (developed under R 4.5.2)
- The packages loaded in [`0-packages.R`](0-packages.R): `tidyverse`, `openxlsx`, `splitstackshape`, `cowplot`, `patchwork`, and `pheatmap`. Install any that are missing, e.g.:

  ```r
  install.packages(c("tidyverse", "openxlsx", "splitstackshape",
                     "cowplot", "patchwork", "pheatmap"))
  ```
>>>>>>> Stashed changes

## Data setup

The raw data files are not included in this repository. Place the following three files together in a single data folder:

| File | Description |
|------|-------------|
| `ListofUFCompMetrics.xlsx` | Lookup of composition metrics and their categories |
| `Carbon_UFCompMetricData_21Nov24.csv` | Carbon article extractions |
| `Avian_UFCompMetricData_21Nov24.csv` | Avian article extractions |

The scripts locate this folder through the `DATA_DIR` environment variable. Set it before running, either for the session:

```r
Sys.setenv(DATA_DIR = "/path/to/data-folder")
```

or persistently by adding a line to a project-level `.Renviron` file (which is git-ignored):

```
DATA_DIR=/path/to/data-folder
```

Figures are written to the `figs/` folder, so make sure it exists.

## Running the analysis

Set `DATA_DIR`, then run the scripts. Each analysis script sources
[`1-UFcompmetrics-cleaning.R`](1-UFcompmetrics-cleaning.R) (which in turn sources
[`0-packages.R`](0-packages.R)), so the numbered scripts can be run independently
in any order — no manual sourcing of the packages or cleaning step is required.

| Script | Output |
|--------|--------|
| [`0-packages.R`](0-packages.R) | Loads required packages (sourced by the cleaning script) |
| [`1-UFcompmetrics-cleaning.R`](1-UFcompmetrics-cleaning.R) | Reads and cleans the raw data; creates the per-article metric data frames used downstream |
| [`2-PctArticlesbyMetricsbyAvianCarbon.R`](2-PctArticlesbyMetricsbyAvianCarbon.R) | Side-by-side bar charts of the percent of carbon and avian articles using each composition metric (self-contained; reads the data directly) |
| [`3-CarbonAvianOutcomes.R`](3-CarbonAvianOutcomes.R) | Carbon and avian outcome frequencies and metric-by-outcome heatmaps |
| [`4-ClusterAnalysis.R`](4-ClusterAnalysis.R) | Metric co-occurrence (Jaccard similarity) clustered heatmaps |
| [`5-Scale.R`](5-Scale.R) | Summary tables of composition metrics by urban scale |
| [`6-articlespermetric.R`](6-articlespermetric.R) | Histograms of the number of composition metrics used per article |
| [`7-VegLayerTypes.R`](7-VegLayerTypes.R) | Vegetation layer type analyses across the carbon and avian datasets |
| [`8-NoTitleFigs.R`](8-NoTitleFigs.R) | Regenerates publication-ready, title-free versions of the figures into `figs/no title figs/` (sources scripts 2–7 to rebuild the plots) |
