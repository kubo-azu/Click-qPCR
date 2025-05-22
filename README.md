# Click-qPCR
Ultra-simple tool for interactive qPCR data analysis developed by R and Shiny.

[Read this document in Japanese (日本語版はこちら)](README_jp.md)

## Overview

Click-qPCR is a user-friendly Shiny web application designed for the straightforward analysis of quantitative PCR (qPCR) data. 

**This tool is readily accessible via a web browser at [https://kubo-azu.shinyapps.io/Click-qPCR/](https://kubo-azu.shinyapps.io/Click-qPCR/), requiring no local installation for end-users.** 

It allows users to upload their Ct (threshold cycle) values, perform ΔCt and ΔΔCt calculations, visualize results as bar plots with individual data points, and download both the statistical summaries and publication-quality plots. 

For users who prefer to run or modify the application locally, the source code is also available (see Installation and Usage section below).

This tool aims to simplify common qPCR data analysis workflows, making them more accessible to researchers without requiring extensive programming knowledge.


**[Optional: Add a sentence here if this tool is associated with a publication, e.g., "This tool is described in our XXXXX (link to be added upon acceptance)."]**


## Features

* **Interactive Data Upload:** Easily upload your qPCR data in CSV format.
    * Download a template CSV file to guide data formatting.

* **Data Preview:** View the head of your uploaded data.

* **ΔCt Analysis:**
    * Select reference and target genes.
    * Choose control and treatment groups for comparison.
    * Calculates relative expression (2<sup>-ΔCt</sup>).
    * Performs Welch's t-test for statistical significance between two groups.
    * Visualizes results as a bar plot showing mean ± SD, with individual data points overlaid.

* **ΔΔCt Analysis:**
    * Uses the reference gene selected in the main ΔCt analysis.
    * Select target gene, control group, and treatment group.
    * Calculates fold-change (2<sup>-ΔΔCt</sup>) relative to the control group.
    * Performs Welch's t-test for statistical significance.
    * Visualizes results as a bar plot showing mean fold-change ± SD, with individual data points overlaid.

* **Downloadable Results:**
    * Download statistical summary tables in CSV format.
    * Download plots in PNG format.


## Installation and Usage

### Prerequisites

* R (version 4.4.2 or later recommended)

* RStudio (recommended for ease of use, but not strictly necessary if running from R console)

* The following R packages (and their dependencies):
    * `shiny`
    * `dplyr`
    * `ggplot2`
    * `tidyr`
    * `DT`
    * `RColorBrewer`

You can install these packages in R using:

```R
install.packages(c("shiny", "dplyr", "ggplot2", "tidyr", "DT", "RColorBrewer"))
```

### Running the Application

Option 1: Directly from GitHub (simplest for most users)

You can run Click-qPCR directly from GitHub within R or RStudio using the shiny::runGitHub() function:

```R
if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")
shiny::runGitHub("kubo-azu/Click-qPCR")
```


Option 2: Cloning the Repository Locally

1. Clone this repository to your local machine:

```sh
git clone [https://github.com/kubo-azu/Click-qPCR.git](https://github.com/kubo-azu/Click-qPCR.git)
```

2. Navigate to the cloned directory in R or open the Click-qPCR.Rproj file in RStudio.

3. If you are using `renv` (recommended for reproducibility), restore the R environment:

```R
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
renv::restore()
```

4. Run the application:

```R
shiny::runApp()
```

## Data Format

Prepare your data as a CSV file with the following four columns:

 - sample: Unique sample identifier (e.g., Patient_A, CellLine_1).
 - group: Experimental group or condition (e.g., Control, Treatment_X).
 - gene: Gene name (e.g., Gapdh, Actb, YourGeneOfInterest).
 - Ct: Threshold Cycle value (numeric).

Each row should represent the Ct value of one gene in one sample. If you have technical replicates for Ct values, please calculate and use their mean value for this tool. A template CSV can be downloaded from the application sidebar.

## How to Use

1. Upload Data: Click "Upload CSV File" and select your data file, or click "Use Example Data" to load a sample dataset. A preview of the uploaded data will be shown.

2. ΔCt Analysis:
 - Select your "Reference Gene" (e.g., Gapdh).
 - Select one or more "Target Gene(s)".
 - Select "Group 1" (typically your control group).
 - Select "Group 2" (typically your treatment or experimental group).
 - Click "Analyze". The plot and statistical summary table will be displayed.

3. ΔΔCt Analysis:
 - The "Reference Gene" from the main analysis section will be automatically used.
 - Select your "Target Gene" for ΔΔCt calculation.
 - Select the "Control Group" to calculate fold-change against.
 - Select the "Target Group" for which to calculate fold-change.
 - Click "Run ΔΔCt Analysis". The plot and table will be displayed.
 - Download Results: Use the download buttons to save plots (PNG) and statistical summaries (CSV).

## License

This project is licensed under the MIT License. See the LICENSE file for details.
