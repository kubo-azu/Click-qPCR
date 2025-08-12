# Click-qPCR

Ultra-simple tool for interactive qPCR data analysis developed by R and Shiny.

[Read this document in Japanese (日本語版のユーザーガイドはこちら)](README_jp.md)

## Overview

Click-qPCR is a user-friendly Shiny web application designed for the straightforward analysis of real-time quantitative PCR (qPCR) data.

**This tool is readily accessible via a web browser at <https://kubo-azu.shinyapps.io/Click-qPCR/>, requiring no local installation for end-users.**

It allows users to upload their Cq (quantification cycle) values, perform ΔCq and ΔΔCq calculations, visualize results as bar plots with individual data points, and download both the statistical summaries and publication-quality plots.

For users who prefer to run or modify the application locally, the source code is also available (see Installation and Usage section below).

This tool aims to simplify common qPCR data analysis workflows, making them more accessible to researchers without requiring extensive programming knowledge.

### <ins>Notice</ins>

This repository contains the source code for the Shiny app accompanying the preprint:

Kubota, et al. *bioRxiv*. (2025). <https://doi.org/10.1101/2025.05.29.656779>.

**Please cite this paper if you use this app or code in your research.**

## Features

-   **Interactive Data Upload:** Easily upload your qPCR data in CSV format. A template is provided to guide data formatting.

-   **Data Preview:** View the first 10 rows of your uploaded data to ensure it's loaded correctly.

-   **Tab-Based Analysis:** The user interface is organized into clear tabs for different analyses.

    -   **Analysis Tab (ΔCq Method):**
        -   Select a reference gene and one or more target genes.
        -   Set up **multiple group comparisons** simultaneously using an intuitive interface.
        -   Calculates relative expression (2<sup>-ΔCq</sup>).
        -   Performs Welch's t-test for statistical significance for each specified pair.

-   Visualizes all results in a comprehensive bar plot showing mean ± SD, with individual data points overlaid.

-   **ΔΔCq Analysis Tab:**

    -   Automatically uses the reference gene selected in the "Analysis" tab.

-   Select a target gene, a base/control group, and one or more treatment groups.

-   Calculates fold-change (2<sup>-ΔΔCq</sup>) relative to the base group.

-   Performs Welch's t-test for statistical significance. \* Visualizes results in a dedicated bar plot.

-   **Advanced Downloading & Plotting:**

    -   **Custom Plot Dimensions:** Interactively adjust the width, height, and resolution (DPI) for downloaded plots using sliders.
    -   **Fixed Aspect Ratio:** Optionally lock the plot's aspect ratio while resizing.

-   **Two Download Modes:**

    1.  **Download Plot:** Saves an image using your custom dimension and DPI settings, perfect for publications.
    2.  **Save Displayed Size:** Saves an image that is an exact replica of the plot shown on the screen, ideal for presentations.

-   **Robust & Informative:**

    -   Handles cases with insufficient data or zero variance gracefully without crashing.

-   Provides clear messages in the results table (e.g., "Zero variance") when statistics cannot be calculated.

-   **Diagnostics Tab:**

    -   Run a series of internal checks on the sample data to verify that all core calculation and statistical functions of the app are working correctly.

## Installation and Usage

While the app is available online, you can also run it locally.

### Prerequisites

-   R (version 4.1 or later recommended)
-   The following R packages: `shiny`, `dplyr`, `ggplot2`, `tidyr`, `DT`, `RColorBrewer`, `fontawesome`

### Requirements

* R (version 4.4.2 or later recommended)
* RStudio (recommended for ease of use, but not required if running from the R console)
* The following R packages (and their dependencies):
* `shiny`
* `dplyr`
* `ggplot2`
* `tidyr`
* `DT`
* `RColorBrewer`

These packages can be installed in R as follows:

```R
install.packages(c("shiny", "dplyr", "ggplot2", "tidyr", "DT", "RColorBrewer"))
```

### Running the Application

<ins>Option 1: Run Directly from GitHub</ins>

You can run directly from GitHub using the shiny::runGitHub() function in R or RStudio:

```R
if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")
shiny::runGitHub("kubo-azu/Click-qPCR")
```

<ins>Option 2: Clone the repository locally</ins>

1. Clone this repository to your local machine (your PC):

```sh
git clone https://github.com/kubo-azu/Click-qPCR.git
```

2. Navigate to the cloned directory in R, or open the Click-qPCR.Rproj file in RStudio.

3. If you are using `renv` (recommended for reproducibility), restore your R environment:

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

-   `sample`: Unique identifier for each sample (e.g., Mouse_A, CellLine_1).
-   `group`: The experimental group or condition (e.g., Control_X, Treatment_Y).
-   `gene`: The name of the gene being measured (e.g., Gapdh, Actb).
-   `Cq`: The Quantification Cycle value (numeric). **Note:** This column must be named `Cq`.

Each row must represent the Cq value of one gene in one sample. If you have technical replicates, please calculate and use their mean value. A template can be downloaded from the application sidebar.

## How to Use

1.  **Upload Data:** On the **"Analysis"** tab, click "Upload CSV File" or "Use Example Data". A preview will appear.

2.  **Perform ΔCq Analysis (Analysis Tab):**

-   Select your "Reference Gene".
-   Select one or more "Target Gene(s)".
-   Under "Comparison Settings," define pairs of groups to compare.
-   Click "Add" to create more comparison pairs.
-   Click **"Analyze"**. The plot and statistical table will appear in the main panel.

3.  **Perform ΔΔCq Analysis (ΔΔCq Analysis Tab):**

-   Click on the **"ΔΔCq Analysis"** tab.
-   The Reference Gene is automatically inherited from the main analysis.
-   Select a single "Target Gene".
-   Select the "Base Group (Control)" that everything will be compared against.
-   Select one or more "Treatment Group(s)".
-   Click **"Run ΔΔCq Analysis"**. The fold-change plot and table will appear.

4.  **Download Results:**

-   In either tab, use the download buttons to save your results.
-   Use the **"Download Plot Settings"** panel to customize the dimensions and resolution for the "Download Plot" button, or use "Save Displayed Size" for a quick snapshot.

## Example Analysis with Sample Data

This example uses the updated sample data which includes genes designed to show up-regulation, down-regulation, and no change.

### 1. Load Sample Data and Perform Analysis

-   On the **"Analysis"** tab, click **"Use Example Data"**.
-   The default selections will be appropriate. For "Comparison Settings," we will compare `Control_X` and `Treatment_X`.
-   Click **"Analyze"**.

A plot will be generated showing bars for all selected genes and groups. The table below it will show the statistics for the specified comparison.

**Expected Table Output (ΔCq):** The table will have columns: `gene`, `group1`, `group2`, `p_value`, `sig`. For the Hoge gene compared between Control_X and Treatment_X, you should see a significant p-value and `**` in the `sig` column.

### 2. Perform ΔΔCq Analysis

-   Navigate to the **"ΔΔCq Analysis"** tab.
-   Select `Fuga` as the "Target Gene".
-   Select `Control_Y` as the "Base Group (Control)".
-   Select `Treatment_Y` as the "Treatment Group(s)".
-   Click **"Run ΔΔCq Analysis"**.

**Expected Table Output (ΔΔCq):** The plot will show the fold change for Fuga in Treatment_Y relative to Control_Y. Based on the modified sample data, this should show a fold change significantly less than 1 (down-regulation). The table will have columns: `group1`, `group2`, `p_value`, `sig`. You will see a significant p-value for this comparison.

## License

This project is licensed under the MIT License. See the LICENSE file for details.
