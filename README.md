# 🧬 Click-qPCR (v1.2.0) 🧬

An ultra-simple tool for interactive qPCR data analysis developed with R and Shiny.

[日本語版のユーザーガイドはこちら (Read this document in Japanese)](README_jp.md)

## Overview

Click-qPCR is a user-friendly Shiny web application designed for the straightforward analysis of real-time quantitative PCR (qPCR) data.

**This tool is readily accessible via a web browser at <https://kubo-azu.shinyapps.io/Click-qPCR/>, requiring no local installation for end-users.**

It allows users to upload their Cq (quantification cycle) values, perform ΔCq and ΔΔCq calculations, visualize results as bar plots with individual data points, and download both the statistical summaries and publication-quality plots.

For users who prefer to run or modify the application locally, the source code is also available (see Installation and Usage section below).

This tool aims to simplify common qPCR data analysis workflows, making them more accessible to researchers without requiring extensive programming knowledge.

### <ins>Notice</ins>

This repository contains the source code for the Shiny app accompanying the paper as follows:

A. Kubota and A. Tajima, *Bio-protocol*, (2025). <https://doi.org/10.21769/BioProtoc.5513>.

**Please cite this paper if you use this app or code in your research.**

## Features

-   **Interactive Data Upload:** Easily upload your qPCR data in CSV format. A template is provided to guide data formatting.

-   **Robust File Upload:** Automatically detects character encoding (e.g., UTF-8, Shift-JIS, etc.) for international compatibility.

-   **Data Privacy (Web app):** No server-side storage, temporary and in-memory processing, automatic deletion upon session end.

-   **Data Preview:** Automatically displays the first 10 rows of your selected file, allowing you to verify the data before analysis.

-   **Tab-Based Analysis:** The user interface is organized into clear tabs for different analyses.

    -   **Preproceccing and ΔCq Analysis:**
        -   Select one or **multiple reference genes**. The ΔCq is calculated using the mean Cq of the selected reference genes.
        -   Select one or more target genes.
        -   Set up **multiple group comparisons** simultaneously using an intuitive interface.
        -   Calculates relative expression ($2^{-\Delta Cq}$).
        -   Performs Welch's *t*-test for statistical significance for each specified pair.
        -   Visualizes all results in a comprehensive bar plot showing mean ± SD, with individual data points overlaid.

    -   **ΔΔCq Analysis:**
        -   Automatically uses the reference gene(s) selected in the "Preproceccing and ΔCq Analysis" tab.
        -   Select target genes, a base/control group, and one or more treatment groups.
        -   Calculates fold-change ($2^{-\Delta\Delta Cq}$) relative to the base group.
        -   Performs Welch's *t*-test for statistical significance.
        -   Visualizes results in a dedicated bar plot.

    -   **ANOVA (Dunnett's post-hoc):**
        -   Designed for comparing three or more groups.
        -   Performs a **one-way ANOVA** followed by **Dunnett's post-hoc test** to compare each treatment group against a single control group.
        -   Results are visualized as Relative Expression ($2^{-\Delta Cq}$) on the **"ΔCq ANOVA (Dunnett's post-hoc)"** tab.
        -   The same statistical results can be visualized as Fold Change ($2^{-\Delta\Delta Cq}$) on the **"ΔΔCq ANOVA (Dunnett's post-hoc)"** tab.

-   **Advanced Downloading & Plotting:**
    -   **Interactive Plots:** All plots are generated with ggplot2 and can be downloaded in PNG and PDF.
    -   **Custom Plot Dimensions:** Interactively adjust the width and height for downloaded plots using sliders. Resolution (DPI) of PNG downloading is also available to change.
    -   **Fixed Aspect Ratio:** Optionally lock the plot's aspect ratio while resizing.
    -   **Two Download Modes:**
        1.  **Download Plot:** Saves an image using your custom dimension and DPI settings.
        2.  **Save Displayed Size:** Saves an image that is an exact replica of the plot shown on the screen.
    - **Customizable Plot Colors:** Select from several built-in color palettes, including colorblind-friendly and grayscale options, to customize your plot's appearance for presentations or publications.

        | Palette Name                  | Key Features & Recommendations                                                                                                                 |
        | ----------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------- |
        | **Default** | The standard, well-recognized `ggplot2` theme.                                          |
        | **Balanced** | Provides a set of clear, distinct colors that are easy on the eyes and work well on screen.                     |
        | **Colorblind-Friendly** | Ensures that your plots are accessible to everyone, including those with color vision deficiencies. |
        | **Paired Colors** | Consists of light/dark pairs of colors. Ideal for analyses where you have paired or closely related experimental groups to compare.               |
        | **Pastel** | A selection of softer, less saturated colors. A great choice for posters or when a less intense visual style is preferred.          |
        | **Grayscale** | Renders the plot in shades of gray. Use this to confirm your figure is interpretable without color.        |
        | **No Fill** | Renders the plot with white-filled bars and black outlines. It is ideal for black-and-white publications or for post-processing in a vector graphic editor.        |

-   **Robust & Informative:**
    -   Handles cases with insufficient data or zero variance gracefully without crashing.
    -   Provides clear messages in the results table (e.g., "Zero variance") when statistics cannot be calculated.

-   **Diagnostics Tab:**
    -   This tab provides a self-testing function. When you click the "Run Diagnostics" button, the app uses its built-in sample data to automatically test its core calculation steps:
        1.  Sample Data Loading
        2.  ΔCq Calculation Validation
        3.  Statistical Test (*t*-test) Validation
        4.  Fold Change (2⁻ΔΔCq) Calculation Validation
        5.  ANOVA and Dunnett's Test Validation
    -   This ensures that each key part of the analysis pipeline—normalization, statistical comparison, and relative scaling—is functioning correctly. If all tests show "Passed ✅", you can be confident that the app's capabilities are working as intended.
    
## Installation and Usage

While the app is available online, you can also run it locally.

### Requirements

* R (version 4.1 or later recommended)
* RStudio (recommended for ease of use)
* The following R packages, which can be installed by running the command below:
    * `shiny`, `shinyjs`, `readr`, `dplyr`, `ggplot2`, `tidyr`, `DT`, `RColorBrewer`, `fontawesome`, `multcomp`

```R
install.packages(c("shiny", "shinyjs", "readr", "dplyr", "ggplot2", "tidyr", "DT", "RColorBrewer", "fontawesome", "multcomp"))
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

Prepare your data as a CSV file with the following four columns: `sample`, `group`, `gene`, and `Cq`. A template file ([Click-qPCR_template.csv](Click-qPCR_template.csv)) can be downloaded from the application sidebar.

Click-qPCR automatically handles the two most common CSV formats, so you do not need to worry about the delimiter or decimal mark when saving from Excel:

-   **Comma-separated values** with a **period** as the decimal mark (e.g., `20.01`).
-   **Semicolon-separated values** with a **comma** as the decimal mark (e.g., `20,01`).

**Note:** Each row must represent the Cq value of one gene in one sample. If you have technical replicates, please calculate and use their mean value. A template file ([Click-qPCR_template.csv](Click-qPCR_template.csv)) can be downloaded from the application sidebar.

## How to Use

1.  **Select Data to Analyze:**
    -   On the **Preprocessing and ΔCq Analysis** tab, click **"Use Example Data"** or click **"Browse..."** to select your own CSV file.
    -   The application will automatically validate the file and display a preview. If the file is valid, the "3. Analyze" button will become clickable.

2.  **Perform ΔCq Analysis:**
    -   Select your "Reference Gene(s)" and "Target Gene(s)". You can check the box to enable multiple reference genes.
    -   Under "2. Comparison Settings," define pairs of groups to compare. Click "Add" to create more pairs.
    -   Click the blue **"3. Analyze"** button. The plot and statistical table will appear.

3.  **Perform ΔΔCq Analysis:**
    -   Click on the **"ΔΔCq Analysis"** tab.
    -   The Reference Gene(s) are automatically inherited.
    -   Select "Target Gene(s)", the "Base Group (Control)", and one or more "Treatment Group(s)".
    -   Click **"Run ΔΔCq Analysis"**.

4.  **Perform ANOVA and Dunnett's post-hoc:**
    -   Navigate to the **"ΔCq ANOVA (Dunnett's post-hoc)"** tab.
    -   Select "Target Gene(s)", the "Control Group", and two or more "Treatment Group(s)".
    -   Click **"Run ANOVA"**.
    -   Navigate to the **"ΔΔCq ANOVA (Dunnett's post-hoc)"** tab to see the same results visualized as fold change.

5.  **Download Results:**
    -   In any analysis tab, use the download buttons and the "Format" selector to save your results as PNG or PDF files.
    -   Use the **"Download Plot Settings"** panel to customize the dimensions and resolution for the "Download Plot" button.

**📊 <ins>Tips: How to change the order of bar plot</ins> 📊**

The display order on the x-axis of the graph corresponds to the order of gene names in the `Target Gene(s):` window, so you can rearrange the graph by changing the input order of target genes.

It also corresponds to the order of group names in the `Treatment Group(s):` window, so you can rearrange the graph by changing the input order of treatment groups.


## Example Analysis with Sample Data

This section demonstrates how to use the app's core functions with the built-in sample data.

### 1. Load Sample Data and Perform ΔCq Analysis

First, we'll compare the expression of a single gene between two groups using a Welch's *t*-test.

* On the **"Preprocessing and ΔCq Analysis"** tab, click the **"Use Example Data"** button.
* Check the box for **"Enable multiple reference genes"**.
* For "Reference Gene(s)", select both `Gapdh` and `Actb`.
* For "Target Gene(s)", ensure only `Hoge` is selected.
* Under "Comparison Settings," set up a comparison between `Control` and `Treatment_X`.
* Click the blue **"Analyze"** button.

#### **Expected Output**

You will see a bar chart and a data table summarizing the analysis. The sample data is designed to show that `Hoge` expression is significantly lower in the `Treatment_X` group compared to the `Control` group.

**Plot:** The chart will display two bars for the `Hoge` gene: one for the `Control` group and one for the `Treatment_X` group. Individual data points will be scattered over the bars, and the bar for `Treatment_X` will be noticeably lower than the `Control` bar. A significance bracket (`***`) will connect the two bars.

**Statistics Table:** The table below the plot will show the result of the Welch's *t*-test performed on the ΔCq values. The p-value will be very small, resulting in a high significance level.

| gene | group1  | group2      | p_value  | sig |
| :--- | :------ | :---------- | :------- | :-- |
| Hoge | Control | Treatment_X | 1.25e-05 | *** |

---

### 2. Perform ANOVA with Dunnett's Post-Hoc Test

Next, we'll compare one target gene across multiple treatment groups against a single control group. The reference genes selected in the first tab (`Gapdh` and `Actb`) will be automatically used.

* Navigate to the **"ΔCq ANOVA (Dunnett's post-hoc)"** tab.
* Select `Hoge` as the "Target Gene".
* Select `Control` as the "Control Group".
* Select `Treatment_X`, `Treatment_Y`, and `Treatment_Z` in the "Treatment Group(s)" box.
* Click the blue **"Run ANOVA"** button.

#### **Expected Output**

This analysis performs a one-way ANOVA to see if there are any differences among the four groups, followed by Dunnett's test to specifically compare each treatment group to the control. The sample data will show that all treatment groups are significantly different from the control.

**Plot:** The chart will display four bars for the `Hoge` gene, one for each group (`Control`, `Treatment_X`, `Treatment_Y`, `Treatment_Z`). Significance brackets will be shown comparing each treatment group back to the `Control` bar.

**Statistics Table:** The table will first display the overall result of the ANOVA *F*-test, which should be highly significant. Below that, it will list the results of Dunnett's test for each treatment-control comparison.

| group1       | group2                               | p_value  | sig |
| :----------- | :----------------------------------- | :------- | :-- |
| ANOVA F-test | F(3, 12) = 59.39                     | 3.12e-08 |     |
| Control      | Treatment_X - Control = 0            | 2.11e-05 | *** |
| Control      | Treatment_Y - Control = 0            | 1.34e-03 | ** |
| Control      | Treatment_Z - Control = 0            | 2.00e-07 | *** |


## Licence

The Click-qPCR application and corresponding files are under MIT licence.


## Contact

Feel free to use GitHub Discussions.
