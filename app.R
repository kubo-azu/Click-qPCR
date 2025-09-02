# 1. Load Libraries
library(shiny)
library(shinyjs)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(DT)
library(RColorBrewer)
library(fontawesome)
library(multcomp)

# 2. Define Sample Data
sample_csv_text <- "sample,group,gene,Cq
Mouse_1,Control,Gapdh,20.01
Mouse_1,Control,Actb,20.21
Mouse_1,Control,Hoge,25.12
Mouse_1,Control,Fuga,27.34
Mouse_2,Control,Gapdh,20.55
Mouse_2,Control,Actb,20.25
Mouse_2,Control,Hoge,25.89
Mouse_2,Control,Fuga,28.02
Mouse_3,Control,Gapdh,20.18
Mouse_3,Control,Actb,20.28
Mouse_3,Control,Hoge,25.21
Mouse_3,Control,Fuga,27.45
Mouse_4,Control,Gapdh,20.33
Mouse_4,Control,Actb,20.13
Mouse_4,Control,Hoge,25.56
Mouse_4,Control,Fuga,27.88
Mouse_5,Treatment_X,Gapdh,19.98
Mouse_5,Treatment_X,Actb,20.28
Mouse_5,Treatment_X,Hoge,26.45
Mouse_5,Treatment_X,Fuga,28.91
Mouse_6,Treatment_X,Gapdh,20.21
Mouse_6,Treatment_X,Actb,20.31
Mouse_6,Treatment_X,Hoge,27.32
Mouse_6,Treatment_X,Fuga,29.99
Mouse_7,Treatment_X,Gapdh,20.55
Mouse_7,Treatment_X,Actb,20.15
Mouse_7,Treatment_X,Hoge,26.68
Mouse_7,Treatment_X,Fuga,29.14
Mouse_8,Treatment_X,Gapdh,20.32
Mouse_8,Treatment_X,Actb,20.22
Mouse_8,Treatment_X,Hoge,27.05
Mouse_8,Treatment_X,Fuga,29.70
Mouse_9,Treatment_Y,Gapdh,20.67
Mouse_9,Treatment_Y,Actb,20.42
Mouse_9,Treatment_Y,Hoge,24.11
Mouse_9,Treatment_Y,Fuga,26.33
Mouse_10,Treatment_Y,Gapdh,20.15
Mouse_10,Treatment_Y,Actb,20.30
Mouse_10,Treatment_Y,Hoge,23.88
Mouse_10,Treatment_Y,Fuga,25.99
Mouse_11,Treatment_Y,Gapdh,20.33
Mouse_11,Treatment_Y,Actb,20.23
Mouse_11,Treatment_Y,Hoge,24.25
Mouse_11,Treatment_Y,Fuga,26.56
Mouse_12,Treatment_Y,Gapdh,20.81
Mouse_12,Treatment_Y,Actb,20.86
Mouse_12,Treatment_Y,Hoge,24.50
Mouse_12,Treatment_Y,Fuga,26.90
Mouse_13,Treatment_Z,Gapdh,20.18
Mouse_13,Treatment_Z,Actb,20.38
Mouse_13,Treatment_Z,Hoge,27.59
Mouse_13,Treatment_Z,Fuga,29.92
Mouse_14,Treatment_Z,Gapdh,20.72
Mouse_14,Treatment_Z,Actb,20.42
Mouse_14,Treatment_Z,Hoge,28.45
Mouse_14,Treatment_Z,Fuga,30.86
Mouse_15,Treatment_Z,Gapdh,19.95
Mouse_15,Treatment_Z,Actb,20.05
Mouse_15,Treatment_Z,Hoge,27.89
Mouse_15,Treatment_Z,Fuga,30.14
Mouse_16,Treatment_Z,Gapdh,20.54
Mouse_16,Treatment_Z,Actb,20.39
Mouse_16,Treatment_Z,Hoge,28.63
Mouse_16,Treatment_Z,Fuga,30.91"

sample_df_global <- read.csv(text = sample_csv_text, stringsAsFactors = FALSE, check.names = FALSE)


# 3. Define UI (User Interface)
ui <- fluidPage(title = "Click-qPCR: Ultra-Simple Tool for Interactive qPCR Data Analysis",
                useShinyjs(),
                tags$head(
                  tags$script(HTML("setInterval(function(){var d=new Date();var pad=n=>n<10?'0'+n:n;var datetime=d.getFullYear()+'-'+pad(d.getMonth()+1)+'-'+pad(d.getDate())+'_'+pad(d.getHours())+pad(d.getMinutes());Shiny.setInputValue('client_time',datetime);},1000);")),
                  tags$style(HTML("
                    label { font-size: 18px !important; font-weight: normal !important; } 
                    .btn{margin-top: 5px; margin-bottom: 5px;}
                    .btn-container {
                      display: flex;
                      flex-wrap: wrap;
                      align-items: flex-end;
                      gap: 8px;
                    }
                    .btn-container .shiny-input-container {
                      margin-bottom: 0px !important;
                    }
                  ")),
                  tags$script(async = NA, src = "https://www.googletagmanager.com/gtag/js?id=G-7J5FG35PN3"),
                  tags$script(HTML(
                    "
    window.dataLayer = window.dataLayer || [];
    function gtag(){dataLayer.push(arguments);}
    gtag('js', new Date());

    gtag('config', 'G-7J5FG35PN3');
    "
                  ))
                ),
                br(),
                div(
                  style = "display: flex; align-items: center; margin-bottom: 10px;",
                  img(src = "Click-qPCR_logo.png", height = "128px", style = "margin-right: 12px;"),
                  div(
                    div(align = "left", style = "font-size: 65px; font-weight: bold; color: #2c3e50; font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;",
                        HTML("Click-qPCR")
                    )
                  )
                ),
                div(align = "left", style = "margin-bottom: 15px; font-size: 16px; color: #555;",
                    HTML("<strong>Notifications:</strong><br># Calculate the mean of Cq values by yourself, if you have technical replicates.<br># Please cite this paper if you use this app in your research. <u>A. Kubota and A. Tajima, <i>bioRxiv</i>, 2025. (<a href='https://doi.org/10.1101/2025.05.29.656779' target='_blank'>https://doi.org/10.1101/2025.05.29.656779</a>).</u>")),
                div(style = "text-align: left; font-size: 15px; color: #777;", "🧬 App Version: v1.1.0 🧬"),
                div(align = "left", style = "margin-bottom: 3px;",
                    tags$a(href = "https://github.com/kubo-azu/Click-qPCR", target = "_blank", icon("github"), "View User Guide (English and Japanese) & Source Code on GitHub")),
                br(),
                tabsetPanel(
                  id = "main_tabs",
                  tabPanel("Preprocessing and ΔCq Analysis",
                           sidebarLayout(
                             sidebarPanel(width = 4,
                                          p(HTML("Prepare your data as a CSV file with the following four columns:<br><strong>sample</strong>, <strong>group</strong>, <strong>gene</strong>, and <strong>Cq</strong> (quantification cycle value).")),
                                          hr(),
                                          actionButton("load_example", "Use Example Data", icon=icon("lightbulb")),
                                          downloadButton("download_template", "Template CSV"),
                                          actionButton("reset_app", "Reset All", icon=icon("sync-alt"), class = "btn-danger"),
                                          br(), br(),
                                          h4("1. Select CSV File to Analyze", style = "font-weight: bold;"),
                                          fileInput("file", NULL),
                                          div(style = "max-height: 350px; overflow-y: auto;", DT::dataTableOutput("data_preview")),
                                          br(),
                                          checkboxInput("enable_multi_ref", "Enable multiple reference genes", value = FALSE),
                                          uiOutput("refgene_selector"),
                                          uiOutput("gene_selector"),
                                          h4("2. Comparison Settings", style = "font-weight: bold;"),
                                          uiOutput("group_pairs_ui"),
                                          actionButton("add_comparison", "Add", icon=icon("plus")),
                                          actionButton("remove_comparison", "Remove", icon=icon("minus")),
                                          br(),br(),
                                          shinyjs::disabled(
                                            actionButton("analyze", "3. Analyze", class="btn-primary btn-lg", icon=icon("play-circle"))
                                          ),
                                          br(),
                                          hr(),
                                          h4("Plot and Data Download"),
                                          selectInput("color_palette_dcq", "Color Palette:",
                                                      choices = c("Default (ggplot2)" = "Default",
                                                                  "Balanced (Set2)" = "Set2",
                                                                  "Colorblind-Friendly (Viridis)" = "Viridis",
                                                                  "Paired Colors" = "Paired",
                                                                  "Pastel (Pastel1)" = "Pastel1",
                                                                  "Grayscale (for printing)" = "Grayscale")),
                                          div(class = "btn-container",
                                              downloadButton("download_csv", "Download Stats", class="btn-success"),
                                              downloadButton("download_plot", "Download Plot", class="btn-success"),
                                              downloadButton("download_plot_asis", "Save Displayed Size", class="btn-info"),
                                              selectInput("plot_format_dcq", "Format:", choices = c("PNG" = "png", "PDF" = "pdf"), selected = "png", width = "120px")
                                          ),
                                          hr(),
                                          h4("Download Plot Settings"),
                                          p("Settings for the 'Download Plot' button."),
                                          fluidRow(
                                            column(6, sliderInput("plot_width", "Width (inches):", min = 4, max = 20, value = 10)),
                                            column(6, sliderInput("plot_height", "Height (inches):", min = 4, max = 20, value = 8))
                                          ),
                                          checkboxInput("fix_ratio", "Fix the ratio", value = FALSE),
                                          sliderInput("plot_dpi", "Resolution (DPI):", min = 72, max = 600, value = 300),
                                          hr(),
                                          h4("Appendix: Statistical Method"),
                                          p(HTML("<div style='font-size: 14px; color: #555; line-height: 1.5;'>
                                    <strong>1. ΔCq Calculation:</strong><br>
                                    ΔCq = Cq(Target Gene) - Mean Cq(Reference Genes)
                                    <br><br>
                                    <strong>2. Statistical Test:</strong><br>
                                    A <strong>Welch's two-sample <i>t</i>-test</strong> is performed on the <strong>ΔCq values</strong> between each treatment group and the control group to calculate the <i>p</i>-value.
                                    <br><br>
                                    <strong>3. Visualization:</strong><br>
                                    The plot displays Relative Expression (2<sup>-ΔCq</sup>) for intuitive visualization.
                                    </div>"))
                             ),
                             mainPanel(width = 8,
                                       tags$div(style = "width: 100%; overflow-x: auto;",
                                                uiOutput("resultPlotUI")
                                       ),
                                       br(),
                                       DT::dataTableOutput("summary_table")
                             )
                           )
                  ),
                  tabPanel("ΔΔCq Analysis",
                           sidebarLayout(
                             sidebarPanel(width = 4,
                                          h5(strong("Reference Gene(s):")),
                                          textOutput("ddCq_refgene_display"),
                                          br(),
                                          uiOutput("ddCq_target_selector"),
                                          hr(),
                                          h4("ΔΔCq Comparison Settings"),
                                          uiOutput("ddCq_base_group_selector"),
                                          uiOutput("ddCq_comparison_groups_ui"),
                                          actionButton("add_ddCq_comparison", "Add", icon=icon("plus")),
                                          actionButton("remove_ddCq_comparison", "Remove", icon=icon("minus")),
                                          actionButton("ddCq_analyze", "Run ΔΔCq Analysis", class="btn-primary btn-lg", icon=icon("play-circle")),
                                          br(), br(),
                                          hr(),
                                          selectInput("color_palette_ddcq", "Color Palette:",
                                                      choices = c("Default (ggplot2)" = "Default",
                                                                  "Balanced (Set2)" = "Set2",
                                                                  "Colorblind-Friendly (Viridis)" = "Viridis",
                                                                  "Paired Colors" = "Paired",
                                                                  "Pastel (Pastel1)" = "Pastel1",
                                                                  "Grayscale (for printing)" = "Grayscale")),
                                          div(class = "btn-container",
                                              downloadButton("ddCq_csv", "Download ΔΔCq Stats", class="btn-success"),
                                              downloadButton("ddCq_plot", "Download ΔΔCq Plot", class="btn-success"),
                                              downloadButton("ddCq_plot_asis", "Save Displayed Size", class="btn-info"),
                                              selectInput("plot_format_ddcq", "Format:", choices = c("PNG" = "png", "PDF" = "pdf"), selected = "png", width = "120px")
                                          ),
                                          hr(),
                                          h4("Download Plot Settings"),
                                          p("Settings for the 'Download ΔΔCq Plot' button."),
                                          fluidRow(
                                            column(6, sliderInput("plot_width_ddCq", "Width (inches):", min = 4, max = 20, value = 10)),
                                            column(6, sliderInput("plot_height_ddCq", "Height (inches):", min = 4, max = 20, value = 8))
                                          ),
                                          checkboxInput("fix_ratio_ddCq", "Fix the ratio", value = FALSE),
                                          sliderInput("plot_dpi_ddCq", "Resolution (DPI):", min = 72, max = 600, value = 300),
                                          hr(),
                                          h4("Appendix: Statistical Method"),
                                          p(HTML("<div style='font-size: 14px; color: #555; line-height: 1.5;'>
                                    <strong>1. ΔΔCq Calculation:</strong><br>
                                    ΔΔCq = ΔCq(Sample) - Mean ΔCq(Control Group)<br>
                                    (where ΔCq = Cq_target - Mean Cq_refs)
                                    <br><br>
                                    <strong>2. Statistical Test:</strong><br>
                                    A <strong>Welch's two-sample <i>t</i>-test</strong> is performed on the <strong>ΔCq values</strong> between each treatment group and the control group to calculate the <i>p</i>-value.
                                    <br><br>
                                    <strong>3. Visualization:</strong><br>
                                    The plot displays Relative Expression (2<sup>-ΔΔCq</sup>) for intuitive visualization.
                                    </div>"))
                             ),
                             mainPanel(width = 8,
                                       tags$div(style = "width: 100%; overflow-x: auto;",
                                                uiOutput("ddCqPlotUI")
                                       ),
                                       br(),
                                       DT::dataTableOutput("ddCq_table_output")
                             )
                           )
                  ),
                  tabPanel("ΔCq ANOVA (Dunnett’s post-hoc)",
                           sidebarLayout(
                             sidebarPanel(width = 4,
                                          p("This tab is for comparing 3 or more groups. It performs an ANOVA followed by Dunnett's test to compare each treatment group against a single control group."),
                                          hr(),
                                          h5(strong("Reference Gene(s):")),
                                          textOutput("anova_refgene_display"),
                                          br(),
                                          uiOutput("anova_target_selector"),
                                          hr(),
                                          h4("ANOVA (3+ Groups) Comparison Settings"),
                                          uiOutput("anova_control_group_selector"),
                                          uiOutput("anova_treatment_group_selector"),
                                          actionButton("anova_analyze", "Run ANOVA", class="btn-primary btn-lg", icon=icon("play-circle")),
                                          br(),br(),
                                          hr(),
                                          selectInput("color_palette_anova", "Color Palette:",
                                                      choices = c("Default (ggplot2)" = "Default",
                                                                  "Balanced (Set2)" = "Set2",
                                                                  "Colorblind-Friendly (Viridis)" = "Viridis",
                                                                  "Paired Colors" = "Paired",
                                                                  "Pastel (Pastel1)" = "Pastel1",
                                                                  "Grayscale (for printing)" = "Grayscale")),
                                          div(class = "btn-container",
                                              downloadButton("anova_csv", "Download ANOVA Stats", class="btn-success"),
                                              downloadButton("anova_plot", "Download ANOVA Plot", class="btn-success"),
                                              downloadButton("anova_plot_asis", "Save Displayed Size", class="btn-info"),
                                              selectInput("plot_format_anova", "Format:", choices = c("PNG" = "png", "PDF" = "pdf"), selected = "png", width = "120px")
                                          ),
                                          hr(),
                                          h4("Download Plot Settings"),
                                          p("Settings for the 'Download ANOVA Plot' button."),
                                          fluidRow(
                                            column(6, sliderInput("plot_width_anova", "Width (inches):", min = 4, max = 20, value = 10)),
                                            column(6, sliderInput("plot_height_anova", "Height (inches):", min = 4, max = 20, value = 8))
                                          ),
                                          checkboxInput("fix_ratio_anova", "Fix the ratio", value = FALSE),
                                          sliderInput("plot_dpi_anova", "Resolution (DPI):", min = 72, max = 600, value = 300),
                                          hr(),
                                          h4("Appendix: Statistical Method"),
                                          p(HTML("<div style='font-size: 14px; color: #555; line-height: 1.5;'>
                                    <strong>1. ΔCq Calculation:</strong><br>
                                    ΔCq = Cq(Target Gene) - Mean Cq(Reference Genes)
                                    <br><br>
                                    <strong>2. Statistical Test:</strong><br>
                                    First, a <strong>one-way ANOVA</strong> is performed on the ΔCq values to determine if there are any significant differences among the means of all selected groups. Then, <strong>Dunnett's test</strong> is used as a post-hoc test to specifically compare each treatment group against the single control group.
                                    <br><br>
                                    <strong>3. Visualization:</strong><br>
                                    The plot displays Relative Expression (2<sup>-ΔCq</sup>) for intuitive visualization.
                                    </div>"))
                             ),
                             mainPanel(width = 8,
                                       tags$div(style = "width: 100%; overflow-x: auto;",
                                                uiOutput("anovaPlotUI")
                                       ),
                                       br(),
                                       DT::dataTableOutput("anova_table_output")
                             )
                           )
                  ),
                  tabPanel("ΔΔCq ANOVA (Dunnett’s post-hoc)",
                           sidebarLayout(
                             sidebarPanel(width = 4,
                                          p(HTML("This tab visualizes the ANOVA results as Fold Change (2<sup>-ΔΔCq</sup>). The statistical test is the same as the 'ANOVA Analysis' tab.")),
                                          hr(),
                                          h5(strong("Reference Gene(s):")),
                                          textOutput("anova_ddCq_refgene_display"),
                                          br(),
                                          uiOutput("anova_ddCq_target_selector"),
                                          hr(),
                                          h4("ANOVA (ΔΔCq) Comparison Settings"),
                                          uiOutput("anova_ddCq_control_group_selector"),
                                          uiOutput("anova_ddCq_treatment_group_selector"),
                                          actionButton("anova_ddCq_analyze", "Run ANOVA (ΔΔCq)", class="btn-primary btn-lg", icon=icon("play-circle")),
                                          br(),br(),
                                          hr(),
                                          selectInput("color_palette_anova_ddcq", "Color Palette:",
                                                      choices = c("Default (ggplot2)" = "Default",
                                                                  "Balanced (Set2)" = "Set2",
                                                                  "Colorblind-Friendly (Viridis)" = "Viridis",
                                                                  "Paired Colors" = "Paired",
                                                                  "Pastel (Pastel1)" = "Pastel1",
                                                                  "Grayscale (for printing)" = "Grayscale")),
                                          div(class = "btn-container",
                                              downloadButton("anova_ddCq_csv", "Download Stats", class="btn-success"),
                                              downloadButton("anova_ddCq_plot", "Download Plot", class="btn-success"),
                                              downloadButton("anova_ddCq_plot_asis", "Save Displayed Size", class="btn-info"),
                                              selectInput("plot_format_anova_ddcq", "Format:", choices = c("PNG" = "png", "PDF" = "pdf"), selected = "png", width = "120px")
                                          ),
                                          hr(),
                                          h4("Download Plot Settings"),
                                          p("Settings for the 'Download Plot' button."),
                                          fluidRow(
                                            column(6, sliderInput("plot_width_anova_ddCq", "Width (inches):", min = 4, max = 20, value = 10)),
                                            column(6, sliderInput("plot_height_anova_ddCq", "Height (inches):", min = 4, max = 20, value = 8))
                                          ),
                                          checkboxInput("fix_ratio_anova_ddCq", "Fix the ratio", value = FALSE),
                                          sliderInput("plot_dpi_anova_ddCq", "Resolution (DPI):", min = 72, max = 600, value = 300),
                                          hr(),
                                          h4("Appendix: Statistical Method"),
                                          p(HTML("<div style='font-size: 14px; color: #555; line-height: 1.5;'>
                                    <strong>1. ΔΔCq Calculation:</strong><br>
                                    ΔΔCq = ΔCq(Sample) - Mean ΔCq(Control Group)<br>
                                    (where ΔCq = Cq_target - Mean Cq_refs)
                                    <br><br>
                                    <strong>2. Statistical Test:</strong><br>
                                    The statistical test is identical to the 'ANOVA Analysis' tab (ANOVA followed by Dunnett's test on <strong>ΔCq values</strong>).
                                    <br><br>
                                    <strong>3. Visualization:</strong><br>
                                    The plot displays Fold Change (2<sup>-ΔΔCq</sup>) to visualize expression relative to the control group.
                                    </div>"))
                             ),
                             mainPanel(width = 8,
                                       tags$div(style = "width: 100%; overflow-x: auto;",
                                                uiOutput("anovaDdCqPlotUI")
                                       ),
                                       br(),
                                       DT::dataTableOutput("anova_ddCq_table_output")
                             )
                           )
                  ),
                  tabPanel("Diagnostics",
                           br(),
                           h3("App Functionality Diagnostics"),
                           p("Use this panel to verify that the app's core functions are working correctly with a sample dataset."),
                           actionButton("run_diagnostics", "Run Diagnostics", icon = icon("stethoscope")),
                           hr(),
                           uiOutput("diagnostics_output")
                  )
                )
)


# 4. Define Server (Server Logic)
server <- function(input, output, session) {
  
  # --- Reactive Values and State Management ---
  raw_data <- reactiveVal()
  preview_data <- reactiveVal()
  analysis_results <- reactiveVal()
  ddCq_analysis_results <- reactiveVal()
  anova_results <- reactiveVal()
  anova_ddCq_results <- reactiveVal()
  
  plot_dims <- reactiveVal(list(width = 10, height = 8))
  ddCq_plot_dims <- reactiveVal(list(width = 10, height = 8))
  anova_plot_dims <- reactiveVal(list(width = 10, height = 8))
  anova_ddCq_plot_dims <- reactiveVal(list(width = 10, height = 8))
  
  comparison_count <- reactiveVal(1)
  ddCq_comparison_count <- reactiveVal(1)
  
  `%||%` <- function(a, b) { if (!is.null(a)) a else b }
  
  # --- Data Loading and Reset ---
  observeEvent(input$load_example, {
    df <- read.csv(text = sample_csv_text, stringsAsFactors = FALSE)
    
    preview_data(df)
    raw_data(df)
    analysis_results(NULL)
    ddCq_analysis_results(NULL)
    anova_results(NULL)
    anova_ddCq_results(NULL)
    
    showNotification("Sample data loaded.", type = "message", duration = 3)
    shinyjs::enable("analyze")
  })
  
  observeEvent(input$file, {
    req(input$file)
    
    # First, try reading as a standard comma-separated CSV
    df <- tryCatch({
      readr::read_csv(input$file$datapath, col_names = TRUE, show_col_types = FALSE)
    }, error = function(e) { NULL })
    
    # If the standard read fails or results in a single column, try as a semicolon-separated CSV
    if (is.null(df) || ncol(df) <= 1) {
      showNotification("Could not parse as comma-separated. Retrying as semicolon-separated...", type = "default", duration = 2)
      df <- tryCatch({
        readr::read_csv2(input$file$datapath, col_names = TRUE, show_col_types = FALSE)
      }, error = function(e) { NULL })
    }
    
    # Validate the resulting data frame
    if (!is.null(df) && all(c("sample", "group", "gene", "Cq") %in% names(df))) {
      if (!is.numeric(df$Cq)) {
        df$Cq <- suppressWarnings(as.numeric(as.character(df$Cq)))
        if(any(is.na(df$Cq))) {
          showNotification("Warning: Non-numeric Cq values were found and will be ignored during calculations.", type="warning", duration=10)
        }
      }
      
      preview_data(df)
      raw_data(df)
      
      showNotification("File successfully loaded and validated.", type = "message", duration = 4)
      shinyjs::enable("analyze")
      
    } else {
      # If all attempts fail or columns are missing, reset and show error
      error_message <- if (is.null(df)) {
        "Error: Could not parse the file. Please ensure it is a valid CSV."
      } else {
        "Error: File is missing one or more required columns: 'sample', 'group', 'gene', 'Cq'."
      }
      
      showNotification(error_message, type = "error", duration = 15)
      
      preview_data(NULL)
      raw_data(NULL)
      analysis_results(NULL)
      shinyjs::disable("analyze")
    }
  })
  
  observeEvent(input$reset_app, {
    session$reload()
  })
  
  # --- UI Rendering ---
  output$data_preview <- DT::renderDataTable({
    req(preview_data())
    DT::datatable(
      head(preview_data(), 10),
      options = list(
        scrollX = TRUE,
        paging = FALSE,
        searching = FALSE,
        info = FALSE,
        ordering = FALSE
      ),
      rownames = FALSE
    )
  })
  
  output$refgene_selector <- renderUI({
    req(raw_data())
    is_multiple <- isTRUE(input$enable_multi_ref)
    
    selectInput("refgene", "Reference Gene(s):", 
                choices = unique(raw_data()$gene), 
                selected = input$refgene %||% "Gapdh", 
                multiple = is_multiple)
  })
  
  output$gene_selector <- renderUI({ 
    req(raw_data(), input$refgene)
    selectInput("goi", "Target Gene(s):", 
                choices = setdiff(unique(raw_data()$gene), input$refgene), 
                multiple = TRUE, 
                selected=setdiff(unique(raw_data()$gene), c("Gapdh", "Actb"))[1]) 
  })
  
  output$group_pairs_ui <- renderUI({
    req(raw_data()); groups <- unique(raw_data()$group)
    lapply(1:comparison_count(), function(i) {
      fluidRow(
        column(6, selectInput(paste0("group1_comp", i), if(i==1) "Group 1" else NULL, choices = groups, selected = input[[paste0("group1_comp", i)]] %||% groups[1])),
        column(6, selectInput(paste0("group2_comp", i), if(i==1) "Group 2" else NULL, choices = groups, selected = input[[paste0("group2_comp", i)]] %||% if(length(groups)>1) groups[2] else groups[1] ))
      )
    })
  })
  observeEvent(input$add_comparison, { comparison_count(comparison_count() + 1) })
  observeEvent(input$remove_comparison, { if (comparison_count() > 1) comparison_count(comparison_count() - 1) })
  
  output$ddCq_refgene_display <- renderText({ req(input$refgene); paste(input$refgene, collapse = ", ") })
  output$anova_refgene_display <- renderText({ req(input$refgene); paste(input$refgene, collapse = ", ") })
  output$anova_ddCq_refgene_display <- renderText({ req(input$refgene); paste(input$refgene, collapse = ", ") })
  
  output$ddCq_target_selector <- renderUI({ req(raw_data(), input$refgene); selectInput("ddCq_target", "Target Gene:", choices = setdiff(unique(raw_data()$gene), input$refgene)) })
  output$ddCq_base_group_selector <- renderUI({ req(raw_data()); selectInput("ddCq_base_group", "Base Group (Control):", choices = unique(raw_data()$group)) })
  output$ddCq_comparison_groups_ui <- renderUI({
    req(raw_data(), input$ddCq_base_group)
    other_groups <- setdiff(unique(raw_data()$group), input$ddCq_base_group)
    lapply(1:ddCq_comparison_count(), function(i) {
      selectInput(paste0("ddCq_comp_group", i), if(i==1) "Treatment Group(s)" else NULL, choices = other_groups, selected = input[[paste0("ddCq_comp_group", i)]] %||% if(length(other_groups) >= i) other_groups[i] else NULL)
    })
  })
  observeEvent(input$add_ddCq_comparison, { ddCq_comparison_count(ddCq_comparison_count() + 1) })
  observeEvent(input$remove_ddCq_comparison, { if (ddCq_comparison_count() > 1) ddCq_comparison_count(ddCq_comparison_count() - 1) })
  
  output$anova_target_selector <- renderUI({
    req(raw_data(), input$refgene)
    selectInput("anova_target", "Target Gene:", choices = setdiff(unique(raw_data()$gene), input$refgene))
  })
  output$anova_control_group_selector <- renderUI({
    req(raw_data())
    selectInput("anova_control", "Control Group:", choices = unique(raw_data()$group))
  })
  output$anova_treatment_group_selector <- renderUI({
    req(raw_data(), input$anova_control)
    choices <- setdiff(unique(raw_data()$group), input$anova_control)
    selectInput("anova_treatments", "Treatment Group(s):", choices = choices, multiple = TRUE, selected = choices)
  })
  
  output$anova_ddCq_target_selector <- renderUI({
    req(raw_data(), input$refgene)
    selectInput("anova_ddCq_target", "Target Gene:", choices = setdiff(unique(raw_data()$gene), input$refgene))
  })
  output$anova_ddCq_control_group_selector <- renderUI({
    req(raw_data())
    selectInput("anova_ddCq_control", "Control Group:", choices = unique(raw_data()$group))
  })
  output$anova_ddCq_treatment_group_selector <- renderUI({
    req(raw_data(), input$anova_ddCq_control)
    choices <- setdiff(unique(raw_data()$group), input$anova_ddCq_control)
    selectInput("anova_ddCq_treatments", "Treatment Group(s):", choices = choices, multiple = TRUE, selected = choices)
  })
  
  
  # --- Analysis Logic ---
  observeEvent(input$analyze, {
    req(raw_data(), input$refgene, input$goi)
    
    comparison_pairs <- lapply(1:comparison_count(), function(i) unique(c(input[[paste0("group1_comp", i)]], input[[paste0("group2_comp", i)]])))
    comparison_pairs <- comparison_pairs[sapply(comparison_pairs, length) == 2]; req(length(comparison_pairs) > 0)
    
    all_selected_groups <- unique(unlist(comparison_pairs))
    
    mean_ref_cq <- raw_data() %>%
      filter(gene %in% input$refgene) %>%
      group_by(sample) %>%
      summarise(mean_ref_cq = mean(Cq, na.rm = TRUE), .groups = "drop")
    
    long_data <- raw_data() %>%
      filter(gene %in% input$goi, group %in% all_selected_groups) %>%
      inner_join(mean_ref_cq, by = "sample") %>%
      mutate(deltaCq_val = Cq - mean_ref_cq) %>%
      filter(!is.na(deltaCq_val)) %>%
      mutate(RelExp = 2^(-deltaCq_val))
    req(nrow(long_data) > 0)
    
    long_data$gene <- factor(long_data$gene, levels = input$goi)
    
    summary_data <- long_data %>% group_by(gene, group) %>%
      summarise(Mean=mean(RelExp, na.rm=TRUE), SD=sd(RelExp, na.rm=TRUE), N=n(), .groups="drop")
    
    significance_raw <- bind_rows(lapply(input$goi, function(g) {
      bind_rows(lapply(comparison_pairs, function(p) {
        dt <- long_data %>% filter(gene == g, group %in% p)
        p_val_result <- if (n_distinct(dt$group) == 2 && all(table(dt$group) >= 2)) {
          tryCatch({
            t.test(deltaCq_val ~ group, data = dt, na.action = na.omit)$p.value
          }, error = function(e) {
            if (grepl("essentially constant", e$message)) "Zero variance" else "Calc. Error"
          })
        } else {
          NA
        }
        data.frame(gene = g, group1 = p[1], group2 = p[2], p_value_raw = p_val_result, stringsAsFactors = FALSE)
      }))
    }))
    
    significance <- significance_raw %>%
      mutate(
        p_value_num = suppressWarnings(as.numeric(p_value_raw)),
        p_value = case_when(
          is.na(p_value_num) & !is.na(p_value_raw) ~ as.character(p_value_raw),
          !is.na(p_value_num) ~ sprintf("%.2e", p_value_num),
          TRUE ~ "NA"
        ),
        sig = case_when(
          is.na(p_value_num) ~ "ns",
          p_value_num < 0.001 ~ "***",
          p_value_num < 0.01 ~ "**",
          p_value_num < 0.05 ~ "*",
          TRUE ~ "ns"
        )
      ) %>%
      dplyr::select(gene, group1, group2, p_value, sig)
    
    summary_data_plot <- summary_data %>% mutate(label = factor(paste(gene, group), levels = unique(paste(gene, group))))
    data_levels <- levels(summary_data_plot$label)
    
    max_vals <- summary_data_plot %>% group_by(label) %>% summarise(y_pos = Mean + ifelse(is.na(SD), 0, SD))
    
    bracket_data_list <- list()
    if(nrow(significance) > 0 && nrow(max_vals) > 0) {
      significance$pos_start <- sapply(paste(significance$gene, significance$group1), function(l) which(data_levels == l))
      significance <- significance %>% arrange(pos_start)
      
      data_y_max <- max(max_vals$y_pos, na.rm = TRUE)
      space_per_bracket <- data_y_max * 0.12
      y_current_level <- data_y_max
      
      for (i in seq_len(nrow(significance))) {
        row <- significance[i,]
        label1 <- paste(row$gene, row$group1)
        label2 <- paste(row$gene, row$group2)
        if (!all(c(label1, label2) %in% data_levels)) next
        pos1 <- which(data_levels == label1)
        pos2 <- which(data_levels == label2)
        
        y_current_level <- y_current_level + space_per_bracket
        
        bracket_data_list[[length(bracket_data_list) + 1]] <- data.frame(
          x=min(pos1, pos2), xend=max(pos1, pos2), y=y_current_level, y_label = y_current_level, label=row$sig
        )
      }
    }
    
    if (length(bracket_data_list) > 0) {
      bracket_data <- bind_rows(bracket_data_list)
    } else {
      bracket_data <- data.frame(x=numeric(), xend=numeric(), y=numeric(), y_label=numeric(), label=character())
    }
    
    analysis_results(list(summary_table = significance %>% dplyr::select(-any_of("pos_start")),
                          raw_data_for_dl = summary_data,
                          bracket_data = bracket_data,
                          long_data = long_data,
                          summary_data_plot = summary_data_plot))
  })
  
  deltaCq_plot_obj <- reactive({
    req(analysis_results())
    results <- analysis_results()
    
    plot_max_y <- if (nrow(results$bracket_data) > 0) max(results$bracket_data$y_label, na.rm=TRUE) * 1.15 else max(results$summary_data_plot$Mean + results$summary_data_plot$SD, na.rm=TRUE) * 1.2
    if(!is.finite(plot_max_y) || plot_max_y <= 0) plot_max_y <- 1
    
    p <- ggplot(results$summary_data_plot, aes(x=label, y=Mean, fill=group)) +
      geom_bar(stat="identity", position=position_dodge(0.7), width=0.7, color="black") +
      geom_jitter(data = results$long_data, aes(x = paste(gene, group), y = RelExp), position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.7), shape = 21, color = "black", size = 2, alpha = 0.6, show.legend = FALSE) +
      geom_errorbar(aes(ymin=pmax(0,Mean-SD), ymax=Mean+SD), width=0.2, position=position_dodge(0.7), color="black") +
      geom_segment(data=results$bracket_data, aes(x=x, xend=xend, y=y, yend=y), inherit.aes=FALSE, color="black") +
      geom_segment(data=results$bracket_data, aes(x=x, xend=x, y=y, yend=y - 0.005 * plot_max_y), inherit.aes=FALSE, color="black") +
      geom_segment(data=results$bracket_data, aes(x=xend, xend=xend, y=y, yend=y - 0.005 * plot_max_y), inherit.aes=FALSE, color="black") +
      geom_text(data=results$bracket_data, aes(x=(x+xend)/2, y=y_label, label=label), inherit.aes=FALSE, vjust=-0.4, size=6) +
      coord_cartesian(ylim=c(0, plot_max_y), clip="off") +
      labs(x="Gene (Group)", y=expression("Relative Expression" ~ (2^{-Delta*Cq})), fill="Group") +
      theme_classic(base_size=16) +
      theme(
        axis.text.x=element_text(angle=45, hjust=1),
        plot.margin = margin(t=40, r=20, b=10, l=10)
      )
    
    selected_palette <- input$color_palette_dcq
    if (is.null(selected_palette)) { return(p) }
    
    if (selected_palette == "Viridis") {
      p <- p + scale_fill_viridis_d()
    } else if (selected_palette == "Grayscale") {
      p <- p + scale_fill_grey()
    } else if (selected_palette != "Default") {
      p <- p + scale_fill_brewer(palette = selected_palette)
    }
    
    return(p)
  })
  
  observeEvent(input$ddCq_analyze, {
    req(raw_data(), input$refgene, input$ddCq_target, input$ddCq_base_group)
    
    ddCq_comparison_groups <- lapply(1:ddCq_comparison_count(), function(i) input[[paste0("ddCq_comp_group", i)]]) %>% unlist() %>% unique()
    
    if(is.null(ddCq_comparison_groups) || length(ddCq_comparison_groups) == 0) {
      showNotification("Please select at least one treatment group for ΔΔCq analysis.", type = "warning")
      return(NULL)
    }
    
    all_selected_groups_ddCq <- c(input$ddCq_base_group, ddCq_comparison_groups)
    
    mean_ref_cq <- raw_data() %>%
      filter(gene %in% input$refgene) %>%
      group_by(sample) %>%
      summarise(mean_ref_cq = mean(Cq, na.rm = TRUE), .groups = "drop")
    
    deltaCq_data <- raw_data() %>%
      filter(gene == input$ddCq_target, group %in% all_selected_groups_ddCq) %>%
      inner_join(mean_ref_cq, by = "sample") %>%
      mutate(deltaCq = Cq - mean_ref_cq) %>%
      filter(!is.na(deltaCq))
    
    req(nrow(deltaCq_data) > 0, input$ddCq_base_group %in% deltaCq_data$group)
    
    mean_control_deltaCq <- mean(deltaCq_data$deltaCq[deltaCq_data$group == input$ddCq_base_group], na.rm = TRUE)
    ddCq_data <- deltaCq_data %>%
      mutate(deltaDeltaCq = deltaCq - mean_control_deltaCq, FoldChange = 2^(-deltaDeltaCq))
    
    summary_data <- ddCq_data %>% group_by(group) %>%
      summarise(Mean = mean(FoldChange), SD = sd(FoldChange), N=n(), .groups="drop")
    
    significance_raw <- bind_rows(lapply(ddCq_comparison_groups, function(g) {
      test_data <- filter(deltaCq_data, group %in% c(input$ddCq_base_group, g))
      p_val_result <- if (n_distinct(test_data$group) == 2 && all(table(test_data$group) >= 2)) {
        tryCatch({
          t.test(deltaCq ~ group, data = test_data, na.action = na.omit)$p.value
        }, error = function(e) {
          if (grepl("essentially constant", e$message)) "Zero variance" else "Calc. Error"
        })
      } else {
        NA
      }
      data.frame(group1 = input$ddCq_base_group, group2 = g, p_value_raw = p_val_result, stringsAsFactors = FALSE)
    }))
    
    significance <- significance_raw %>%
      mutate(
        p_value_num = suppressWarnings(as.numeric(p_value_raw)),
        p_value = case_when(
          is.na(p_value_num) & !is.na(p_value_raw) ~ as.character(p_value_raw),
          !is.na(p_value_num) ~ sprintf("%.2e", p_value_num),
          TRUE ~ "NA"
        ),
        sig = case_when(
          is.na(p_value_num) ~ "ns",
          p_value_num < 0.001 ~ "***",
          p_value_num < 0.01 ~ "**",
          p_value_num < 0.05 ~ "*",
          TRUE ~ "ns"
        )
      ) %>%
      dplyr::select(group1, group2, p_value, sig)
    
    summary_data_plot <- summary_data %>% mutate(group = factor(group, levels = all_selected_groups_ddCq))
    data_levels <- levels(summary_data_plot$group)
    max_vals <- summary_data_plot %>% group_by(group) %>% summarise(y_pos = Mean + ifelse(is.na(SD), 0, SD))
    
    bracket_data_list <- list()
    if(nrow(significance) > 0 && nrow(max_vals) > 0){
      data_y_max <- max(max_vals$y_pos, na.rm = TRUE)
      space_per_bracket <- data_y_max * 0.18
      y_current_level <- data_y_max
      
      for(i in seq_len(nrow(significance))) {
        row <- significance[i,]
        pos1 <- which(data_levels == row$group1)
        pos2 <- which(data_levels == row$group2)
        if(length(pos1)==0 || length(pos2)==0) next
        
        y_current_level <- y_current_level + space_per_bracket
        
        bracket_data_list[[i]] <- data.frame(x=min(pos1, pos2), xend=max(pos1, pos2), y=y_current_level, y_label=y_current_level, label=row$sig)
      }
    }
    
    if (length(bracket_data_list) > 0) {
      bracket_data <- bind_rows(bracket_data_list)
    } else {
      bracket_data <- data.frame(x=numeric(), xend=numeric(), y=numeric(), y_label=numeric(), label=character())
    }
    
    ddCq_analysis_results(list(summary_table = significance, raw_data_for_dl = summary_data, bracket_data=bracket_data, summary_data_plot=summary_data_plot, ddCq_data=ddCq_data))
  })
  
  ddCq_plot_obj <- reactive({
    req(ddCq_analysis_results())
    results <- ddCq_analysis_results()
    
    plot_max_y <- if (nrow(results$bracket_data) > 0) max(results$bracket_data$y_label, na.rm=TRUE) * 1.15 else max(results$summary_data_plot$Mean + results$summary_data_plot$SD, na.rm=TRUE) * 1.2
    if(!is.finite(plot_max_y) || plot_max_y <= 0) plot_max_y <- 2
    
    p <- ggplot(results$summary_data_plot, aes(x = group, y = Mean, fill = group)) +
      geom_bar(stat="identity", color="black", width=0.7) +
      geom_errorbar(aes(ymin = pmax(0, Mean - SD), ymax = Mean + SD), width = 0.2, color="black") +
      geom_jitter(data = results$ddCq_data, aes(y = FoldChange), width = 0.1, shape=21, size=2.5, alpha=0.7, fill="grey80", show.legend = FALSE) +
      geom_hline(yintercept=1, linetype="dashed", color="red", linewidth=1) +
      geom_segment(data=results$bracket_data, aes(x=x, xend=xend, y=y, yend=y), inherit.aes=FALSE, color="black") +
      geom_segment(data=results$bracket_data, aes(x=x, xend=x, y=y, yend=y - 0.01 * plot_max_y), inherit.aes=FALSE, color="black") +
      geom_segment(data=results$bracket_data, aes(x=xend, xend=xend, y=y, yend=y - 0.01 * plot_max_y), inherit.aes=FALSE, color="black") +
      geom_text(data=results$bracket_data, aes(x=(x+xend)/2, y=y_label, label=label), inherit.aes=FALSE, vjust=-0.4, size=6) +
      coord_cartesian(ylim=c(0, plot_max_y), clip="off") +
      labs(y=expression("Fold Change (2"^{-ΔΔCq}*")"), x="Group", title=paste("Gene Expression of", input$ddCq_target)) +
      theme_classic(base_size = 14) +
      theme(axis.text.x=element_text(size=12, color="black", angle=45, hjust=1),
            axis.text.y=element_text(color="black"),
            axis.title=element_text(size=14,face="bold", color="black"),
            plot.title=element_text(size=16,face="bold",hjust=0.5, color="black"),
            legend.title=element_text(size=12,face="bold", color="black"),
            legend.text=element_text(size=10, color="black"),
            plot.margin = margin(t=30, r=10, b=10, l=10))
    
    selected_palette <- input$color_palette_ddcq
    if (is.null(selected_palette)) { return(p) }
    
    if (selected_palette == "Viridis") {
      p <- p + scale_fill_viridis_d()
    } else if (selected_palette == "Grayscale") {
      p <- p + scale_fill_grey()
    } else if (selected_palette != "Default") {
      p <- p + scale_fill_brewer(palette = selected_palette)
    }
    
    return(p)
  })
  
  observeEvent(input$anova_analyze, {
    req(raw_data(), input$refgene, input$anova_target, input$anova_control, input$anova_treatments)
    
    selected_groups <- c(input$anova_control, input$anova_treatments)
    if(length(selected_groups) < 2) {
      showNotification("Please select at least one treatment group for ANOVA.", type = "warning")
      return()
    }
    
    mean_ref_cq <- raw_data() %>%
      filter(gene %in% input$refgene) %>%
      group_by(sample) %>%
      summarise(mean_ref_cq = mean(Cq, na.rm = TRUE), .groups = "drop")
    
    deltaCq_data <- raw_data() %>%
      filter(gene == input$anova_target, group %in% selected_groups) %>%
      inner_join(mean_ref_cq, by = "sample") %>%
      mutate(deltaCq = Cq - mean_ref_cq,
             RelExp = 2^(-deltaCq)) %>%
      filter(!is.na(deltaCq))
    
    deltaCq_data$group <- factor(deltaCq_data$group, levels = selected_groups)
    
    tryCatch({
      aov_model <- aov(deltaCq ~ group, data = deltaCq_data)
      aov_summary <- summary(aov_model)[[1]]
      
      f_val <- aov_summary[["F value"]][1]
      df1 <- aov_summary[["Df"]][1]
      df2 <- aov_summary[["Df"]][2]
      aov_pval <- aov_summary[["Pr(>F)"]][1]
      f_test_text <- paste0("ANOVA: F(", df1, ", ", df2, ") = ", round(f_val, 2), ", p = ", sprintf("%.2e", aov_pval))
      
      linfct_anova <- mcp(group = "Dunnett")
      dunnett_results <- summary(glht(aov_model, linfct = linfct_anova))
      dunnett_pvals <- dunnett_results$test$pvalues
      
      summary_data <- deltaCq_data %>%
        group_by(group) %>%
        summarise(Mean = mean(RelExp), SD = sd(RelExp), N = n(), .groups = "drop")
      
      significance_table_detailed <- data.frame(
        group1 = input$anova_control,
        group2 = input$anova_treatments,
        p_value_raw = unname(dunnett_pvals)
      ) %>%
        mutate(
          p_value_num = suppressWarnings(as.numeric(p_value_raw)),
          sig = case_when(
            is.na(p_value_num) ~ "ns",
            p_value_num < 0.001 ~ "***",
            p_value_num < 0.01 ~ "**",
            p_value_num < 0.05 ~ "*",
            TRUE ~ "ns"
          )
        )
      
      significance_table_display <- significance_table_detailed %>%
        mutate(p_value = sprintf("%.2e", p_value_num)) %>%
        dplyr::select(group1, group2, p_value, sig)
      
      anova_summary_table <- bind_rows(
        data.frame(
          group1 = "ANOVA F-test",
          group2 = paste0("F(", df1, ", ", df2, ") = ", round(f_val, 2)),
          p_value = sprintf("%.2e", aov_pval),
          sig = ""
        ),
        significance_table_display
      )
      
      max_vals <- summary_data %>% group_by(group) %>% summarise(y_pos = Mean + ifelse(is.na(SD), 0, SD))
      data_levels <- levels(summary_data$group)
      bracket_data_list <- list()
      if(nrow(significance_table_detailed) > 0 && nrow(max_vals) > 0){
        data_y_max <- max(max_vals$y_pos, na.rm = TRUE)
        space_per_bracket <- data_y_max * 0.15
        y_current_level <- data_y_max
        
        for(i in seq_len(nrow(significance_table_detailed))) {
          row <- significance_table_detailed[i,]
          pos1 <- which(data_levels == row$group1)
          pos2 <- which(data_levels == row$group2)
          if(length(pos1)==0 || length(pos2)==0) next
          
          y_current_level <- y_current_level + space_per_bracket
          
          bracket_data_list[[i]] <- data.frame(x=min(pos1, pos2), xend=max(pos1, pos2), y=y_current_level, y_label=y_current_level, label=row$sig)
        }
      }
      
      bracket_data <- if (length(bracket_data_list) > 0) bind_rows(bracket_data_list) else data.frame(x=numeric(), xend=numeric(), y=numeric(), y_label=numeric(), label=character())
      
      anova_results(list(summary_table = anova_summary_table,
                         summary_data_plot = summary_data,
                         bracket_data = bracket_data,
                         long_data = deltaCq_data,
                         f_test_text = f_test_text))
    }, error = function(e) {
      showNotification(paste("An error occurred during ANOVA calculation:", e$message), type = "error")
      anova_results(NULL)
    })
  })
  
  anova_plot_obj <- reactive({
    req(anova_results())
    results <- anova_results()
    
    plot_max_y <- if (nrow(results$bracket_data) > 0) max(results$bracket_data$y_label, na.rm=TRUE) * 1.15 else max(results$summary_data_plot$Mean + results$summary_data_plot$SD, na.rm=TRUE) * 1.2
    if(!is.finite(plot_max_y) || plot_max_y <= 0) plot_max_y <- 1
    
    p <- ggplot(results$summary_data_plot, aes(x = group, y = Mean, fill = group)) +
      geom_bar(stat="identity", color="black", width=0.7) +
      geom_errorbar(aes(ymin = pmax(0, Mean - SD), ymax = Mean + SD), width = 0.2, color="black") +
      geom_jitter(data = results$long_data, aes(y = RelExp), width = 0.1, shape=21, size=2.5, alpha=0.7, fill="grey80") +
      geom_segment(data=results$bracket_data, aes(x=x, xend=xend, y=y, yend=y), inherit.aes=FALSE, color="black") +
      geom_segment(data=results$bracket_data, aes(x=x, xend=x, y=y, yend=y - 0.005 * plot_max_y), inherit.aes=FALSE, color="black") +
      geom_segment(data=results$bracket_data, aes(x=xend, xend=xend, y=y, yend=y - 0.005 * plot_max_y), inherit.aes=FALSE, color="black") +
      geom_text(data=results$bracket_data, aes(x=(x+xend)/2, y=y_label, label=label), inherit.aes=FALSE, vjust=-0.4, size=6) +
      coord_cartesian(ylim=c(0, plot_max_y), clip="off") +
      labs(x="Group", y=expression("Relative Expression" ~ (2^{-Delta*Cq})), fill="Group", 
           title = paste("Gene Expression of", input$anova_target),
           subtitle = results$f_test_text) +
      theme_classic(base_size=16) +
      theme(
        axis.text.x=element_text(angle=45, hjust=1, color="black"),
        axis.text.y=element_text(color="black"),
        axis.title=element_text(size=14,face="bold", color="black"),
        plot.title=element_text(size=16,face="bold",hjust=0.5, color="black"),
        plot.subtitle = element_text(size = 12, hjust = 0.5, color = "black"),
        legend.title=element_text(size=12,face="bold", color="black"),
        legend.text=element_text(size=10, color="black"),
        plot.margin = margin(t=40, r=20, b=10, l=10)
      )
    
    selected_palette <- input$color_palette_anova
    if (is.null(selected_palette)) { return(p) }
    
    if (selected_palette == "Viridis") {
      p <- p + scale_fill_viridis_d()
    } else if (selected_palette == "Grayscale") {
      p <- p + scale_fill_grey()
    } else if (selected_palette != "Default") {
      p <- p + scale_fill_brewer(palette = selected_palette)
    }
    
    return(p)
  })
  
  observeEvent(input$anova_ddCq_analyze, {
    req(raw_data(), input$refgene, input$anova_ddCq_target, input$anova_ddCq_control, input$anova_ddCq_treatments)
    
    selected_groups <- c(input$anova_ddCq_control, input$anova_ddCq_treatments)
    if(length(selected_groups) < 2) {
      showNotification("Please select at least one treatment group.", type = "warning")
      return()
    }
    
    mean_ref_cq <- raw_data() %>%
      filter(gene %in% input$refgene) %>%
      group_by(sample) %>%
      summarise(mean_ref_cq = mean(Cq, na.rm = TRUE), .groups = "drop")
    
    deltaCq_data <- raw_data() %>%
      filter(gene == input$anova_ddCq_target, group %in% selected_groups) %>%
      inner_join(mean_ref_cq, by = "sample") %>%
      mutate(deltaCq = Cq - mean_ref_cq) %>%
      filter(!is.na(deltaCq))
    
    deltaCq_data$group <- factor(deltaCq_data$group, levels = selected_groups)
    
    tryCatch({
      aov_model <- aov(deltaCq ~ group, data = deltaCq_data)
      aov_summary <- summary(aov_model)[[1]]
      f_val <- aov_summary[["F value"]][1]
      df1 <- aov_summary[["Df"]][1]
      df2 <- aov_summary[["Df"]][2]
      aov_pval <- aov_summary[["Pr(>F)"]][1]
      f_test_text <- paste0("ANOVA: F(", df1, ", ", df2, ") = ", round(f_val, 2), ", p = ", sprintf("%.2e", aov_pval))
      
      linfct_anova <- mcp(group = "Dunnett")
      dunnett_results <- summary(glht(aov_model, linfct = linfct_anova))
      dunnett_pvals <- dunnett_results$test$pvalues
      
      mean_control_deltaCq <- mean(deltaCq_data$deltaCq[deltaCq_data$group == input$anova_ddCq_control], na.rm = TRUE)
      ddCq_data <- deltaCq_data %>%
        mutate(deltaDeltaCq = deltaCq - mean_control_deltaCq, 
               FoldChange = 2^(-deltaDeltaCq))
      
      summary_data <- ddCq_data %>%
        group_by(group) %>%
        summarise(Mean = mean(FoldChange), SD = sd(FoldChange), N = n(), .groups = "drop")
      
      significance_table_detailed <- data.frame(
        group1 = input$anova_ddCq_control,
        group2 = input$anova_ddCq_treatments,
        p_value_raw = unname(dunnett_pvals)
      ) %>%
        mutate(p_value_num = suppressWarnings(as.numeric(p_value_raw)),
               sig = case_when(is.na(p_value_num) ~ "ns", p_value_num < 0.001 ~ "***", p_value_num < 0.01 ~ "**", p_value_num < 0.05 ~ "*", TRUE ~ "ns"))
      
      significance_table_display <- significance_table_detailed %>%
        mutate(p_value = sprintf("%.2e", p_value_num)) %>%
        dplyr::select(group1, group2, p_value, sig)
      
      anova_summary_table <- bind_rows(
        data.frame(group1 = "ANOVA F-test", group2 = paste0("F(", df1, ", ", df2, ") = ", round(f_val, 2)), p_value = sprintf("%.2e", aov_pval), sig = ""),
        significance_table_display
      )
      
      max_vals <- summary_data %>% group_by(group) %>% summarise(y_pos = Mean + ifelse(is.na(SD), 0, SD))
      data_levels <- levels(summary_data$group)
      bracket_data_list <- list()
      if(nrow(significance_table_detailed) > 0 && nrow(max_vals) > 0){
        data_y_max <- max(max_vals$y_pos, na.rm = TRUE)
        space_per_bracket <- data_y_max * 0.15
        y_current_level <- data_y_max
        for(i in seq_len(nrow(significance_table_detailed))) {
          row <- significance_table_detailed[i,]
          pos1 <- which(data_levels == row$group1)
          pos2 <- which(data_levels == row$group2)
          if(length(pos1)==0 || length(pos2)==0) next
          y_current_level <- y_current_level + space_per_bracket
          bracket_data_list[[i]] <- data.frame(x=min(pos1, pos2), xend=max(pos1, pos2), y=y_current_level, y_label=y_current_level, label=row$sig)
        }
      }
      bracket_data <- if (length(bracket_data_list) > 0) bind_rows(bracket_data_list) else data.frame(x=numeric(), xend=numeric(), y=numeric(), y_label=numeric(), label=character())
      
      anova_ddCq_results(list(summary_table = anova_summary_table,
                              summary_data_plot = summary_data,
                              bracket_data = bracket_data,
                              long_data = ddCq_data,
                              f_test_text = f_test_text))
    }, error = function(e) {
      showNotification(paste("An error occurred during ANOVA (ΔΔCq) calculation:", e$message), type = "error")
      anova_ddCq_results(NULL)
    })
  })
  
  anova_ddCq_plot_obj <- reactive({
    req(anova_ddCq_results())
    results <- anova_ddCq_results()
    
    plot_max_y <- if (nrow(results$bracket_data) > 0) max(results$bracket_data$y_label, na.rm=TRUE) * 1.15 else max(results$summary_data_plot$Mean + results$summary_data_plot$SD, na.rm=TRUE) * 1.2
    if(!is.finite(plot_max_y) || plot_max_y <= 0) plot_max_y <- 2
    
    p <- ggplot(results$summary_data_plot, aes(x = group, y = Mean, fill = group)) +
      geom_bar(stat="identity", color="black", width=0.7) +
      geom_errorbar(aes(ymin = pmax(0, Mean - SD), ymax = Mean + SD), width = 0.2, color="black") +
      geom_jitter(data = results$long_data, aes(y = FoldChange), width = 0.1, shape=21, size=2.5, alpha=0.7, fill="grey80", show.legend = FALSE) +
      geom_hline(yintercept=1, linetype="dashed", color="red", linewidth=1) +
      geom_segment(data=results$bracket_data, aes(x=x, xend=xend, y=y, yend=y), inherit.aes=FALSE, color="black") +
      geom_segment(data=results$bracket_data, aes(x=x, xend=x, y=y, yend=y - 0.01 * plot_max_y), inherit.aes=FALSE, color="black") +
      geom_segment(data=results$bracket_data, aes(x=xend, xend=xend, y=y, yend=y - 0.01 * plot_max_y), inherit.aes=FALSE, color="black") +
      geom_text(data=results$bracket_data, aes(x=(x+xend)/2, y=y_label, label=label), inherit.aes=FALSE, vjust=-0.4, size=6) +
      coord_cartesian(ylim=c(0, plot_max_y), clip="off") +
      labs(y=expression("Fold Change (2"^{-ΔΔCq}*")"), x="Group", 
           title=paste("Gene Expression of", input$anova_ddCq_target),
           subtitle = results$f_test_text) +
      theme_classic(base_size = 14) +
      theme(axis.text.x=element_text(size=12, color="black", angle=45, hjust=1),
            axis.text.y=element_text(color="black"),
            axis.title=element_text(size=14,face="bold", color="black"),
            plot.title=element_text(size=16,face="bold",hjust=0.5, color="black"),
            plot.subtitle = element_text(size = 12, hjust = 0.5, color = "black"),
            legend.title=element_text(size=12,face="bold", color="black"),
            legend.text=element_text(size=10, color="black"),
            plot.margin = margin(t=30, r=10, b=10, l=10))
    
    selected_palette <- input$color_palette_anova_ddcq
    if (is.null(selected_palette)) { return(p) }
    
    if (selected_palette == "Viridis") {
      p <- p + scale_fill_viridis_d()
    } else if (selected_palette == "Grayscale") {
      p <- p + scale_fill_grey()
    } else if (selected_palette != "Default") {
      p <- p + scale_fill_brewer(palette = selected_palette)
    }
    
    return(p)
  })
  
  
  # --- Dynamic UI for Plots ---
  output$resultPlotUI <- renderUI({
    results <- analysis_results()
    width_val <- "100%"
    height_val <- "600px"
    if (!is.null(results)) {
      n_bars <- nrow(results$summary_data_plot)
      n_brackets <- nrow(results$bracket_data)
      width_px <- max(600, 200 + n_bars * 60)
      height_px <- 500 + n_brackets * 35
      width_val <- paste0(width_px, "px")
      height_val <- paste0(height_px, "px")
    }
    plotOutput("resultPlot", width = width_val, height = height_val)
  })
  
  output$ddCqPlotUI <- renderUI({
    results <- ddCq_analysis_results()
    width_val <- "100%"
    height_val <- "600px"
    if (!is.null(results)) {
      n_bars <- nrow(results$summary_data_plot)
      n_brackets <- nrow(results$bracket_data)
      width_px <- max(500, 150 + n_bars * 100)
      height_px <- 500 + n_brackets * 40
      width_val <- paste0(width_px, "px")
      height_val <- paste0(height_px, "px")
    }
    plotOutput("ddCq_plot_output", width = width_val, height = height_val)
  })
  
  output$anovaPlotUI <- renderUI({
    results <- anova_results()
    width_val <- "100%"
    height_val <- "600px"
    if (!is.null(results)) {
      n_bars <- nrow(results$summary_data_plot)
      n_brackets <- nrow(results$bracket_data)
      width_px <- max(500, 150 + n_bars * 100)
      height_px <- 500 + n_brackets * 50
      width_val <- paste0(width_px, "px")
      height_val <- paste0(height_px, "px")
    }
    plotOutput("anovaPlot", width = width_val, height = height_val)
  })
  
  output$anovaDdCqPlotUI <- renderUI({
    results <- anova_ddCq_results()
    width_val <- "100%"
    height_val <- "600px"
    if (!is.null(results)) {
      n_bars <- nrow(results$summary_data_plot)
      n_brackets <- nrow(results$bracket_data)
      width_px <- max(500, 150 + n_bars * 100)
      height_px <- 500 + n_brackets * 50
      width_val <- paste0(width_px, "px")
      height_val <- paste0(height_px, "px")
    }
    plotOutput("anovaDdCqPlot", width = width_val, height = height_val)
  })
  
  # --- Outputs ---
  output$resultPlot <- renderPlot({ req(analysis_results()); deltaCq_plot_obj() })
  output$ddCq_plot_output <- renderPlot({ req(ddCq_analysis_results()); ddCq_plot_obj() })
  output$anovaPlot <- renderPlot({ req(anova_results()); anova_plot_obj() })
  output$anovaDdCqPlot <- renderPlot({ req(anova_ddCq_results()); anova_ddCq_plot_obj() })
  
  output$summary_table <- DT::renderDataTable({
    results <- analysis_results(); if(!is.null(results)) DT::datatable(results$summary_table, options = list(scrollX=TRUE), rownames=FALSE)
  })
  output$ddCq_table_output <- DT::renderDataTable({
    results <- ddCq_analysis_results(); if(!is.null(results)) DT::datatable(results$summary_table, options = list(scrollX=TRUE), rownames=FALSE)
  })
  output$anova_table_output <- DT::renderDataTable({
    results <- anova_results(); if(!is.null(results)) DT::datatable(results$summary_table, options = list(scrollX=TRUE), rownames=FALSE)
  })
  output$anova_ddCq_table_output <- DT::renderDataTable({
    results <- anova_ddCq_results(); if(!is.null(results)) DT::datatable(results$summary_table, options = list(scrollX=TRUE), rownames=FALSE)
  })
  
  # --- This observeEvent updates the reference dimensions for the aspect ratio calculation.
  observeEvent(analysis_results(), {
    req(analysis_results())
    results <- analysis_results()
    req(results$summary_data_plot, results$bracket_data)
    n_bars <- length(unique(results$summary_data_plot$label))
    n_brackets <- nrow(results$bracket_data)
    dims <- list(width = 400 + (n_bars * 100), height = 400 + (n_bars * 40) + (n_brackets * 30))
    plot_dims(dims)
  })
  
  observeEvent(ddCq_analysis_results(), {
    req(ddCq_analysis_results())
    results <- ddCq_analysis_results()
    req(results$summary_data_plot, results$bracket_data)
    n_bars <- length(unique(results$summary_data_plot$group))
    n_brackets <- nrow(results$bracket_data)
    dims <- list(width = 400 + (n_bars * 100), height = 400 + (n_bars * 40) + (n_brackets * 30))
    ddCq_plot_dims(dims)
  })
  
  observeEvent(anova_results(), {
    req(anova_results())
    results <- anova_results()
    req(results$summary_data_plot, results$bracket_data)
    n_bars <- length(unique(results$summary_data_plot$group))
    n_brackets <- nrow(results$bracket_data)
    dims <- list(width = 400 + (n_bars * 100), height = 400 + (n_bars * 40) + (n_brackets * 30))
    anova_plot_dims(dims)
  })
  
  observeEvent(anova_ddCq_results(), {
    req(anova_ddCq_results())
    results <- anova_ddCq_results()
    req(results$summary_data_plot, results$bracket_data)
    n_bars <- length(unique(results$summary_data_plot$group))
    n_brackets <- nrow(results$bracket_data)
    dims <- list(width = 400 + (n_bars * 100), height = 400 + (n_bars * 40) + (n_brackets * 30))
    anova_ddCq_plot_dims(dims)
  })
  
  # --- Aspect Ratio Synchronization Logic ---
  observeEvent(input$plot_width, {
    req(isTRUE(input$fix_ratio))
    dims <- plot_dims()
    req(dims, dims$width > 0)
    ratio <- dims$height / dims$width
    new_height <- round(input$plot_width * ratio, 2)
    if (!is.null(input$plot_height) && new_height != input$plot_height) {
      updateSliderInput(session, "plot_height", value = new_height)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$plot_height, {
    req(isTRUE(input$fix_ratio))
    dims <- plot_dims()
    req(dims, dims$height > 0)
    ratio <- dims$width / dims$height
    new_width <- round(input$plot_height * ratio, 2)
    if (!is.null(input$plot_width) && new_width != input$plot_width) {
      updateSliderInput(session, "plot_width", value = new_width)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$plot_width_ddCq, {
    req(isTRUE(input$fix_ratio_ddCq))
    dims <- ddCq_plot_dims()
    req(dims, dims$width > 0)
    ratio <- dims$height / dims$width
    new_height <- round(input$plot_width_ddCq * ratio, 2)
    if (!is.null(input$plot_height_ddCq) && new_height != input$plot_height_ddCq) {
      updateSliderInput(session, "plot_height_ddCq", value = new_height)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$plot_height_ddCq, {
    req(isTRUE(input$fix_ratio_ddCq))
    dims <- ddCq_plot_dims()
    req(dims, dims$height > 0)
    ratio <- dims$width / dims$height
    new_width <- round(input$plot_height_ddCq * ratio, 2)
    if (!is.null(input$plot_width_ddCq) && new_width != input$plot_width_ddCq) {
      updateSliderInput(session, "plot_width_ddCq", value = new_width)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$plot_width_anova, {
    req(isTRUE(input$fix_ratio_anova))
    dims <- anova_plot_dims()
    req(dims, dims$width > 0)
    ratio <- dims$height / dims$width
    new_height <- round(input$plot_width_anova * ratio, 2)
    if (!is.null(input$plot_height_anova) && new_height != input$plot_height_anova) {
      updateSliderInput(session, "plot_height_anova", value = new_height)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$plot_height_anova, {
    req(isTRUE(input$fix_ratio_anova))
    dims <- anova_plot_dims()
    req(dims, dims$height > 0)
    ratio <- dims$width / dims$height
    new_width <- round(input$plot_height_anova * ratio, 2)
    if (!is.null(input$plot_width_anova) && new_width != input$plot_width_anova) {
      updateSliderInput(session, "plot_width_anova", value = new_width)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$plot_width_anova_ddCq, {
    req(isTRUE(input$fix_ratio_anova_ddCq))
    dims <- anova_ddCq_plot_dims()
    req(dims, dims$width > 0)
    ratio <- dims$height / dims$width
    new_height <- round(input$plot_width_anova_ddCq * ratio, 2)
    if (!is.null(input$plot_height_anova_ddCq) && new_height != input$plot_height_anova_ddCq) {
      updateSliderInput(session, "plot_height_anova_ddCq", value = new_height)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$plot_height_anova_ddCq, {
    req(isTRUE(input$fix_ratio_anova_ddCq))
    dims <- anova_ddCq_plot_dims()
    req(dims, dims$height > 0)
    ratio <- dims$width / dims$height
    new_width <- round(input$plot_height_anova_ddCq * ratio, 2)
    if (!is.null(input$plot_width_anova_ddCq) && new_width != input$plot_width_anova_ddCq) {
      updateSliderInput(session, "plot_width_anova_ddCq", value = new_width)
    }
  }, ignoreInit = TRUE)
  
  # --- Interactive UI Logic (Enable/Disable DPI Slider) ---
  observeEvent(input$plot_format_dcq, {
    if (input$plot_format_dcq == "pdf") {
      shinyjs::disable("plot_dpi")
    } else {
      shinyjs::enable("plot_dpi")
    }
  })
  
  observeEvent(input$plot_format_ddcq, {
    if (input$plot_format_ddcq == "pdf") {
      shinyjs::disable("plot_dpi_ddCq")
    } else {
      shinyjs::enable("plot_dpi_ddCq")
    }
  })
  
  observeEvent(input$plot_format_anova, {
    if (input$plot_format_anova == "pdf") {
      shinyjs::disable("plot_dpi_anova")
    } else {
      shinyjs::enable("plot_dpi_anova")
    }
  })
  
  observeEvent(input$plot_format_anova_ddcq, {
    if (input$plot_format_anova_ddcq == "pdf") {
      shinyjs::disable("plot_dpi_anova_ddCq")
    } else {
      shinyjs::enable("plot_dpi_anova_ddCq")
    }
  })
  
  
  # --- Downloads & Diagnostics ---
  output$download_csv <- downloadHandler(
    filename = function() { paste0(input$client_time %||% "analysis", "_stats.csv") },
    content = function(file) { write.csv(analysis_results()$raw_data_for_dl, file, row.names = FALSE, fileEncoding = "UTF-8") }
  )
  
  output$download_plot <- downloadHandler(
    filename = function() { paste0(input$client_time %||% "analysis", "_plot_custom.", input$plot_format_dcq) },
    content = function(file) {
      req(deltaCq_plot_obj())
      ggsave(file, plot = deltaCq_plot_obj(),
             width = input$plot_width, height = input$plot_height, dpi = input$plot_dpi)
    }
  )
  
  output$download_plot_asis <- downloadHandler(
    filename = function() { paste0(input$client_time %||% "analysis", "_plot_displayed.png") },
    content = function(file) {
      req(deltaCq_plot_obj())
      w <- session$clientData$output_resultPlot_width
      h <- session$clientData$output_resultPlot_height
      req(w, h)
      dpi <- 96
      ggsave(file, plot = deltaCq_plot_obj(), width = w / dpi, height = h / dpi, dpi = dpi, units = "in")
    }
  )
  
  output$ddCq_csv <- downloadHandler(
    filename = function() { paste0(input$client_time %||% "analysis", "_ddCq_stats.csv") },
    content = function(file) { write.csv(ddCq_analysis_results()$summary_table, file, row.names = FALSE, fileEncoding = "UTF-8") }
  )
  
  output$ddCq_plot <- downloadHandler(
    filename = function() { paste0(input$client_time %||% "analysis", "_ddCq_plot_custom.", input$plot_format_ddcq) },
    content = function(file) {
      req(ddCq_plot_obj())
      ggsave(file, plot = ddCq_plot_obj(),
             width = input$plot_width_ddCq, height = input$plot_height_ddCq, dpi = input$plot_dpi_ddCq)
    }
  )
  
  output$ddCq_plot_asis <- downloadHandler(
    filename = function() { paste0(input$client_time %||% "analysis", "_ddCq_plot_displayed.png") },
    content = function(file) {
      req(ddCq_plot_obj())
      w <- session$clientData$output_ddCq_plot_output_width
      h <- session$clientData$output_ddCq_plot_output_height
      req(w, h)
      dpi <- 96
      ggsave(file, plot = ddCq_plot_obj(), width = w / dpi, height = h / dpi, dpi = dpi, units = "in")
    }
  )
  
  output$anova_csv <- downloadHandler(
    filename = function() { paste0(input$client_time %||% "analysis", "_anova_stats.csv") },
    content = function(file) { write.csv(anova_results()$summary_table, file, row.names = FALSE, fileEncoding = "UTF-8") }
  )
  
  output$anova_plot <- downloadHandler(
    filename = function() { paste0(input$client_time %||% "analysis", "_anova_plot.", input$plot_format_anova) },
    content = function(file) {
      req(anova_plot_obj())
      ggsave(file, plot = anova_plot_obj(), width = input$plot_width_anova, height = input$plot_height_anova, dpi = input$plot_dpi_anova)
    }
  )
  
  output$anova_plot_asis <- downloadHandler(
    filename = function() { paste0(input$client_time %||% "analysis", "_anova_plot_displayed.png") },
    content = function(file) {
      req(anova_plot_obj())
      w <- session$clientData$output_anovaPlot_width
      h <- session$clientData$output_anovaPlot_height
      req(w, h)
      dpi <- 96
      ggsave(file, plot = anova_plot_obj(), width = w / dpi, height = h / dpi, dpi = dpi, units = "in")
    }
  )
  
  output$anova_ddCq_csv <- downloadHandler(
    filename = function() { paste0(input$client_time %||% "analysis", "_anova_ddCq_stats.csv") },
    content = function(file) { write.csv(anova_ddCq_results()$summary_table, file, row.names = FALSE, fileEncoding = "UTF-8") }
  )
  
  output$anova_ddCq_plot <- downloadHandler(
    filename = function() { paste0(input$client_time %||% "analysis", "_anova_ddCq_plot.", input$plot_format_anova_ddcq) },
    content = function(file) {
      req(anova_ddCq_plot_obj())
      ggsave(file, plot = anova_ddCq_plot_obj(), width = input$plot_width_anova_ddCq, height = input$plot_height_anova_ddCq, dpi = input$plot_dpi_anova_ddCq)
    }
  )
  
  output$anova_ddCq_plot_asis <- downloadHandler(
    filename = function() { paste0(input$client_time %||% "analysis", "_anova_ddCq_plot_displayed.png") },
    content = function(file) {
      req(anova_ddCq_plot_obj())
      w <- session$clientData$output_anovaDdCqPlot_width
      h <- session$clientData$output_anovaDdCqPlot_height
      req(w, h)
      dpi <- 96
      ggsave(file, plot = anova_ddCq_plot_obj(), width = w / dpi, height = h / dpi, dpi = dpi, units = "in")
    }
  )
  
  output$download_template <- downloadHandler(filename = "Click-qPCR_template.csv", content = function(file){ write.csv(read.csv(text=sample_csv_text), file, row.names=FALSE, fileEncoding = "UTF-8") })
  
  # --- Diagnostics Section ---
  observeEvent(input$run_diagnostics, {
    report <- data.frame(Check = character(), Result = character(), stringsAsFactors = FALSE)
    
    tryCatch({
      df <- read.csv(text = sample_csv_text, stringsAsFactors = FALSE, check.names = FALSE)
      test_passed <- nrow(df) > 0 && all(c("sample", "group", "gene", "Cq") %in% names(df))
      report <- rbind(report, data.frame(Check = "Test 1: Sample data loads correctly with required columns", Result = ifelse(test_passed, "Passed ✅", "Failed ❌")))
    }, error = function(e) {
      report <<- rbind(report, data.frame(Check = "Test 1: Sample data loads correctly with required columns", Result = "Failed ❌"))
    })
    
    tryCatch({
      ref_genes_diag <- c("Gapdh", "Actb")
      target_gene <- "Hoge"
      group1 <- "Treatment_X"
      group2 <- "Control"
      
      mean_ref_cq_diag <- sample_df_global %>%
        filter(gene %in% ref_genes_diag) %>%
        group_by(sample) %>%
        summarise(mean_ref_cq = mean(Cq, na.rm = TRUE), .groups = "drop")
      
      deltaCq_data_diag <- sample_df_global %>%
        filter(gene == target_gene, group %in% c(group1, group2)) %>%
        inner_join(mean_ref_cq_diag, by = "sample") %>%
        mutate(deltaCq = Cq - mean_ref_cq)
      
      deltaCq_vals_group1 <- deltaCq_data_diag$deltaCq[deltaCq_data_diag$group == group1]
      deltaCq_vals_group2 <- deltaCq_data_diag$deltaCq[deltaCq_data_diag$group == group2]
      
      p_value <- t.test(deltaCq_vals_group1, deltaCq_vals_group2, na.rm = TRUE)$p.value
      test_passed <- is.numeric(p_value)
      
      report <- rbind(report, data.frame(Check = "Test 2: ΔCq values are calculated and t-test runs", Result = ifelse(test_passed, "Passed ✅", "Failed ❌")))
    }, error = function(e) {
      report <<- rbind(report, data.frame(Check = "Test 2: ΔCq values are calculated and t-test runs", Result = paste("Failed ❌:", e$message)))
    })
    
    tryCatch({
      ref_genes_diag <- c("Gapdh", "Actb")
      target_gene <- "Hoge"
      base_group <- "Control"
      comp_group <- "Treatment_X"
      
      mean_ref_cq_diag <- sample_df_global %>%
        filter(gene %in% ref_genes_diag) %>%
        group_by(sample) %>%
        summarise(mean_ref_cq = mean(Cq, na.rm = TRUE), .groups = "drop")
      
      deltaCq_data <- sample_df_global %>%
        filter(gene == target_gene, group %in% c(base_group, comp_group)) %>%
        inner_join(mean_ref_cq_diag, by = "sample") %>%
        mutate(deltaCq = Cq - mean_ref_cq) %>%
        filter(!is.na(deltaCq))
      
      deltaCq_base <- deltaCq_data$deltaCq[deltaCq_data$group == base_group]
      deltaCq_comp <- deltaCq_data$deltaCq[deltaCq_data$group == comp_group]
      
      p_value <- t.test(deltaCq_base, deltaCq_comp, na.rm = TRUE)$p.value
      test_passed <- is.numeric(p_value)
      
      report <- rbind(report, data.frame(Check = "Test 3: ΔΔCq analysis (t-test on ΔCq values) runs", Result = ifelse(test_passed, "Passed ✅", "Failed ❌")))
      
    }, error = function(e) {
      report <<- rbind(report, data.frame(Check = "Test 3: ΔΔCq analysis (t-test on ΔCq values) runs", Result = paste("Failed ❌:", e$message)))
    })
    
    tryCatch({
      ref_genes_diag <- c("Gapdh", "Actb")
      target_gene <- "Hoge"
      control_group <- "Control"
      treatment_groups <- c("Treatment_X", "Treatment_Y")
      selected_groups <- c(control_group, treatment_groups)
      
      mean_ref_cq_diag <- sample_df_global %>%
        filter(gene %in% ref_genes_diag) %>%
        group_by(sample) %>%
        summarise(mean_ref_cq = mean(Cq, na.rm = TRUE), .groups = "drop")
      
      deltaCq_data <- sample_df_global %>%
        filter(gene == target_gene, group %in% selected_groups) %>%
        inner_join(mean_ref_cq_diag, by = "sample") %>%
        mutate(deltaCq = Cq - mean_ref_cq) %>%
        filter(!is.na(deltaCq))
      
      deltaCq_data$group <- factor(deltaCq_data$group, levels = selected_groups)
      
      aov_model <- aov(deltaCq ~ group, data = deltaCq_data)
      dunnett_results <- summary(glht(aov_model, linfct = mcp(group = "Dunnett")))
      test_passed <- !is.null(dunnett_results) && is.numeric(dunnett_results$test$pvalues)
      
      report <- rbind(report, data.frame(Check = "Test 4: ANOVA and Dunnett's test run correctly", Result = ifelse(test_passed, "Passed ✅", "Failed ❌")))
    }, error = function(e) {
      report <<- rbind(report, data.frame(Check = "Test 4: ANOVA and Dunnett's test run correctly", Result = paste("Failed ❌:", e$message)))
    })
    
    output$diagnostics_output <- renderUI({
      div(
        style = "font-size: 16px;",
        lapply(1:nrow(report), function(i) {
          result_color <- ifelse(grepl("Passed", report$Result[i]), "green", "red")
          div(style = paste0("color: ", result_color, "; margin-bottom: 5px;"),
              tags$b(report$Check[i]), ": ", report$Result[i])
        })
      )
    })
  })
}


# 5. Run the Application
shinyApp(ui, server)