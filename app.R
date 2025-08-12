# 1. Load Libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(DT)
library(RColorBrewer)
library(fontawesome)

# 2. Define Sample Data
sample_csv_text <- "sample,group,gene,Cq
Mouse_A,Treatment_X,Hoge,25.58
Mouse_A,Treatment_X,Fuga,27.23
Mouse_A,Treatment_X,Gapdh,20.27
Mouse_A,Treatment_X,Wow,28.28
Mouse_B,Treatment_Y,Hoge,26.21
Mouse_B,Treatment_Y,Fuga,30.65
Mouse_B,Treatment_Y,Gapdh,20.83
Mouse_B,Treatment_Y,Wow,28.82
Mouse_C,Treatment_X,Hoge,25.79
Mouse_C,Treatment_X,Fuga,27.39
Mouse_C,Treatment_X,Gapdh,20.34
Mouse_C,Treatment_X,Wow,28.34
Mouse_D,Treatment_Y,Hoge,25.85
Mouse_D,Treatment_Y,Fuga,30.43
Mouse_D,Treatment_Y,Gapdh,20.43
Mouse_D,Treatment_Y,Wow,28.45
Mouse_E,Treatment_X,Hoge,24.81
Mouse_E,Treatment_X,Fuga,26.31
Mouse_E,Treatment_X,Gapdh,19.26
Mouse_E,Treatment_X,Wow,27.25
Mouse_F,Treatment_Y,Hoge,25.14
Mouse_F,Treatment_Y,Fuga,29.64
Mouse_F,Treatment_Y,Gapdh,19.65
Mouse_F,Treatment_Y,Wow,27.65
Mouse_G,Control_X,Hoge,27.04
Mouse_G,Control_X,Fuga,29.66
Mouse_G,Control_X,Gapdh,20.98
Mouse_G,Control_X,Wow,28.99
Mouse_H,Control_Y,Hoge,26.71
Mouse_H,Control_Y,Fuga,29.22
Mouse_H,Control_Y,Gapdh,20.26
Mouse_H,Control_Y,Wow,28.26
Mouse_I,Control_X,Hoge,26.54
Mouse_I,Control_X,Fuga,28.91
Mouse_I,Control_X,Gapdh,20.05
Mouse_I,Control_X,Wow,28.05
Mouse_J,Control_Y,Hoge,27.06
Mouse_J,Control_Y,Fuga,29.79
Mouse_J,Control_Y,Gapdh,20.93
Mouse_J,Control_Y,Wow,28.92
Mouse_K,Control_X,Hoge,26.38
Mouse_K,Control_X,Fuga,28.64
Mouse_K,Control_X,Gapdh,19.94
Mouse_K,Control_X,Wow,27.95
Mouse_L,Control_Y,Hoge,26.99
Mouse_L,Control_Y,Fuga,29.42
Mouse_L,Control_Y,Gapdh,20.61
Mouse_L,Control_Y,Wow,28.61"

sample_df_global <- read.csv(text = sample_csv_text, stringsAsFactors = FALSE, check.names = FALSE)


# 3. Define UI (User Interface)
ui <- fluidPage(title = "Click-qPCR: Ultra-Simple Tool for Interactive qPCR Data Analysis",
                tags$head(
                  tags$script(HTML("setInterval(function(){var d=new Date();var pad=n=>n<10?'0'+n:n;var datetime=d.getFullYear()+'-'+pad(d.getMonth()+1)+'-'+pad(d.getDate())+'_'+pad(d.getHours())+pad(d.getMinutes());Shiny.setInputValue('client_time',datetime);},1000);")),
                  tags$style(HTML("label { font-size: 18px !important; font-weight: normal !important; } .btn{margin-top: 5px; margin-bottom: 5px;}")),
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
                div(align = "left", style = "font-size: 40px; font-weight: bold; color: #2c3e50; margin-top: 4px; margin-bottom: 1px; font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;",
                    HTML("🧬 Click-qPCR 🧬 <span style='font-size:32px; font-weight:normal;'>(Ultra-Simple Tool for Interactive qPCR Data Analysis)</span>")),
                br(),
                div(align = "left", style = "margin-bottom: 15px; font-size: 16px; color: #555;",
                    HTML("<strong>Notifications:</strong><br># Calculate the mean of Cq values by yourself, if you have technical replicates.<br># Please cite this paper if you use this app in your research. <u>Kubota, et al. bioRxiv. 2025. (<a href='https://doi.org/10.1101/2025.05.29.656779' target='_blank'>https://doi.org/10.1101/2025.05.29.656779</a>).</u>")),
                div(align = "left", style = "margin-bottom: 3px;",
                    tags$a(href = "https://github.com/kubo-azu/Click-qPCR", target = "_blank", icon("github"), "View User Guide (English and Japanese) & Source Code on GitHub")),
                br(),
                tabsetPanel(
                  id = "main_tabs",
                  tabPanel("Analysis",
                           sidebarLayout(
                             sidebarPanel(width = 4,
                                          p(HTML("Prepare your data as a CSV file with the following four columns:<br><strong>sample</strong>, <strong>group</strong>, <strong>gene</strong>, and <strong>Cq</strong> (quantification cycle value).")),
                                          hr(),
                                          actionButton("load_example", "Use Example Data", icon=icon("lightbulb")),
                                          downloadButton("download_template", "Template CSV"),
                                          actionButton("reset_app", "Reset All", icon=icon("sync-alt"), class = "btn-danger"),
                                          br(), br(),
                                          fileInput("file", "1. Select CSV File"),
                                          actionButton("load_file", HTML('<span style="font-size: 1.15em;">2. Load File</span>'), icon=icon("upload")),   # Change the text size >> "... X.XXem; ..."
                                          div(style = "max-height: 250px; overflow-y: auto;", tableOutput("data_preview")),
                                          br(),
                                          uiOutput("refgene_selector"),
                                          uiOutput("gene_selector"),
                                          h4("3. Comparison Settings", style = "font-weight:normal;"),
                                          uiOutput("group_pairs_ui"),
                                          actionButton("add_comparison", "Add", icon=icon("plus")),
                                          actionButton("remove_comparison", "Remove", icon=icon("minus")),
                                          hr(),
                                          actionButton("analyze", "Analyze", class="btn-primary btn-lg", icon=icon("play-circle")),
                                          br(), br(),
                                          downloadButton("download_csv", "Download Stats", class="btn-success"),
                                          div(style="display: inline-block;", downloadButton("download_plot", "Download Plot", class="btn-success")),
                                          div(style="display: inline-block;", downloadButton("download_plot_asis", "Save Displayed Size", class="btn-info")),
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
                                 ΔCq = Cq(Target Gene) - Cq(Reference Gene)
                                 <br><br>
                                 <strong>2. Statistical Test:</strong><br>
                                 A <strong>Welch's two-sample t-test</strong> is performed on the <strong>ΔCq values</strong> between each treatment group and the control group to calculate the p-value.
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
                                          h5(strong("Reference Gene:")),
                                          textOutput("ddct_refgene_display"),
                                          br(),
                                          uiOutput("ddct_target_selector"),
                                          hr(),
                                          h4("ΔΔCq Comparison Settings"),
                                          uiOutput("ddct_base_group_selector"),
                                          uiOutput("ddct_comparison_groups_ui"),
                                          actionButton("add_ddct_comparison", "Add", icon=icon("plus")),
                                          actionButton("remove_ddct_comparison", "Remove", icon=icon("minus")),
                                          hr(),
                                          actionButton("ddct_analyze", "Run ΔΔCq Analysis", class="btn-primary btn-lg", icon=icon("play-circle")),
                                          br(), br(),
                                          downloadButton("ddct_csv", "Download ΔΔCq Stats", class="btn-success"),
                                          div(style="display: inline-block;", downloadButton("ddct_plot", "Download ΔΔCq Plot", class="btn-success")),
                                          div(style="display: inline-block;", downloadButton("ddct_plot_asis", "Save Displayed Size", class="btn-info")),
                                          hr(),
                                          h4("Download Plot Settings"),
                                          p("Settings for the 'Download ΔΔCq Plot' button."),
                                          fluidRow(
                                            column(6, sliderInput("plot_width_ddct", "Width (inches):", min = 4, max = 20, value = 10)),
                                            column(6, sliderInput("plot_height_ddct", "Height (inches):", min = 4, max = 20, value = 8))
                                          ),
                                          checkboxInput("fix_ratio_ddct", "Fix the ratio", value = FALSE),
                                          sliderInput("plot_dpi_ddct", "Resolution (DPI):", min = 72, max = 600, value = 300),
                                          hr(),
                                          h4("Appendix: Statistical Method"),
                                          p(HTML("<div style='font-size: 14px; color: #555; line-height: 1.5;'>
                                 <strong>1. ΔΔCq Calculation:</strong><br>
                                 ΔΔCq = ΔCq(Sample) - Mean ΔCq(Control Group)
                                 <br><br>
                                 <strong>2. Statistical Test:</strong><br>
                                 A <strong>Welch's two-sample t-test</strong> is performed on the <strong>ΔCq values</strong> between each treatment group and the control group to calculate the p-value.
                                 <br><br>
                                 <strong>3. Visualization:</strong><br>
                                 The plot displays Relative Expression (2<sup>-ΔΔCq</sup>) for intuitive visualization.
                                 </div>"))
                             ),
                             mainPanel(width = 8,
                                       tags$div(style = "width: 100%; overflow-x: auto;",
                                                uiOutput("ddctPlotUI")
                                       ),
                                       br(),
                                       DT::dataTableOutput("ddct_table_output")
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
  preview_data <- reactiveVal() # For showing a preview before loading
  analysis_results <- reactiveVal() 
  ddct_analysis_results <- reactiveVal()
  
  plot_dims <- reactiveVal(list(width = 10, height = 8))
  ddct_plot_dims <- reactiveVal(list(width = 10, height = 8))
  comparison_count <- reactiveVal(1)
  ddCq_comparison_count <- reactiveVal(1)
  
  `%||%` <- function(a, b) { if (!is.null(a)) a else b }
  
  # --- Data Loading and Reset ---
  observeEvent(input$load_example, {
    df <- read.csv(text = sample_csv_text, stringsAsFactors = FALSE)
    raw_data(df)
    preview_data(df)
    analysis_results(NULL)
    ddct_analysis_results(NULL)
    showNotification("Sample data loaded.", type = "message", duration = 3)
  })
  
  observeEvent(input$file, {
    req(input$file)
    tryCatch({
      df <- read.csv(input$file$datapath, stringsAsFactors = FALSE, check.names = FALSE)
      preview_data(df) # Show preview immediately
    }, error = function(e) {
      showNotification(paste("Could not preview file:", e$message), type = "error")
      preview_data(NULL)
    })
  })
  
  observeEvent(input$load_file, {
    req(input$file, preview_data())
    tryCatch({
      df <- preview_data()
      req(all(c("sample", "group", "gene", "Cq") %in% names(df)))
      if (!is.numeric(df$Cq)) { 
        df$Cq <- suppressWarnings(as.numeric(as.character(df$Cq))) 
        if(any(is.na(df$Cq))) {
          showNotification("Warning: Non-numeric Cq values were converted to NA.", type="warning")
        }
      }
      raw_data(df)
      analysis_results(NULL)
      ddct_analysis_results(NULL)
      showNotification("File successfully loaded for analysis.", type = "message", duration=3)
    }, error = function(e) { 
      showNotification(paste("Error loading file:", e$message), type = "error", duration=10) 
    })
  })
  
  observeEvent(input$reset_app, {
    session$reload()
  })
  
  # --- UI Rendering ---
  output$data_preview <- renderTable({ req(preview_data()); head(preview_data(), 10) })
  output$refgene_selector <- renderUI({ req(raw_data()); selectInput("refgene", "Reference Gene:", choices = unique(raw_data()$gene), selected = "Gapdh") })
  output$gene_selector <- renderUI({ req(raw_data(), input$refgene); selectInput("goi", "Target Gene(s):", choices = setdiff(unique(raw_data()$gene), input$refgene), multiple = TRUE, selected=setdiff(unique(raw_data()$gene), input$refgene)[1]) })
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
  output$ddct_refgene_display <- renderText({ req(input$refgene); input$refgene })
  output$ddct_target_selector <- renderUI({ req(raw_data(), input$refgene); selectInput("ddct_target", "Target Gene:", choices = setdiff(unique(raw_data()$gene), input$refgene)) })
  output$ddct_base_group_selector <- renderUI({ req(raw_data()); selectInput("ddct_base_group", "Base Group (Control):", choices = unique(raw_data()$group)) })
  output$ddct_comparison_groups_ui <- renderUI({
    req(raw_data(), input$ddct_base_group)
    other_groups <- setdiff(unique(raw_data()$group), input$ddct_base_group)
    lapply(1:ddCq_comparison_count(), function(i) {
      selectInput(paste0("ddct_comp_group", i), if(i==1) "Treatment Group(s)" else NULL, choices = other_groups, selected = input[[paste0("ddct_comp_group", i)]] %||% if(length(other_groups) >= i) other_groups[i] else NULL)
    })
  })
  observeEvent(input$add_ddct_comparison, { ddCq_comparison_count(ddCq_comparison_count() + 1) })
  observeEvent(input$remove_ddct_comparison, { if (ddCq_comparison_count() > 1) ddCq_comparison_count(ddCq_comparison_count() - 1) })
  
  # --- Analysis Logic ---
  observeEvent(input$analyze, {
    req(raw_data(), input$refgene, input$goi)
    
    comparison_pairs <- lapply(1:comparison_count(), function(i) unique(c(input[[paste0("group1_comp", i)]], input[[paste0("group2_comp", i)]])))
    comparison_pairs <- comparison_pairs[sapply(comparison_pairs, length) == 2]; req(length(comparison_pairs) > 0)
    
    wide_data <- raw_data() %>%
      filter(gene %in% c(input$refgene, input$goi), group %in% unlist(comparison_pairs)) %>%
      group_by(sample, group, gene) %>% summarise(Cq = mean(Cq, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = gene, values_from = Cq)
    
    req(input$refgene %in% names(wide_data)); current_goi <- intersect(input$goi, names(wide_data)); req(length(current_goi) > 0)
    
    long_data <- wide_data %>%
      mutate(across(all_of(current_goi), ~ . - .data[[input$refgene]], .names = "deltaCq_{.col}")) %>%
      pivot_longer(cols = starts_with("deltaCq_"), names_to = "gene", names_prefix = "deltaCq_", values_to = "deltaCq_val") %>%
      filter(!is.na(deltaCq_val)) %>% mutate(RelExp = 2^(-deltaCq_val))
    req(nrow(long_data) > 0)
    
    summary_data <- long_data %>% group_by(gene, group) %>%
      summarise(Mean=mean(RelExp, na.rm=TRUE), SD=sd(RelExp, na.rm=TRUE), N=n(), .groups="drop")
    
    significance_raw <- bind_rows(lapply(current_goi, function(g) {
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
      select(gene, group1, group2, p_value, sig)
    
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
    
    analysis_results(list(summary_table = significance %>% select(-any_of("pos_start")), 
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
    
    ggplot(results$summary_data_plot, aes(x=label, y=Mean, fill=group)) +
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
  })
  
  observeEvent(input$ddct_analyze, {
    req(raw_data(), input$refgene, input$ddct_target, input$ddct_base_group)
    
    ddCq_comparison_groups <- lapply(1:ddCq_comparison_count(), function(i) input[[paste0("ddct_comp_group", i)]]) %>% unlist() %>% unique()
    req(length(ddCq_comparison_groups) > 0)
    all_selected_groups_ddCq <- c(input$ddct_base_group, ddCq_comparison_groups)
    
    wide_df <- raw_data() %>%
      filter(gene %in% c(input$refgene, input$ddct_target), group %in% all_selected_groups_ddCq) %>%
      group_by(sample, group, gene) %>% summarise(Cq = mean(Cq, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = gene, values_from = Cq)
    
    req(input$refgene %in% names(wide_df), input$ddct_target %in% names(wide_df))
    
    deltaCq_data <- wide_df %>%
      mutate(deltaCq = .data[[input$ddct_target]] - .data[[input$refgene]]) %>%
      filter(!is.na(deltaCq))
    
    req(nrow(deltaCq_data) > 0, input$ddct_base_group %in% deltaCq_data$group)
    
    mean_control_deltaCq <- mean(deltaCq_data$deltaCq[deltaCq_data$group == input$ddct_base_group], na.rm = TRUE)
    ddCq_data <- deltaCq_data %>%
      mutate(deltaDeltaCq = deltaCq - mean_control_deltaCq, FoldChange = 2^(-deltaDeltaCq))
    
    summary_data <- ddCq_data %>% group_by(group) %>%
      summarise(Mean = mean(FoldChange), SD = sd(FoldChange), N=n(), .groups="drop")
    
    significance_raw <- bind_rows(lapply(ddCq_comparison_groups, function(g) {
      test_data <- filter(deltaCq_data, group %in% c(input$ddct_base_group, g))
      p_val_result <- if (n_distinct(test_data$group) == 2 && all(table(test_data$group) >= 2)) {
        tryCatch({
          t.test(deltaCq ~ group, data = test_data, na.action = na.omit)$p.value
        }, error = function(e) {
          if (grepl("essentially constant", e$message)) "Zero variance" else "Calc. Error"
        })
      } else {
        NA
      }
      data.frame(group1 = input$ddct_base_group, group2 = g, p_value_raw = p_val_result, stringsAsFactors = FALSE)
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
      select(group1, group2, p_value, sig)
    
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
    
    ddct_analysis_results(list(summary_table = significance, raw_data_for_dl = summary_data, bracket_data=bracket_data, summary_data_plot=summary_data_plot, ddCq_data=ddCq_data))
  })
  
  ddCq_plot_obj <- reactive({
    req(ddct_analysis_results())
    results <- ddct_analysis_results()
    
    plot_max_y <- if (nrow(results$bracket_data) > 0) max(results$bracket_data$y_label, na.rm=TRUE) * 1.15 else max(results$summary_data_plot$Mean + results$summary_data_plot$SD, na.rm=TRUE) * 1.2
    if(!is.finite(plot_max_y) || plot_max_y <= 0) plot_max_y <- 2
    
    num_ddCq_groups_plot <- length(results$summary_data_plot$group)
    ddCq_color_palette <- if(num_ddCq_groups_plot > 0 && num_ddCq_groups_plot <= brewer.pal.info["Paired", "maxcolors"]) {
      setNames(brewer.pal(n = max(3, num_ddCq_groups_plot), name = "Paired")[1:num_ddCq_groups_plot], results$summary_data_plot$group)
    } else {
      setNames(colorRampPalette(brewer.pal(8, "Paired"))(num_ddCq_groups_plot), results$summary_data_plot$group)
    }
    
    ggplot(results$summary_data_plot, aes(x = group, y = Mean, fill = group)) +
      geom_bar(stat="identity", color="black", width=0.7) +
      scale_fill_manual(values = ddCq_color_palette, name = "Group") +
      geom_errorbar(aes(ymin = pmax(0, Mean - SD), ymax = Mean + SD), width = 0.2, color="black") +
      geom_jitter(data = results$ddCq_data, aes(y = FoldChange), width = 0.1, shape=21, size=2.5, alpha=0.7, fill="grey80", show.legend = FALSE) +
      geom_hline(yintercept=1, linetype="dashed", color="red", size=1) +
      geom_segment(data=results$bracket_data, aes(x=x, xend=xend, y=y, yend=y), inherit.aes=FALSE, color="black") +
      geom_segment(data=results$bracket_data, aes(x=x, xend=x, y=y, yend=y - 0.01 * plot_max_y), inherit.aes=FALSE, color="black") +
      geom_segment(data=results$bracket_data, aes(x=xend, xend=xend, y=y, yend=y - 0.01 * plot_max_y), inherit.aes=FALSE, color="black") +
      geom_text(data=results$bracket_data, aes(x=(x+xend)/2, y=y_label, label=label), inherit.aes=FALSE, vjust=-0.4, size=6) +
      coord_cartesian(ylim=c(0, plot_max_y), clip="off") +
      labs(y="Fold Change (2^-ΔΔCq)", x="Group", title=paste("Gene Expression of", input$ddct_target)) +
      theme_classic(base_size = 14) +
      theme(axis.text.x=element_text(size=12, color="black", angle=45, hjust=1),
            axis.text.y=element_text(color="black"),
            axis.title=element_text(size=14,face="bold", color="black"),
            plot.title=element_text(size=16,face="bold",hjust=0.5, color="black"),
            legend.title=element_text(size=12,face="bold", color="black"),
            legend.text=element_text(size=10, color="black"),
            plot.margin = margin(t=30, r=10, b=10, l=10))
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
  
  output$ddctPlotUI <- renderUI({
    results <- ddct_analysis_results()
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
    plotOutput("ddct_plot_output", width = width_val, height = height_val)
  })
  
  # --- Outputs ---
  output$resultPlot <- renderPlot({ req(analysis_results()); deltaCq_plot_obj() })
  output$ddct_plot_output <- renderPlot({ req(ddct_analysis_results()); ddCq_plot_obj() })
  
  output$summary_table <- DT::renderDataTable({
    results <- analysis_results(); if(!is.null(results)) DT::datatable(results$summary_table, options = list(scrollX=TRUE), rownames=FALSE)
  })
  output$ddct_table_output <- DT::renderDataTable({
    results <- ddct_analysis_results(); if(!is.null(results)) DT::datatable(results$summary_table, options = list(scrollX=TRUE), rownames=FALSE)
  })
  
  # This observeEvent updates the reference dimensions for the aspect ratio calculation.
  observeEvent(analysis_results(), {
    req(analysis_results())
    results <- analysis_results()
    req(results$summary_data_plot, results$bracket_data)
    n_bars <- length(unique(results$summary_data_plot$label))
    n_brackets <- nrow(results$bracket_data)
    dims <- list(
      width = 400 + (n_bars * 100),
      height = 400 + (n_bars * 40) + (n_brackets * 30)
    )
    plot_dims(dims)
  })
  
  # This observeEvent updates the reference dimensions for the aspect ratio calculation.
  observeEvent(ddct_analysis_results(), {
    req(ddct_analysis_results())
    results <- ddct_analysis_results()
    req(results$summary_data_plot, results$bracket_data)
    n_bars <- length(unique(results$summary_data_plot$group))
    n_brackets <- nrow(results$bracket_data)
    dims <- list(
      width = 400 + (n_bars * 100),
      height = 400 + (n_bars * 40) + (n_brackets * 30)
    )
    ddct_plot_dims(dims)
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
  
  observeEvent(input$plot_width_ddct, {
    req(isTRUE(input$fix_ratio_ddct))
    dims <- ddct_plot_dims()
    req(dims, dims$width > 0)
    ratio <- dims$height / dims$width
    new_height <- round(input$plot_width_ddct * ratio, 2)
    if (!is.null(input$plot_height_ddct) && new_height != input$plot_height_ddct) {
      updateSliderInput(session, "plot_height_ddct", value = new_height)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$plot_height_ddct, {
    req(isTRUE(input$fix_ratio_ddct))
    dims <- ddct_plot_dims()
    req(dims, dims$height > 0)
    ratio <- dims$width / dims$height
    new_width <- round(input$plot_height_ddct * ratio, 2)
    if (!is.null(input$plot_width_ddct) && new_width != input$plot_width_ddct) {
      updateSliderInput(session, "plot_width_ddct", value = new_width)
    }
  }, ignoreInit = TRUE)
  
  # --- Downloads & Diagnostics ---
  output$download_csv <- downloadHandler(filename = function() { paste0(input$client_time %||% "analysis", "_stats.csv") }, content = function(file) { write.csv(analysis_results()$raw_data_for_dl, file, row.names = FALSE) })
  
  output$download_plot <- downloadHandler(
    filename = function() { paste0(input$client_time %||% "analysis", "_plot_custom.png") },
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
      dpi <- 96 # Use standard screen DPI
      ggsave(file, plot = deltaCq_plot_obj(), width = w / dpi, height = h / dpi, dpi = dpi, units = "in")
    }
  )
  
  output$ddct_csv <- downloadHandler(filename = function() { paste0(input$client_time %||% "analysis", "_ddCq_stats.csv") }, content = function(file) { write.csv(ddct_analysis_results()$summary_table, file, row.names = FALSE) })
  
  output$ddct_plot <- downloadHandler(
    filename = function() { paste0(input$client_time %||% "analysis", "_ddCq_plot_custom.png") },
    content = function(file) {
      req(ddCq_plot_obj())
      ggsave(file, plot = ddCq_plot_obj(),
             width = input$plot_width_ddct, height = input$plot_height_ddct, dpi = input$plot_dpi_ddct)
    }
  )
  
  output$ddct_plot_asis <- downloadHandler(
    filename = function() { paste0(input$client_time %||% "analysis", "_ddCq_plot_displayed.png") },
    content = function(file) {
      req(ddCq_plot_obj())
      w <- session$clientData$output_ddct_plot_output_width
      h <- session$clientData$output_ddct_plot_output_height
      req(w, h)
      dpi <- 96 # Use standard screen DPI
      ggsave(file, plot = ddCq_plot_obj(), width = w / dpi, height = h / dpi, dpi = dpi, units = "in")
    }
  )
  
  output$download_template <- downloadHandler(filename = "Click-qPCR_template.csv", content = function(file){ write.csv(read.csv(text=sample_csv_text), file, row.names=FALSE) })
  
  # --- Revised Diagnostics Section ---
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
      ref_gene <- "Gapdh"; target_gene <- "Hoge"; group1 <- "Treatment_X"; group2 <- "Control_X"
      
      wide_data <- sample_df_global %>%
        filter(gene %in% c(ref_gene, target_gene), group %in% c(group1, group2)) %>%
        group_by(sample, group, gene) %>% summarise(Cq = mean(Cq, na.rm = TRUE), .groups = "drop") %>%
        pivot_wider(names_from = gene, values_from = Cq)
      
      deltaCq_vals_group1 <- wide_data[[target_gene]][wide_data$group == group1] - wide_data[[ref_gene]][wide_data$group == group1]
      deltaCq_vals_group2 <- wide_data[[target_gene]][wide_data$group == group2] - wide_data[[ref_gene]][wide_data$group == group2]
      
      p_value <- t.test(deltaCq_vals_group1, deltaCq_vals_group2, na.rm = TRUE)$p.value
      test_passed <- p_value < 0.05
      
      report <- rbind(report, data.frame(Check = "Test 2: ΔCq values show a statistically significant difference", Result = ifelse(test_passed, "Passed ✅", "Failed ❌")))
    }, error = function(e) {
      report <<- rbind(report, data.frame(Check = "Test 2: ΔCq values show a statistically significant difference", Result = "Failed ❌"))
    })
    
    tryCatch({
      ref_gene <- "Gapdh"; target_gene <- "Hoge"; base_group <- "Control_X"; comp_group <- "Treatment_X"
      
      wide_df <- sample_df_global %>%
        filter(gene %in% c(ref_gene, target_gene), group %in% c(base_group, comp_group)) %>%
        group_by(sample, group, gene) %>% summarise(Cq = mean(Cq, na.rm = TRUE), .groups = "drop") %>%
        pivot_wider(names_from = gene, values_from = Cq)
      
      deltaCq_data <- wide_df %>%
        mutate(deltaCq = .data[[target_gene]] - .data[[ref_gene]]) %>%
        filter(!is.na(deltaCq))
      
      mean_control_deltaCq <- mean(deltaCq_data$deltaCq[deltaCq_data$group == base_group], na.rm = TRUE)
      ddCq_data <- deltaCq_data %>%
        mutate(deltaDeltaCq = deltaCq - mean_control_deltaCq, FoldChange = 2^(-deltaDeltaCq))
      
      fold_change_vals <- ddCq_data$FoldChange[ddCq_data$group == comp_group]
      p_value <- t.test(fold_change_vals, mu = 1, na.rm = TRUE)$p.value
      test_passed <- p_value < 0.05
      
      report <- rbind(report, data.frame(Check = "Test 3: ΔΔCq (Fold Change) shows expected statistical significance", Result = ifelse(test_passed, "Passed ✅", "Failed ❌")))
    }, error = function(e) {
      report <<- rbind(report, data.frame(Check = "Test 3: ΔΔCq (Fold Change) shows expected statistical significance", Result = "Failed ❌"))
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