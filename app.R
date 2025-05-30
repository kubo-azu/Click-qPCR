#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#     https://shiny.posit.co/

library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(DT)
library(RColorBrewer)
library(fontawesome)

# --- Sample CSV string and global sample dataframe ---
sample_csv_text <- "sample,group,gene,Ct
Mouse_A,Treatment_X,Hoge,25.58
Mouse_A,Treatment_X,Fuga,27.23
Mouse_A,Treatment_X,Gapdh,20.27
Mouse_B,Treatment_Y,Hoge,26.21
Mouse_B,Treatment_Y,Fuga,27.65
Mouse_B,Treatment_Y,Gapdh,20.83
Mouse_C,Treatment_X,Hoge,25.79
Mouse_C,Treatment_X,Fuga,27.39
Mouse_C,Treatment_X,Gapdh,20.34
Mouse_D,Treatment_Y,Hoge,25.85
Mouse_D,Treatment_Y,Fuga,27.43
Mouse_D,Treatment_Y,Gapdh,20.43
Mouse_E,Treatment_X,Hoge,24.81
Mouse_E,Treatment_X,Fuga,26.31
Mouse_E,Treatment_X,Gapdh,19.26
Mouse_F,Treatment_Y,Hoge,25.14
Mouse_F,Treatment_Y,Fuga,26.64
Mouse_F,Treatment_Y,Gapdh,19.65
Mouse_G,Control_X,Hoge,27.04
Mouse_G,Control_X,Fuga,29.66
Mouse_G,Control_X,Gapdh,20.98
Mouse_H,Control_Y,Hoge,26.71
Mouse_H,Control_Y,Fuga,29.22
Mouse_H,Control_Y,Gapdh,20.26
Mouse_I,Control_X,Hoge,26.54
Mouse_I,Control_X,Fuga,28.91
Mouse_I,Control_X,Gapdh,20.05
Mouse_J,Control_Y,Hoge,27.06
Mouse_J,Control_Y,Fuga,29.79
Mouse_J,Control_Y,Gapdh,20.93
Mouse_K,Control_X,Hoge,26.38
Mouse_K,Control_X,Fuga,28.64
Mouse_K,Control_X,Gapdh,19.94
Mouse_L,Control_Y,Hoge,26.99
Mouse_L,Control_Y,Fuga,29.42
Mouse_L,Control_Y,Gapdh,20.61"

# UI Definition
ui <- fluidPage(
  tags$head(
    tags$script(HTML("setInterval(function(){var d=new Date();var pad=n=>n<10?'0'+n:n;var datetime=d.getFullYear()+'-'+pad(d.getMonth()+1)+'-'+pad(d.getDate())+'_'+pad(d.getHours())+pad(d.getMinutes());Shiny.setInputValue('client_time',datetime);},1000);")),
    tags$script(async = NA, src = "https://www.googletagmanager.com/gtag/js?id=G-7J5FG35PN3"),
    tags$script(HTML(
      "
      window.dataLayer = window.dataLayer || [];
      function gtag(){dataLayer.push(arguments);}
      gtag('js', new Date());
      
      gtag('config', 'G-7J5FG35PN3');
      "
    )),
    tags$style(HTML("label { font-size: 20px !important; font-weight: bold; }"))
  ),
  br(),
  div(align = "left",
      style = "font-size: 40px; font-weight: bold; color: #2c3e50; margin-top: 4px; margin-bottom: 1px; font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;",
      HTML("🧬 Click-qPCR 🧬 <span style='font-size:32px; font-weight:normal;'>(Ultra-Simple Tool for Interactive qPCR Data Analysis)</span>")
  ),
  div(align = "left", style = "margin-bottom: 16px;", 
      tags$a(href = "https://github.com/kubo-azu/Click-qPCR",
             target = "_blank",
             icon("github"), 
             "View Source Code on GitHub")
  ),
  br(),
  sidebarLayout(
    sidebarPanel(
      p(HTML("Prepare your data as a CSV file with the following four columns:<br>
             <strong>sample</strong> (sample identifier), <strong>group</strong> (e.g., treatment or condition), <strong>gene</strong> (gene name), and <strong>Ct</strong> (threshold cycle value).<br>
             Each row should represent the Ct value of one gene in one sample.<br><strong>Hint: </strong>If you have several Ct values as the technical replicates, please use the mean value to analyze with this tool.<br>")),
      br(),
      actionButton("load_example", "Use Example Data"),
      downloadButton("download_template", "Download Template CSV"),
      br(), br(),
      fileInput("file", "Upload CSV File"),
      h4("Uploaded Data Preview:"),
      div(style = "max-height: 300px; overflow-y: auto;", tableOutput("data_preview")),
      br(), br(),
      uiOutput("refgene_selector"),
      uiOutput("gene_selector"),
      uiOutput("group1_selector"),
      uiOutput("group2_selector"),
      actionButton("analyze", "Analyze"),
      br(), br(),
      downloadButton("download_csv", "Download Stats (CSV)"),
      downloadButton("download_plot", "Download Plot (PNG)"),
      br(), br(),
      h4("ΔΔCt Analysis Settings:"),
      h5(strong("Reference Gene (from main analysis):")),
      textOutput("ddct_refgene_display"),
      br(),
      uiOutput("ddct_target_selector"),
      uiOutput("ddct_control_group_selector"),
      uiOutput("ddct_treatment_group_selector"),
      actionButton("ddct_analyze", "Run ΔΔCt Analysis"),
      br(), br(),
      downloadButton("ddct_csv", "Download ΔΔCt Stats (CSV)"),
      downloadButton("ddct_plot", "Download ΔΔCt Plot (PNG)")
    ), # sidebarPanel closing
    mainPanel(
      h4("Analysis Plot:"),
      plotOutput("resultPlot", height = "600px"),
      br(), 
      h4("Statistical Summary Table:"),
      DT::dataTableOutput("summary_table"),
      br(), 
      h4("ΔΔCt Plot:"),
      plotOutput("ddct_plot_output", height = "600px"),
      br(), 
      h4("ΔΔCt Table:"),
      DT::dataTableOutput("ddct_table_output")
    ) # mainPanel closing
  ) # sidebarLayout closing
) # fluidPage closing

# Server logic
server <- function(input, output, session) {
  raw_data <- reactiveVal()
  
  deltaCt_stats_data <- reactiveVal(NULL)
  deltaCt_plot_obj <- reactiveVal(NULL)
  ddCt_stats_data <- reactiveVal(NULL)
  ddCt_plot_obj <- reactiveVal(NULL)
  
  sample_df_server <- read.csv(text = sample_csv_text, stringsAsFactors = FALSE)
  
  clear_all_analysis_outputs <- function() {
    output$resultPlot <- renderPlot({})
    output$summary_table <- DT::renderDataTable(NULL)
    deltaCt_stats_data(NULL)
    deltaCt_plot_obj(NULL)
    output$ddct_plot_output <- renderPlot({})
    output$ddct_table_output <- DT::renderDataTable(NULL)
    ddCt_stats_data(NULL)
    ddCt_plot_obj(NULL)
  }
  
  observeEvent(input$file, {
    req(input$file)
    tryCatch({
      df <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
      required_cols <- c("sample", "group", "gene", "Ct")
      if (!all(required_cols %in% names(df))) {
        missing_cols <- setdiff(required_cols, names(df))
        showNotification(paste("Uploaded CSV is missing required columns:", paste(missing_cols, collapse=", "), ". Data was not updated."), type = "error", duration = 15)
        return()
      }
      if (!is.numeric(df$Ct)) {
        original_ct_class <- class(df$Ct)
        df$Ct <- suppressWarnings(as.numeric(as.character(df$Ct)))
        if(any(is.na(df$Ct))) {
          showNotification(paste0("The 'Ct' column contained non-numeric values (original class: ", original_ct_class, ") which were coerced to NA. Please check your data."), type = "warning", duration = 15)
        } else {
          showNotification(paste0("The 'Ct' column (original class: ", original_ct_class, ") was successfully coerced to numeric."), type = "message", duration = 10)
        }
      }
      if (!is.numeric(df$Ct)) {
        showNotification("The 'Ct' column could not be converted to numeric. Please ensure it contains only numbers. Data was not updated.", type = "error", duration = 15)
        return()
      }
      raw_data(df)
      showNotification("File uploaded and data loaded successfully.", type = "message", duration = 5)
      clear_all_analysis_outputs()
    }, error = function(e) {
      showNotification(paste("Error reading CSV file:", e$message, ". Data was not updated."), type = "error", duration = 15)
    })
  })
  
  observeEvent(input$load_example, {
    raw_data(sample_df_server)
    showNotification("Sample data loaded.", type = "message", duration = 5)
    clear_all_analysis_outputs()
  })
  
  output$data_preview <- renderTable({ req(raw_data()); head(raw_data(), 10) })
  
  output$refgene_selector <- renderUI({
    req(raw_data()); genes <- unique(raw_data()$gene)
    selectInput("refgene", "Select Reference Gene:", choices = genes, selected = if(length(genes) > 0) genes[1] else NULL)
  })
  output$gene_selector <- renderUI({
    req(raw_data(), input$refgene); all_genes <- unique(raw_data()$gene)
    target_choices <- setdiff(all_genes, input$refgene)
    selectInput("goi", "Select Target Gene(s):", choices = target_choices, selected = if(length(target_choices) > 0) target_choices[1] else NULL, multiple = TRUE)
  })
  output$group1_selector <- renderUI({
    req(raw_data()); groups <- unique(raw_data()$group)
    selectInput("group1", "Select Group 1 [usually, control]:", choices = groups, selected = if(length(groups) > 0) groups[1] else NULL)
  })
  output$group2_selector <- renderUI({
    req(raw_data(), input$group1); all_groups <- unique(raw_data()$group)
    other_groups <- setdiff(all_groups, input$group1)
    selectInput("group2", "Select Group 2 [usually, treatment]:", choices = other_groups, selected = if(length(other_groups) > 0) other_groups[1] else NULL)
  })
  
  output$ddct_refgene_display <- renderText({ req(input$refgene); paste(input$refgene) })
  output$ddct_target_selector <- renderUI({
    req(raw_data(), input$refgene); all_genes <- unique(raw_data()$gene)
    target_choices_ddct <- setdiff(all_genes, input$refgene)
    selectInput("ddct_target", "Target Gene:", choices = target_choices_ddct, selected = if(length(target_choices_ddct)>0) target_choices_ddct[1] else NULL)
  })
  output$ddct_control_group_selector <- renderUI({
    req(raw_data()); groups <- unique(raw_data()$group)
    selectInput("ddct_control", "Select Control Group to Calculate Fold-Change:", choices = groups, selected = if(length(groups) > 0) groups[1] else NULL)
  })
  output$ddct_treatment_group_selector <- renderUI({
    req(raw_data(), input$ddct_control); all_groups <- unique(raw_data()$group)
    other_groups <- setdiff(all_groups, input$ddct_control)
    selectInput("ddct_treatment", "Select Target Group to Calculate Fold-Change:", choices = other_groups, selected = if(length(other_groups) > 0) other_groups[1] else NULL)
  })
  
  observeEvent(input$analyze, {
    if (is.null(raw_data())||nrow(raw_data())==0) { showNotification("Please upload data or load example data.", type = "warning"); return() }
    if (is.null(input$refgene)) { showNotification("Please select a reference gene.", type = "warning"); return() }
    if (is.null(input$goi)||length(input$goi)==0) { showNotification("Please select at least one target gene.", type = "warning"); return() }
    if (is.null(input$group1)) { showNotification("Please select Group 1.", type = "warning"); return() }
    if (is.null(input$group2)) { showNotification("Please select Group 2.", type = "warning"); return() }
    
    df_filtered <- raw_data() %>%
      dplyr::filter(gene %in% c(input$goi, input$refgene), group %in% c(input$group1, input$group2))
    if(nrow(df_filtered) == 0) { showNotification("No data remaining after filtering. Check selections.", type = "error", duration=10); return() }
    
    df_summarized_for_pivot <- df_filtered %>%
      group_by(sample, group, gene) %>% summarise(Ct = mean(Ct, na.rm = TRUE), .groups = "drop") %>% ungroup()
    if(any(is.na(df_summarized_for_pivot$Ct))){
      showNotification("NA Ct values after summarizing. Check for missing Ct values.", type="warning", duration=10)
      df_summarized_for_pivot <- df_summarized_for_pivot %>% filter(!is.na(Ct))
      if(nrow(df_summarized_for_pivot) == 0) { showNotification("No valid Ct values after NA removal pre-pivot.", type="error", duration=10); return() }
    }
    
    wide_df <- tryCatch({
      df_summarized_for_pivot %>% tidyr::pivot_wider(names_from = gene, values_from = Ct)
    }, error = function(e) { showNotification(paste("Error pivoting data:", e$message), type = "error", duration=10); return(NULL) })
    if (is.null(wide_df)||nrow(wide_df)==0) { showNotification("Could not reshape data (pivot_wider).", type="error", duration=10); return() }
    
    if (!input$refgene %in% names(wide_df)) { showNotification(paste("Ref gene '", input$refgene, "' not in reshaped data."), type = "error", duration=15); return() }
    current_goi <- input$goi
    missing_goi_cols <- setdiff(current_goi, names(wide_df))
    if (length(missing_goi_cols) > 0) {
      showNotification(paste("Target gene(s) '", paste(missing_goi_cols, collapse=","), "' not in reshaped data. Excluding."), type = "warning", duration=15)
      current_goi <- intersect(current_goi, names(wide_df))
      if (length(current_goi) == 0) { showNotification("No selected target genes in reshaped data.", type="error", duration=15); return() }
    }
    
    long_data_list <- lapply(current_goi, function(target_gene_col_name) {
      ref_gene_col_name <- input$refgene
      if (!is.numeric(wide_df[[target_gene_col_name]]) || !is.numeric(wide_df[[ref_gene_col_name]])) {
        deltaCt_values <- rep(NA_real_, nrow(wide_df))
      } else {
        deltaCt_values <- wide_df[[target_gene_col_name]] - wide_df[[ref_gene_col_name]]
      }
      temp_df <- data.frame(sample=wide_df$sample, group=wide_df$group, gene=target_gene_col_name, deltaCt_val=deltaCt_values) %>% filter(!is.na(deltaCt_val))
      if(nrow(temp_df) > 0) { temp_df <- temp_df %>% mutate(RelExp = 2^(-deltaCt_val)) }
      return(temp_df)
    })
    long_data <- dplyr::bind_rows(long_data_list)
    
    if(is.null(long_data)||nrow(long_data)==0){ showNotification("No valid data after ΔCt calculation. Ensure samples have Ct for ref and target genes.", type="error", duration=15); return() }
    
    summary_data <- long_data %>% group_by(gene, group) %>% summarise(Mean=mean(RelExp, na.rm=TRUE), SD=sd(RelExp, na.rm=TRUE), N=n(), .groups="drop") %>% mutate(label=paste(gene, group))
    significance <- long_data %>% group_by(gene) %>%
      summarise(p_value=if(NROW(na.omit(RelExp[group==input$group1]))>=2 && NROW(na.omit(RelExp[group==input$group2]))>=2){t.test(RelExp[group==input$group1], RelExp[group==input$group2])$p.value}else{NA_real_}, .groups="drop") %>%
      rename(`p (Welch's t-test)`=p_value) %>%
      mutate(sig=case_when(is.na(`p (Welch's t-test)`)~"N/A", `p (Welch's t-test)`<0.001~"***", `p (Welch's t-test)`<0.01~"**", `p (Welch's t-test)`<0.05~"*", TRUE~"ns"))
    y_max_val <- max(summary_data$Mean+summary_data$SD,0,na.rm=TRUE)*1.15
    if(is.infinite(y_max_val)||is.na(y_max_val)||y_max_val==0) y_max_val<-1
    summary_data_plot <- summary_data %>% mutate(label=factor(label, levels=unique(label)))
    get_x_pos <- function(g,grp,data_levels){pos<-which(data_levels==paste(g,grp)); if(length(pos)==0)NA else pos}
    bracket_data <- significance %>% filter(!is.na(`p (Welch's t-test)`)) %>% rowwise() %>%
      mutate(xmin=get_x_pos(gene,input$group1,levels(summary_data_plot$label)), xmax=get_x_pos(gene,input$group2,levels(summary_data_plot$label)), y=y_max_val) %>%
      ungroup() %>% filter(!is.na(xmin)&!is.na(xmax))
    
    group_names_deltaCt <- unique(summary_data_plot$group) 
    num_groups_deltaCt <- length(group_names_deltaCt)
    deltaCt_color_palette <- if(num_groups_deltaCt > 0 && num_groups_deltaCt <= brewer.pal.info["Set2", "maxcolors"]) {
      setNames(brewer.pal(n = max(3, num_groups_deltaCt), name = "Set2")[1:num_groups_deltaCt], group_names_deltaCt)
    } else if (num_groups_deltaCt > brewer.pal.info["Set2", "maxcolors"]) {
      setNames(rep(brewer.pal(n = brewer.pal.info["Set2", "maxcolors"], name = "Set2"), length.out = num_groups_deltaCt), group_names_deltaCt)
    } else { 
      setNames("grey70", if(length(group_names_deltaCt)>0) group_names_deltaCt else "DefaultGroup")
    }
    
    p <- ggplot(summary_data_plot, aes(x=label, y=Mean, fill=group)) +
      geom_bar(stat="identity", position=position_dodge(0.7), width=0.7, color="black") + 
      scale_fill_manual(values = deltaCt_color_palette, name = "Group") +
      geom_jitter(data = long_data, 
                  aes(x = paste(gene, group), y = RelExp, fill = group), 
                  position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.7),
                  shape = 21, color = "black", size = 2, alpha = 0.6,
                  show.legend = FALSE) +
      geom_errorbar(aes(ymin=pmax(0,Mean-SD), ymax=Mean+SD), width=0.2, position=position_dodge(0.7), color="black") +
      labs(x="Gene (Group)", y="Relative Expression (2^-ΔCt)") + 
      theme_classic(base_size=14) + 
      theme(axis.text.x=element_text(angle=45,hjust=1,size=12, color="black"), 
            axis.text.y=element_text(color="black"),
            axis.title=element_text(size=14,face="bold", color="black"), 
            legend.title=element_text(size=12,face="bold", color="black"), 
            legend.text=element_text(size=10, color="black"),
            panel.grid.major.y = element_line(color = "grey90"), 
            panel.grid.major.x = element_blank(), 
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.line = element_line(color = "black"),
            strip.background = element_blank(),
            strip.text = element_text(color="black", size=12, face="bold")
      ) 
    if(nrow(bracket_data)>0){
      p <- p + 
        geom_segment(data=bracket_data, aes(x=xmin,xend=xmax,y=y,yend=y), inherit.aes=FALSE, linewidth=0.5, color="black") +
        geom_segment(data=bracket_data, aes(x=xmin,xend=xmin,y=y,yend=y-0.03*y_max_val), inherit.aes=FALSE, linewidth=0.5, color="black") +
        geom_segment(data=bracket_data, aes(x=xmax,xend=xmax,y=y,yend=y-0.03*y_max_val), inherit.aes=FALSE, linewidth=0.5, color="black") +
        geom_text(data=bracket_data, aes(x=(xmin+xmax)/2,y=y+0.03*y_max_val,label=sig), inherit.aes=FALSE, size=5, vjust=-0.5, color="black")
    }
    output$resultPlot <- renderPlot({p})
    output$summary_table <- DT::renderDataTable({display_summary<-left_join(summary_data,significance,by="gene"); DT::datatable(display_summary,options=list(pageLength=10,scrollX=TRUE),rownames=FALSE)})
    deltaCt_stats_data(left_join(summary_data,significance,by="gene")); deltaCt_plot_obj(p)
  })
  
  observeEvent(input$ddct_analyze, {
    req(input$refgene)
    if (is.null(raw_data())||nrow(raw_data())==0) { showNotification("Please upload data or load example data.", type = "warning"); return() }
    if (is.null(input$ddct_target)||length(input$ddct_target)==0) { showNotification("Please select a target gene for ΔΔCt analysis.", type = "warning"); return() }
    if (is.null(input$ddct_control)) { showNotification("Please select a control group for ΔΔCt analysis.", type = "warning"); return() }
    if (is.null(input$ddct_treatment)) { showNotification("Please select a target/treatment group for ΔΔCt analysis.", type = "warning"); return() }
    
    current_ddct_refgene <- input$refgene
    df_ddct_filtered <- raw_data() %>%
      filter(gene %in% c(current_ddct_refgene, input$ddct_target), group %in% c(input$ddct_control, input$ddct_treatment))
    if(nrow(df_ddct_filtered) == 0) { showNotification("No data after filtering for ΔΔCt. Check selections.", type="error", duration=10); return() }
    
    df_ddct_summarized_for_pivot <- df_ddct_filtered %>%
      group_by(sample, group, gene) %>% summarise(Ct = mean(Ct, na.rm = TRUE), .groups = "drop") %>% ungroup()
    if(any(is.na(df_ddct_summarized_for_pivot$Ct))){
      showNotification("NA Ct values after summarizing for ΔΔCt. Check data.", type="warning", duration=10)
      df_ddct_summarized_for_pivot <- df_ddct_summarized_for_pivot %>% filter(!is.na(Ct))
      if(nrow(df_ddct_summarized_for_pivot) == 0) { showNotification("No valid Ct values for ΔΔCt after NA removal pre-pivot.", type="error", duration=10); return() }
    }
    
    wide_df <- tryCatch({
      df_ddct_summarized_for_pivot %>% tidyr::pivot_wider(names_from = gene, values_from = Ct) %>%
        {tar_g<-input$ddct_target; if(!tar_g %in% names(.))stop(paste("Target gene '",tar_g,"' not found after pivoting for ddCt.")) else .; if(!current_ddct_refgene %in% names(.))stop(paste("Ref gene '",current_ddct_refgene,"' not found after pivoting for ddCt.")) else .} %>%
        mutate(deltaCt = .data[[input$ddct_target]] - .data[[current_ddct_refgene]])
    }, error=function(e){showNotification(paste("Error preparing data for ΔΔCt:",e$message),type="error",duration=10);return(NULL)})
    if(is.null(wide_df)||nrow(wide_df)==0){showNotification("Failed to prepare data for ΔΔCt (wide_df empty).",type="error",duration=10);return()}
    wide_df <- wide_df %>% filter(!is.na(deltaCt))
    if(nrow(wide_df)==0){showNotification("All deltaCt values NA for ΔΔCt. Check Ct values.",type="error",duration=10);return()}
    
    control_mean_deltaCt <- mean(wide_df$deltaCt[wide_df$group == input$ddct_control], na.rm = TRUE)
    if(is.na(control_mean_deltaCt)){showNotification(paste("Could not calculate mean ΔCt for control group (",input$ddct_control,") in ΔΔCt."),type="error",duration=10);return()}
    ddct_data <- wide_df %>% mutate(deltaDeltaCt = deltaCt - control_mean_deltaCt, RelExp = 2^(-deltaDeltaCt)) %>% ungroup()
    ddct_summary <- ddct_data %>% filter(!is.na(RelExp)) %>% group_by(group) %>%
      summarise(Mean_FoldChange=mean(RelExp,na.rm=TRUE),SD_FoldChange=sd(RelExp,na.rm=TRUE),N=sum(!is.na(RelExp)),SE_FoldChange=SD_FoldChange/sqrt(N),.groups="drop") %>% filter(N > 0)
    if(nrow(ddct_summary)<2&&n_distinct(ddct_data$group)==2){showNotification("Not enough data for 2 groups for ΔΔCt after calculations.",type="warning",duration=10)}
    
    t_test_result_ddct <- if(nrow(ddct_summary)==2&&all(ddct_summary$N>=2)&&!is.null(ddct_data$RelExp)&&input$ddct_control%in%ddct_summary$group&&input$ddct_treatment%in%ddct_summary$group&&sum(!is.na(ddct_data$RelExp[ddct_data$group==input$ddct_control]))>=2&&sum(!is.na(ddct_data$RelExp[ddct_data$group==input$ddct_treatment]))>=2){
      tryCatch(t.test(RelExp~group,data=ddct_data%>%filter(group%in%c(input$ddct_control,input$ddct_treatment)&!is.na(RelExp))),error=function(e)list(p.value=NA_real_))}else{list(p.value=NA_real_)}
    p_val_ddct <- t_test_result_ddct$p.value
    sig_ddct <- case_when(is.na(p_val_ddct)~"N/A",p_val_ddct<0.001~"***",p_val_ddct<0.01~"**",p_val_ddct<0.05~"*",TRUE~"ns")
    y_max_ddct <- if(nrow(ddct_summary)>0&&any(!is.na(ddct_summary$Mean_FoldChange)))max(ddct_summary$Mean_FoldChange+ddct_summary$SD_FoldChange,0,na.rm=TRUE)*1.2 else 1
    if(is.infinite(y_max_ddct)||is.na(y_max_ddct)||y_max_ddct==0)y_max_ddct<-1
    
    ddct_group_names_plot <- unique(ddct_summary$group)
    num_ddct_groups_plot <- length(ddct_group_names_plot)
    ddct_color_palette <- if(num_ddct_groups_plot > 0 && num_ddct_groups_plot <= brewer.pal.info["Paired", "maxcolors"]) {
      setNames(brewer.pal(n = max(3, num_ddct_groups_plot), name = "Paired")[1:num_ddct_groups_plot], ddct_group_names_plot)
    } else if (num_ddct_groups_plot > brewer.pal.info["Paired", "maxcolors"]) {
      setNames(rep(brewer.pal(n = brewer.pal.info["Paired", "maxcolors"], name = "Paired"), length.out = num_ddct_groups_plot), ddct_group_names_plot)
    } else {
      setNames("grey50", if(length(ddct_group_names_plot)>0) ddct_group_names_plot else "DefaultGroup") 
    }
    
    p_ddct <- ggplot(ddct_summary, aes(x=factor(group), y=Mean_FoldChange, fill=factor(group))) +
      geom_bar(stat="identity", width=0.7, position=position_dodge(), color="black") +
      scale_fill_manual(values = ddct_color_palette, name = "Group") +
      geom_jitter(data = ddct_data,
                  aes(x = factor(group), y = RelExp, fill = factor(group)), 
                  position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.7),
                  shape = 21, color = "black", size = 2, alpha = 0.6,
                  show.legend = FALSE) +
      geom_errorbar(aes(ymin=pmax(0,Mean_FoldChange-SD_FoldChange), ymax=Mean_FoldChange+SD_FoldChange), width=0.2, position=position_dodge(0.7), color="black") +
      labs(x="Group",y="Fold-change (2^-ΔΔCt)",title=paste0("ΔΔCt Analysis: ",input$ddct_target," (vs ",input$ddct_control,") using Ref: ",current_ddct_refgene)) +
      theme_classic(base_size=14) +
      theme(axis.text.x=element_text(size=12, color="black"),
            axis.text.y=element_text(color="black"),
            axis.title=element_text(size=14,face="bold", color="black"),
            plot.title=element_text(size=16,face="bold",hjust=0.5, color="black"),
            legend.title=element_text(size=12,face="bold", color="black"),
            legend.text=element_text(size=10, color="black"),
            panel.grid.major.y = element_line(color = "grey90"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.line = element_line(color = "black"),
            strip.background = element_blank(),
            strip.text = element_text(color="black", size=12, face="bold")
      )
    
    if (sig_ddct!="N/A"&&nrow(ddct_summary)==2&&input$ddct_control%in%ddct_summary$group&&input$ddct_treatment%in%ddct_summary$group){
      group_levels_for_plot_ddct <- levels(factor(ddct_summary$group))
      pos_group1 <- which(group_levels_for_plot_ddct == input$ddct_control)
      pos_group2 <- which(group_levels_for_plot_ddct == input$ddct_treatment)
      if (length(pos_group1)==1 && length(pos_group2)==1 && pos_group1 != pos_group2) {
        bracket_plot_data_ddct <- data.frame(x_start_num=min(pos_group1,pos_group2),x_end_num=max(pos_group1,pos_group2),y_pos=y_max_ddct,sig_label=sig_ddct)
        p_ddct <- p_ddct +
          geom_segment(data=bracket_plot_data_ddct,aes(x=x_start_num,xend=x_end_num,y=y_pos,yend=y_pos),inherit.aes=FALSE,linewidth=0.5, color="black") +
          geom_segment(data=bracket_plot_data_ddct,aes(x=x_start_num,xend=x_start_num,y=y_pos,yend=y_pos-0.05*y_pos),inherit.aes=FALSE,linewidth=0.5, color="black") +
          geom_segment(data=bracket_plot_data_ddct,aes(x=x_end_num,xend=x_end_num,y=y_pos,yend=y_pos-0.05*y_pos),inherit.aes=FALSE,linewidth=0.5, color="black") +
          geom_text(data=bracket_plot_data_ddct,aes(x=(x_start_num+x_end_num)/2,y=y_pos+0.03*y_pos,label=sig_label),inherit.aes=FALSE,size=6,vjust=-0.5, color="black")
      }
    }
    output$ddct_plot_output <- renderPlot({p_ddct})
    output$ddct_table_output <- DT::renderDataTable({
      ddct_summary_final<-ddct_summary
      if(input$ddct_treatment%in%ddct_summary_final$group){
        ddct_summary_final<-ddct_summary_final%>%mutate(`p-value (vs Control)`=if_else(group==input$ddct_treatment,p_val_ddct,NA_real_),Significance=if_else(group==input$ddct_treatment,sig_ddct,NA_character_))
      }else{
        ddct_summary_final<-ddct_summary_final%>%mutate(`p-value (vs Control)`=NA_real_,Significance=NA_character_)
      }
      DT::datatable(ddct_summary_final,options=list(pageLength=10,scrollX=TRUE),rownames=FALSE)})
    ddCt_stats_data_final<-ddct_summary
    if(input$ddct_treatment%in%ddCt_stats_data_final$group){
      ddCt_stats_data_final<-ddCt_stats_data_final%>%mutate(`p-value (vs Control)`=if_else(group==input$ddct_treatment,p_val_ddct,NA_real_),Significance=if_else(group==input$ddct_treatment,sig_ddct,NA_character_))
    }else{
      ddCt_stats_data_final<-ddCt_stats_data_final%>%mutate(`p-value (vs Control)`=NA_real_,Significance=NA_character_)}
    ddCt_stats_data(ddCt_stats_data_final);ddCt_plot_obj(p_ddct)
  })
  
  output$download_csv <- downloadHandler(filename=function(){time_str<-if(!is.null(input$client_time))input$client_time else format(Sys.time(),"%Y-%m-%d_%H%M");paste0(time_str,"_qPCR_stats.csv")},content=function(file){req(deltaCt_stats_data());write.csv(deltaCt_stats_data(),file,row.names=FALSE)})
  output$download_plot <- downloadHandler(filename=function(){time_str<-if(!is.null(input$client_time))input$client_time else format(Sys.time(),"%Y-%m-%d_%H%M");paste0(time_str,"_qPCR_plot.png")},content=function(file){req(deltaCt_plot_obj());ggsave(file,plot=deltaCt_plot_obj(),width=10,height=8,dpi=300)})
  output$download_template <- downloadHandler(filename=function(){"Click-qPCR_template.csv"},content=function(file){write.csv(sample_df_server,file,row.names=FALSE)})
  output$ddct_csv <- downloadHandler(filename=function(){time_str<-if(!is.null(input$client_time))input$client_time else format(Sys.time(),"%Y-%m-%d_%H%M");paste0(time_str,"_ddCt_stats.csv")},content=function(file){req(ddCt_stats_data());write.csv(ddCt_stats_data(),file,row.names=FALSE)})
  output$ddct_plot <- downloadHandler(filename=function(){time_str<-if(!is.null(input$client_time))input$client_time else format(Sys.time(),"%Y-%m-%d_%H%M");paste0(time_str,"_ddCt_plot.png")},content=function(file){req(ddCt_plot_obj());ggsave(file,plot=ddCt_plot_obj(),width=10,height=8,dpi=300)})
}

shinyApp(ui, server)