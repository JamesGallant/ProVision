##
#shiny app for analysing mass spec data
#creators James Gallant, Tiaan Heunis
#Copyright 2019
#To do: downloads Bind plot buttonsF
#Libraries we need
require(shinydashboard)
require(shiny)
require(ggplot2)
require(dplyr)
require(reshape2)
require(shinydashboardPlus)
require(DT)
require(shinyjs)
require(stringr)
require(rhandsontable)
require(colourpicker)
require(shinyWidgets)
require(RColorBrewer)
require(Hmisc)
require(limma)


#starting cod e for the app

#user interface starts here
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "ProVision"),
  dashboardSidebar(useShinyjs(),
                   #data handling sidebar menu
                   conditionalPanel(condition = "input.main_tabs == 'data_handling'",
                                    sidebarMenu(
                                      #this is where we handle file uploading
                                      menuItem("Upload your files", tabName = "file_upload", 
                                               icon = icon("upload"),
                                               fileInput("user_file", "Choose your file",
                                                         accept = c("text/csv",
                                                                    "text/comma-separated-value",
                                                                    ".txt")),
                                               div(style = "text-align:center", "The proteinGroups file is located",
                                                   br(), "in the 'txt' folder of",
                                                   br(), "the maxQuant output files")),
                                      #Filtering comes here
                                      menuItem("Filter and Transform", tabName = "file_filter",
                                               icon = icon("filter"),
                                               #filtering buttons
                                               div(style = "text-align:center", "choose minimum unique peptides",
                                                   br(), "default of 2 is chosen automatically"),
                                               numericInput(inputId = "user_unique_pep",
                                                            value = 2, 
                                                            label = "Unique peptides",
                                                            min = 1),
                                               div(style="text-align:center", "Log transforms intensity data",
                                                   br(), "This will make intenstiy data", 
                                                   br(), "normally distributed and",
                                                   br(), "is recommended for stats"),
                                               checkboxInput(inputId = "logTransform", 
                                                             label = "Log2 transform", 
                                                             value = TRUE),
                                               #this activates the filtering process
                                               actionButton(inputId = "activate_filter",
                                                            label = "Start filtering",
                                                            icon = icon("play-circle"),
                                                            width = 200)),
                                      menuItem("Assign groups",
                                               icon = icon("clone"),
                                               div(style = "text-align:center", "Click to render annotation table"),
                                               actionButton(inputId = "start_anno",
                                                            label = "Start",
                                                            icon = icon("play-circle"),
                                                            width = 200),
                                               br(),
                                               div(style = "text-align:center", "Your input in the second table",
                                                   br(), "will be used for stastical",
                                                   br(), "comparisons. Make sure to",
                                                   br(), "rename your replicates with",
                                                   br(), "the same annotion.",
                                                   br(), "Example:"),
                                               img(src = 'img1.tif', width = '100%'),
                                               
                                               div(style = "text-align:center", "once done renaming press",
                                                   br(), "submit to lock in the annotation"),
                                               actionButton(inputId = "submit_anno",
                                                            label = "Submit",
                                                            icon = icon("running"),
                                                            width = 200),
                                               div(style = "text-align:center", "Enable submit button"),
                                               actionButton(inputId = "anno_enable",
                                                            label = "Redo",
                                                            icon = icon("backward"),
                                                            width = 200)),
                                      menuItem("Filter valid values",
                                               icon = icon("filter"),
                                               numericInput(inputId = "min_val_user", 
                                                            label = "Minimum values per replicate", 
                                                            value = 2,
                                                            min = 1
                                                            ),
                                               radioButtons(inputId = "in_one",
                                                            label = "Filter valid values by group",
                                                            choices = c("In at least one group" = TRUE,
                                                                        "In each group"= FALSE), 
                                                            selected = TRUE),
                                               actionButton(inputId = "filter_valids",
                                                            label = "filter",
                                                            icon = icon("filter"),
                                                            width = 200)),
                                      
                                      #imputation
                                      menuItem("Impute missing values",
                                               icon = icon("cogs"),
                                               checkboxInput(inputId = "median_center",
                                                             label = "Center the median",
                                                             width = 200,
                                                             value = TRUE),
                                               actionButton(inputId = "start_imputation",
                                                            label = "Start imputing",
                                                            width = 200,
                                                            icon = icon("play-circle"))),
                                      menuItem("Download tables",
                                               icon = icon("download"),
                                               div(style = "text-align:center", "Download the tables generated",
                                                   br(), "thus far. Final data frame will also",
                                                   br(), "be availible in the export tab"),
                                               br(),
                                               div(style = "text-align:center", "Download original unprocessed data"),
                                               downloadButton(outputId = "rawfile_download",
                                                              label = "original data frame",
                                                              style="display: block; margin: 0 auto; width: 200px;color: black;"),
                                               br(),
                                               div(style = "text-align:center", "Download data filtered", 
                                                   br(), "for contaminants"),
                                               downloadButton(outputId = "filt1_download",
                                                              label = "Processed data",
                                                              style="display: block; margin: 0 auto; width: 200px;color: black;")))),
                   #qualityMetrics sidebar
                   conditionalPanel(condition = "input.main_tabs == 'quality_metrics'",
                                    sidebarMenu(
                                      #QQ-plots
                                      menuItem("Q-Q plots",
                                               icon = icon("line-chart"),
                                               actionButton(inputId = "qqRender",
                                                            label = "Generate plot(s)",
                                                            icon = icon("play-circle"),
                                                            style ="display: block; margin: 0 auto; width: 200px;color: black;"),
                                               br(),
                                               div(style = "text-align:center; color: white, ", 
                                                   tags$b("Cycle through Q-Q plots")),
                                               disabled(actionButton(inputId = "qqPrevious",
                                                                     label = "Previous",
                                                                     icon = icon("backward"),
                                                                     style="display:inline-block;width:40%;text-align: center;"),
                                                        actionButton(inputId = "qqNext",
                                                                     label = "Next",
                                                                     icon = icon("forward"),
                                                                     style="display:inline-block;width:40%;text-align: center;")),
                                               br(),
                                               colourInput(inputId = "qq_point_col",
                                                           label = "plot point color", showColour = "both",
                                                           palette = "limited",
                                                           value = "#000000")),
                                      #scatter plots
                                      menuItem("Scatter plots",
                                               icon = icon("line-chart"),
                                               actionButton(inputId = "scatRender",
                                                            label = "Generate plot(s)",
                                                            icon = icon("play-circle"),
                                                            style ="display: block; margin: 0 auto; width: 200px;color: black;"),
                                               br(),
                                               div(style = "text-align:center; color: white, ", 
                                                   tags$b("Cycle through scatter plots")),
                                               disabled(actionButton(inputId = "scatPrevious",
                                                                     label = "Previous",
                                                                     icon = icon("backward"),
                                                                     style="display:inline-block;width:40%;text-align: center;"),
                                                        actionButton(inputId = "scatNext",
                                                                     label = "Next",
                                                                     icon = icon("forward"),
                                                                     style="display:inline-block;width:40%;text-align: center;")),
                                               colourInput(inputId = "scat_point_col",
                                                           label = "plot point color", showColour = "both",
                                                           palette = "limited",
                                                           value = "#000000")),
                                      menuItem("Correllelogram",
                                               icon = icon("line-chart"),
                                               actionButton(inputId = "corrRender",
                                                            label = "Generate plot(s)",
                                                            icon = icon("play-circle"),
                                                            style ="display: block; margin: 0 auto; width: 200px;color: black;"),
                                               radioButtons(inputId = "corrColChoice", 
                                                            label = "Choose correlation colour gradient",
                                                            choices = c("Red-Yellow-Blue" = "RdYlBu",
                                                                        "Red-Blue" = "RdBu",
                                                                        "Purple-Orange" = "PuOr",
                                                                        "Purple-Green" = "PRGn",
                                                                        "Purple-Yellow-Green" = "PiYG", 
                                                                        "Brown-Blue-Green" = "BrBG"),
                                                            selected = "RdYlBu"),
                                               sliderInput(inputId = "corrSlider", 
                                                           label = "Set lower correlation limit",
                                                           min = 0, max = 1, step = 0.05, value = 0),
                                               checkboxInput(inputId = "corrValDisp", 
                                                             label = "Show correlation value",
                                                             value = TRUE ),
                                               numericInput(inputId = "corrDecimalPos", 
                                                            label = "Number of decimal spaces", 
                                                            value = 2, min = 1)),
                                      #Principle componet analysis
                                      menuItem("PCA plots",
                                               icon = icon("line-chart"),
                                               actionButton(inputId = "pcaRender",
                                                            label = "Generate plot(s)",
                                                            icon = icon("play-circle"),
                                                            style ="display: block; margin: 0 auto; width: 200px;color: black;"),
                                              # br(),
                                               sliderInput(inputId = "pcaPlotPointSize", label = "change point size",
                                                           min = 1, max = 10, step = 1,
                                                           value = 1),
                                               menuItem(text = div(style = "text-align:left; color: white, ", 
                                                                   tags$b("Colour by group")),
                                                        icon = icon("swatchbook"),
                                                        uiOutput("pcaColChoice")),
                                               sliderInput(inputId = "pcaAlphaChannel", label = "Change transparency",
                                                           min = 0.1, max = 1, step = 0.1, value = 1),
                                               radioButtons(inputId = "pcaLegendPostition", label = "Position legend",
                                                            choices = c("right", "left", "top", "bottom", "none"), 
                                                            selected = "top"),
                                               textInput(inputId = "pcaPlotTitle", label = "Enter plot title")
                                               
                                               
                                               )#PCA close
                                    )
                                    ), #QC metrics close
                   #statistics sidebar
                   conditionalPanel(condition = "input.main_tabs == 'statistics'",
                                    sidebarMenu(
                                      br(),
                                      radioButtons(inputId = "ComparisonSwitch",
                                                   label = "Switch comparison",
                                                   choices = c("A-B" = 1,
                                                               "B-A" = 2),
                                                   selected = 1, inline = TRUE),
                                      uiOutput("statComparisonMat"),
                                      radioButtons(inputId = "UserSigCutoff",
                                                   label = "Choose significance cut off",
                                                   choices = c("Less than 0.05" = 0.05,
                                                               "Less than 0.01" = 0.01),
                                                   selected = 0.05),
                                      sliderInput(inputId = "UserFCCutoff",
                                                  label = "Choose log Fold change cutt off",
                                                  min = 0, max = 3, step = 0.5,
                                                  value = 1,
                                                  ticks = TRUE),
                                      radioButtons(inputId = "pvalAdjust",
                                                   label = "Choose p-value adjustment",
                                                   choices = c("Benjamini-Hochberg FDR" = "BH", 
                                                               "Bonferonni" = "bonferroni",
                                                               "Hommel" = "hommel",
                                                               "Benjamini-Yekutieli" = "BY"),
                                                   selected = "BH"),
                                      actionButton(inputId = "calculateStats",
                                                   label = "Calculate",
                                                   icon = icon("play-circle"),
                                                   style ="display: block; margin: 0 auto; width: 200px;color: black;"),
                                      disabled(actionButton(inputId = "statCyclePrevious",
                                                            label = "Previous",
                                                            icon = icon("backward"),
                                                            style="display:inline-block;width:40%;text-align: center;"),
                                               actionButton(inputId = "statCycleNext",
                                                            label = "Next",
                                                            icon = icon("forward"),
                                                            style="display:inline-block;width:40%;text-align: center;"))
                                      )
                                    ),
                   #figure construction sidebar
                   conditionalPanel(condition = "input.main_tabs == 'figures'",
                                    sidebarMenu(
                                      menuItem("Volcano plots",
                                               actionButton(inputId = "generateVolcs",
                                                            label = "Render plots",
                                                            icon = icon("play-circle"),
                                                            style ="display: block; margin: 0 auto; width: 200px;color: black;"),
                                               br(),
                                               div(style = "text: align-left; color: white,", tags$b("Current comparison")),
                                               verbatimTextOutput(outputId = "currentCompareText"),
                                               br(),
                                               disabled(actionButton(inputId = "volcCyclePrevious",
                                                                     label = "Previous",
                                                                     icon = icon("backward"),
                                                                     style="display:inline-block;width:40%;text-align: center;"),
                                                        actionButton(inputId = "volcCycleNext",
                                                                     label = "Next",
                                                                     icon = icon("forward"),
                                                                     style="display:inline-block;width:40%;text-align: center;")),
                                               sliderInput(inputId = "volclabelNoOfSig",
                                                           label = "Label top signficantly regulated proteins",
                                                           min = 0, max = 20, step = 1, value = 0),
                                               sliderInput(inputId = "volcPlotPointSize", label = "change point size",
                                                           min = 1, max = 10, step = 1,
                                                           value = 3),
                                               sliderInput(inputId = "volcAlphaChannel", label = "Change transparency",
                                                           min = 0.1, max = 1, step = 0.1, value = 1),
                                               menuItem(text = div(style = "text-align:left; color: white, ", 
                                                                   tags$b("Change point colours")),
                                                        icon = icon("swatchbook"),
                                                        colourInput(inputId = "volcDown",
                                                                    label = "Downregulated", showColour = "both",
                                                                    palette = "limited",
                                                                    value = "blue"),
                                                        colourInput(inputId = "volcUp",
                                                                    label = "Upregulated", showColour = "both",
                                                                    palette = "limited",
                                                                    value = "red"),
                                                        colourInput(inputId = "volcNS",
                                                                    label = "Non significant", showColour = "both",
                                                                    palette = "limited",
                                                                    value = "#000000")
                                                        ),
                                               textInput(inputId = "volcTitle",
                                                         label = "Enter plot title",
                                                         value = ""),
                                               radioButtons(
                                                 inputId = "volcFeatures",
                                                 label = "display:", 
                                                 choices = c("Lines", "Counts", "Both", "None"),
                                                 selected = "Both",
                                                 inline = TRUE),
                                               radioButtons(inputId = "volcLegendPostition", label = "Legend position",
                                                            choices = c("right", "left", "top", "bottom", "none"), 
                                                            selected = "top", inline = TRUE)
                                               
                                               
                                               
                                    ))
                                    ) #figures close
  ), #sidebar close
  
  dashboardBody(useShinyjs(),
                tabsetPanel(id = "main_tabs",
                            #welcome tab/about tab
                            tabPanel(title = "Welcome",
                                     value = "welcome",
                                     icon = icon("door-open")),
                            #data handling tab
                            tabPanel(title = "Data handling",
                                     value = "data_handling",
                                     icon = icon("table"),
                                     fluidPage(
                                       fluidRow(
                                         infoBoxOutput("data_handling_info"),
                                         valueBoxOutput("protein_ids_count"),
                                         valueBoxOutput("contaminants_count")
                                       ),
                                       fluidRow(
                                         column(width = 6,
                                                DT::dataTableOutput("user_data_in")),
                                         column(width = 3, 
                                                rHandsontableOutput("defineReps")))
                                     )),
                            #QC tab
                            tabPanel(title = "Quality metrics",
                                     value = "quality_metrics",
                                     icon = icon("chart-line"),
                                     fluidPage(
                                       fluidRow(
                                         column(6,
                                                plotOutput("qqPlot")),
                                         column(6,
                                                plotOutput("scatPlot"))),
                                       br(),
                                       fluidRow(
                                         column(6,
                                                plotOutput("corrPlot")),
                                         column(6,
                                                plotOutput("pcaPlot"))
                                       ))),
                            #statistics tab
                            tabPanel(title = "Statistics",
                                     value = "statistics",
                                     icon = icon("calculator"),
                                     fluidPage(
                                       fluidRow(
                                         infoBoxOutput("currentCompare"),
                                         valueBoxOutput("downReg"),
                                         valueBoxOutput("upReg"),
                                         valueBoxOutput("totalSig")
                                       ),
                                       fluidRow(column(12,
                                                       dataTableOutput("statsTable"))))
                                     ),
                            #figures tab
                            tabPanel(title = "Figure construction",
                                     value = "figures",
                                     icon = icon("chart-area"),
                                     dropdownButton(circle = TRUE,status = "primary", tooltip = TRUE,
                                                    icon = icon("gears"),label = "Click for more plotting options",
                                                    sliderInput(inputId = "volcTitlePos",
                                                                label = "Set title position",
                                                                min = 0, max = 1, step = 0.1,
                                                                value = 0),
                                                    radioButtons(inputId = "VolcTitleFace",
                                                                 label = "Title face",
                                                                 choices = c("plain", "bold",
                                                                             "italic", "bold.italic")),
                                                    sliderInput(inputId = "volcLinesLWD", 
                                                                label = "Set line width",
                                                                min = 0, max = 5, step = 0.1, value = 1.4),
                                                    radioButtons(inputId = "volcLinesType",
                                                                label = "Choose line type",
                                                                choices = c("solid", "dashed", 
                                                                            "dotted", "dotdash", 
                                                                            "longdash", "twodash"),
                                                                selected = "longdash", inline = TRUE),
                                                    div(tags$b("Control position of significant counts:")),
                                                    br(),
                                                    fluidRow(column(4,
                                                                    numericInput(inputId = "volcXdown",
                                                                                 label = "Downregulated x position",
                                                                                 value = -5)),
                                                             column(4,
                                                                    numericInput(inputId = "volcYdown",
                                                                                 label = "Downregulated y position",
                                                                                 value = 10))),
                                                    fluidRow(column(4,
                                                                    numericInput(inputId = "volcXup",
                                                                                 label = "Upregulated x position",
                                                                                 value = 5)),
                                                             column(4,
                                                                    numericInput(inputId = "volcYup",
                                                                                 label = "Upregulated y position",
                                                                                 value = 10)))
                                                    ),
                                     plotOutput("volcplotOut")),
                            #export tab
                            tabPanel(title = "Export",
                                     value = "export",
                                     icon = icon("file-export")))
                
  )#Body close
) #user interface close

#server starts here
server <- function(input, output, session) {
  ######Upload limit#####
  options(shiny.maxRequestSize=30*1024^2)
  ###### DATA INPUT #####
  file_upload <- reactive({
    data <- read.csv(input$user_file$datapath, 
                     stringsAsFactors = FALSE,
                     colClasses = "character",
                     sep = "\t",
                     header = TRUE)
    return(data)
  })
  
  #### Data handling ################################################################
  #functions
  #filter valid values
  filterValidVals <- function(x, in_one, user_val) {
    #count reps and get groups
    conditions <- as.data.frame(table(unlist(names(x))))
    conditions <- conditions$Var1
    
    cond.filter <- sapply(levels(conditions), function(i) {
      df2 <- x[, grepl(i, names(x))]
      counts <- rowSums(is.finite(as.matrix(df2)))
      counts >= user_val
    })
    
    if (in_one == TRUE) {
      x$keep = apply(cond.filter, 1, any)
    } else {
      x$keep = apply(cond.filter, 1, all)
    }
    
    
    return(x)
  }
  #median centering for normalisation
  center_med = function(x) {
    kol.name <- as.data.frame(table(unlist(names(x))))
    kol.name <- as.character(kol.name$Var1)
    
    x[, kol.name] = lapply(kol.name, 
                           function(i){
                             LOG2 = x[[i]]
                             LOG2[!is.finite(LOG2)] = NA
                             gMedian = median(LOG2, na.rm = TRUE)
                             LOG2 - gMedian
                           })
    #x$GeneNames <- gene.names
    return(x)
  }
  #impute by normal distro
  imputeFunc = function(x, width, downshift, centerMedian) {
    kol.name <- as.data.frame(table(unlist(names(x))))
    kol.name <- as.character(kol.name$Var1)
    
    if (centerMedian) {
      x[, kol.name] = lapply(kol.name, function(i) {
        LOG2 = x[[i]]
        LOG2[!is.finite(LOG2)] = NA
        gMedian = median(LOG2, na.rm = TRUE)
        LOG2 - gMedian
      })
    }
    
    set.seed(1)
    x[kol.name] = lapply(kol.name,
                         function(y) {
                           temp = x[[y]]
                           temp[!is.finite(temp)] = NA
                           temp.sd = width * sd(temp, na.rm = TRUE)   # shrink sd width
                           temp.mean = mean(temp, na.rm = TRUE) - 
                             downshift * sd(temp, na.rm = TRUE)   # shift mean of imputed values
                           n.missing = sum(is.na(temp))
                           temp[is.na(temp)] = rnorm(n.missing, mean = temp.mean, sd = temp.sd)                          
                           return(temp)
                         })
    return(x)
    
  }
  
  abbreviateSTR <- function(value, prefix){  # format string more concisely
    lst = c()
    for (item in value) {
      if (is.nan(item) || is.na(item)) { # if item is NaN return empty string
        lst <- c(lst, '')
        next
      }
      item <- round(item, 2) # round to two digits
      if (item == 0) { # if rounding results in 0 clarify
        item = '<.01'
      }
      item <- as.character(item)
      item <- sub("(^[0])+", "", item)    # remove leading 0: 0.05 -> .05
      item <- sub("(^-[0])+", "-", item)  # remove leading -0: -0.05 -> -.05
      lst <- c(lst, paste(prefix, item, sep = ""))
    }
    return(lst)
  }
  
  processed_data <- reactive({
    if (input$activate_filter > 0) {
      raw <- file_upload()
      uniquePep <- isolate(input$user_unique_pep)
      logTrans <- isolate(input$logTransform)
      
      # Filter identifications  
      df = raw %>%
        filter(Potential.contaminant != "+") %>%
        filter(Reverse != "+") %>%
        filter(Only.identified.by.site != "+") 
      
      df <- subset(df, df$Unique.peptides > (uniquePep-1))
      # Extract names of intensity columns
      intensity.names = grep("^LFQ.intensity", names(df), value = TRUE)
      
      
      # Cast as numeric
      df[intensity.names] = sapply(df[intensity.names], as.numeric)
      
      #logTransfomation
      if (logTrans == TRUE) {
        df[intensity.names] = log2(df[intensity.names])
        
      } 
      
      #create new dataframe from LFQ intensities
      df2 <- df[, grep(pattern ='^LFQ.intensity.*', names(df))]
      
      #assign protein IDs from oringinal this will be majority prt IDs in the end
      df2$Protein.IDs <- df$Majority.protein.IDs
      df2$Majority.protein.IDs <- df$Majority.protein.IDs
      
      #need to get everything before a ; character first
      fasta <- word(df2$Majority.protein.IDs, 1, sep = ";")
      df2$UniprotID <- str_extract(fasta,"(?<=\\|)(.+)(?=\\|)")
      temp1_genename <- str_extract(fasta,"(?<=\\|)(.+)(?=\\_)")
      df2$GeneNames <- sapply(strsplit(temp1_genename, "\\|"), "[", 2)
      
      
      #get the uniprot ID
      
      #rename cols
      names(df2) = gsub(pattern = "LFQ.intensity.", replacement = "", x = names(df2))
      
      #rownames as IDs for later
      rownames(df2) <- df2$UniprotID
      
      #remove redundant col
      df2$UniprotID <- NULL
      df2$Protein.IDs <- NULL
      df2$Majority.protein.IDs <- NULL
      orig.col.names <- colnames(df2[names(df2) != "GeneNames"])
     
      
      if (input$filter_valids > 0) {
        x <- df2
        anno_data <- anno_data()
        #need to keep genenames indexed
        gene.names <- x$GeneNames
        x$GeneNames <- NULL
        
        colnames(x) <- anno_data$annotation
        
        
        min_val_user <- isolate(input$min_val_user)
        in_one_user <- isolate(input$in_one)
        dat2 <- filterValidVals(x = x, 
                                user_val = min_val_user, 
                                in_one = in_one_user)
        
        #add gene names again
        dat2$GeneNames <- gene.names
        #filter
        dat2 <- dat2[!(dat2$keep=="FALSE"),]
        dat2$keep <- NULL
        df2 <- dat2
        
        if (input$start_imputation > 0) {
          #centering
          center_dat <- isolate(input$median_center)
          impute_dat <- isolate(input$impute_choices)
          gene.names <- df2$GeneNames
          df2$GeneNames <- NULL
          colnames(df2) <- orig.col.names
          
          df2 <- imputeFunc(x = df2, 
                            width = 0.3,
                            downshift = 1.8,
                            centerMedian = center_dat)
          anno_data <- anno_data()
          colnames(df2) <- anno_data$annotation
          df2$GeneNames <- gene.names
          
          return(df2)
        }
      }
      return(df2)
    }
    return(df2)
  })
  
  ##### get df to display for user input ###
  #we will prompt for reps like this
  categorial_anno <- reactive({
    d <- processed_data()
    d$GeneNames <- NULL
    d1 <- data.frame(ID = colnames(d),
                     annotation = colnames(d),
                     axisLabels = colnames(d))
    return(d1)
  })
  
  output$defineReps <- renderRHandsontable({
    if (input$start_anno) {
      categorial_anno <- categorial_anno()
      categorial_anno$ID <- as.character(categorial_anno$ID)
      categorial_anno$annotation <- as.character(categorial_anno$annotation)
      categorial_anno$axisLabels <- as.character(categorial_anno$axisLabels)
      rhandsontable(categorial_anno) %>%
        hot_col("ID", readOnly = T)
    }
  })
  
  #get data from user
  #displays need to signal data is submitted
  anno_data <- eventReactive(input$submit_anno, {
    reps <- isolate(input$defineReps)
    repsOut <- hot_to_r(reps)
    return(repsOut)
  })
  
  
  #This controls enabling and disabling anno button
  observe({
    if (input$submit_anno > 0) {
      disable("submit_anno")
      hide("defineReps")
    }
    if (input$anno_enable >= input$submit_anno) {
      enable("submit_anno")
      show("defineReps")
    }
  })
  
  
  ######filter based on groups#####
 
 
  #get df from user inputs
  
  ###### info boxes #####
  ##### Display data #####
  ####cant display filtered data
  output$user_data_in <-  DT::renderDataTable({
    
    #this is to remove error message in display
    if (is.null(input$user_file)) {
      return(NULL)
    } 
    
    if (input$activate_filter == 0) {
      #first display
      datatable(file_upload(), options = list(searching = F,
                                              pageLength = 20,
                                              lengthMenu = c(5, 10, 15, 20), 
                                              scrollX = T,
                                              autoWidth = TRUE
      ))
    } else if (input$activate_filter > 0) {
      datatable(processed_data(),  options = list(searching = TRUE,
                                                  pageLength = 20,
                                                  lengthMenu = c(5, 10, 15, 20), 
                                                  scrollX = T,
                                                  autoWidth = TRUE
      ))
    }
  })
  
 
   #get df from user inputs
   
  ###### info boxes #####

  #information boxes: These display dynamic help for data upload

  infovals = reactiveValues(countervalue = 0)
  
  observeEvent(input$activate_filter, {
    infovals$countervalue <- infovals$countervalue + 1
  })
  
  observeEvent(input$submit_anno, {
    infovals$countervalue <- infovals$countervalue + 1
  })
  
  observeEvent(input$anno_enable, {
    infovals$countervalue <- infovals$countervalue - 1
  })
  
  observeEvent(input$filter_valids, {
    infovals$countervalue <- infovals$countervalue + 1
  })
  
  observeEvent(input$start_imputation, {
    infovals$countervalue <- infovals$countervalue + 1
  })

  output$data_handling_info <- renderValueBox({
    #check if data is loaded
    if (is.null(input$user_file)) {
      infoBox(title = "Information",
              value = "Upload proteinGroups.txt file",
              icon = icon("info"),
              color = "olive")
      
    } else if (infovals$countervalue == 0) {
      infoBox(title = "Information",
              value = div("Next step:",
                          br(),
                          "Filter erroneus IDs"),
              icon = icon("info"),
              color = "olive",
              subtitle = "Two unique peptides is the default")
      
    } else if (infovals$countervalue == 1 ) {
      infoBox(title = "Information",
              value = div("Next step:",
                          br(),
                          "go to assign groups"),
              subtitle = "reps get the same name",
              icon = icon("info"),
              color = "olive")
      
      } else if (infovals$countervalue == 2 ) {
        infoBox(title = "Information",
                value = div("Next step:",
                            br(),
                            "valid value filtering"),
                subtitle = "Recommended choices are autoselected",
                icon = icon("info"),
                color = "olive") 
        
      } else if (infovals$countervalue == 3) {
        infoBox(title = "Information",
                value = div("Next step:",
                            br(),
                            "Impute missing data"),
                subtitle = "Optional step but recommended",
                icon = icon("info"),
                color = "olive")
      } else if (infovals$countervalue == 4) {
        infoBox(title = "Information",
                value = div("Next step:",
                            br(),
                            "Go to next tab"),
                subtitle = "Perform QC metrics assesment",
                icon = icon("info"),
                color = "olive")
      } else if (infovals$countervalue > 4) {
        infoBox(title = "Information",
                value = "Help is out of bounds",
                subtitle = "Data calculations are not affected",
                icon = icon("info"),
                color = "red")
      }
  })
  
  
  #valuebox1: This will dynamically change in accordance with protein
  #           numbers
  output$protein_ids_count <- renderValueBox({
    #Display unique peptides here
    if (is.null(input$user_file)) {
      valueBox(value = "No file loaded",
               subtitle = "Number of proteins",
               color = "aqua",
               icon = icon("list-ol"))
    } else {
      if (input$activate_filter == 0) {
        valueBox(value = nrow(file_upload()),
                 subtitle = "Number of proteins",
                 color = "orange",
                 icon = icon("list-ol")) 
      } else {
        if (input$activate_filter > 0) {
          valueBox(value = nrow(processed_data()),
                   subtitle = "Number of proteins",
                   color = "orange",
                   icon = icon("list-ol")) 
        }
      } 
    }
  }) #protein ID close
  
  #valuebox 2: This will display contaminants and such
  output$contaminants_count <- renderValueBox({
    
    if (is.null(input$user_file)) {
      valueBox(value = "No file loaded",
               subtitle = "Potential errouneuos protein IDs",
               color = "aqua",
               icon = icon("exclamation-triangle"))
    } else {
      if (input$activate_filter == 0) {
        rawFile <- file_upload()
        contam1 <- nrow(subset(rawFile, Potential.contaminant == "+"))
        contam2 <- nrow(subset(rawFile, Reverse == "+"))
        contam3 <- nrow(subset(rawFile, Only.identified.by.site == "+"))
        contam <- sum(contam1,contam2, contam3)
        valueBox(value = contam,
                 subtitle = "Potential errouneuos protein IDs",
                 icon = icon("exclamation-triangle"),
                 color = "orange")
      } else {
        if (input$activate_filter > 0) {
          valueBox(value = 0,
                   subtitle = "Potential errouneuos protein IDs",
                   icon = icon("exclamation-triangle"),
                   color = "orange")
        }
      }
    } 
    
  }) #valuebox 2 close
  
  ##########data handling download##########

  output$rawfile_download <- downloadHandler(
    filename = function() {
      paste("proteinGroups.txt", sep = "\t")
    },
    content = function(file) {
      write.csv(file_upload(), file)
    }
  )
  
  output$filt1_download <- downloadHandler(
    filename = function() {
      paste("processedData.txt", sep = ",")
    },
    content = function(file) {
      write.csv(processed_data(), file)
    }
  )
  
  


#################################################################################
######### Quality Metrics ###########################
  Counter <- reactiveValues(qqcounter = 1,
                            scatcounter = 1)
  ###Q-Qplots###
  
  observeEvent(input$qqRender, {
    enable("qqPrevious")
    enable("qqNext")
  })
  
  observeEvent(input$qqPrevious, {
    if (Counter$qqcounter > 1) {
      Counter$qqcounter <- Counter$qqcounter - 1
    }
  })
  
  observeEvent(input$qqNext, {
    #processed_data <- processed_data()
    if (Counter$qqcounter < ncol(processed_data()) - 1) {
      Counter$qqcounter <- Counter$qqcounter + 1
    }
  })
  
  qqplot_user <- reactive({
    
    if (is.null(input$user_file)) {
      return(NULL)
    }
    processed_data <- processed_data()
    anno_data <- anno_data()
    colnames(processed_data) <- anno_data$axisLabels
    
    index <- Counter$qqcounter
    ylabname <- colnames(processed_data[index])
    plot_title <- paste("Q-Q plot of", ylabname, sep = " ")
    plot.col <- input$qq_point_col
    if (input$qqRender > 0) {
      p <- qqnorm(processed_data[[index]],
                  ylab = ylabname,
                  main = plot_title,
                  col = plot.col)
    }
    return(p)
  })
  
  output$qqPlot <- renderPlot({qqplot_user()})
  
  ###scatterplots###
  scatter_user <- reactive({
    if (is.null(input$user_file)) {
      return(NULL)
    }
    
    processed_data <- processed_data()
    anno_data <- anno_data()
    colnames(processed_data) <- anno_data$ID
    index <- Counter$scatcounter
    plot_list <- list()
    plot.col <- input$scat_point_col
    for(i in unique(anno_data$annotation)){
      COLS=anno_data$ID[anno_data$annotation ==i]
      plot_combinations <- combn(COLS,
                                 2,
                                 simplify = FALSE)
      
      for (a in 1:length(plot_combinations)) {
        p = ggplot(processed_data, 
                   aes_string(x = plot_combinations[[a]][1], 
                              y = plot_combinations[[a]][2])) +
          geom_point(pch = 21, colour = "black", fill = plot.col) + 
          theme_classic(base_size = 14)
        out_name <- paste(i,a,sep = "_")
        plot_list[[out_name]] = p
      }
    }
    
    if (input$scatRender == 0) {
      return(NULL)
    } else {
      dispPlot <- plot_list
    }
    return(dispPlot)
  })
  
  observeEvent(input$scatRender, {
    enable("scatPrevious")
    enable("scatNext")
  })
  
  observeEvent(input$scatPrevious, {
    if (Counter$scatcounter > 1) {
      Counter$scatcounter <- Counter$scatcounter - 1
    }
  })

  observeEvent(input$scatNext, {
    if (Counter$scatcounter < length(scatter_user())) {
      Counter$scatcounter <- Counter$scatcounter + 1
    } else {
      Counter$scatcounter <- Counter$scatcounter - (length(scatter_user()) + 1)
     
    }
  })
  
  output$scatPlot <- renderPlot({
    index <- Counter$scatcounter
    scatter_user()[index]})  
  
  #corrplots
  correllelogram <- reactive({
    if (is.null(input$user_file)) {
      return(NULL)
    }
    processed_data <-processed_data()
    anno_data <- anno_data()
    processed_data$GeneNames <- NULL
    colnames(processed_data) <- anno_data$axisLabels
    d <- processed_data
    cormatrix = rcorr(as.matrix(d), type='pearson')
    cordata = melt(cormatrix$r)
    cordata$labelr = abbreviateSTR(melt(cormatrix$r)$value, 'r')
    cordata$labelP = abbreviateSTR(melt(cormatrix$P)$value, 'P')
    cordata$label = paste(cordata$labelr, "\n", 
                          cordata$labelP, sep = "")
    cordata$strike = ""
    cordata$strike[cormatrix$P > 0.05] = "X"
    
    txtsize <- par('din')[2] / 2
    
    cordata$value <- round(cordata$value, digits = input$corrDecimalPos)
    
    
    if (input$corrRender == 0) {
      return(NULL)
    } else {
      p = ggplot(cordata, aes(x=Var1, y=Var2, fill=value)) +
        geom_tile() + 
        scale_fill_gradientn(colours = brewer.pal(input$corrColChoice, 
                                                  n = 11), 
                             name = "Pearson",
                             limits = c(input$corrSlider, 1)) +
        theme_classic(base_size = 14) +
        theme(axis.text.x = element_text(angle=90, hjust=TRUE)) +
        xlab(NULL) + ylab(NULL)
      
      if (input$corrValDisp == TRUE) {
       p =  p + geom_text(label=cordata$value, size=txtsize * 0.8, color="grey9") 
       return(p)
      } else {
        return(p)
      }
    }
  })
  
  output$corrPlot <- renderPlot({
    correllelogram()
    })
  
  #pca plots
  #render colour picking ui
  pcaCols <- reactive({
    anno_data <- anno_data()
    if (input$pcaRender > 0) {
      lapply(unique(anno_data$annotation), function(i) {
        colourInput(inputId = paste("col", i, sep="_"),
                    label = paste0("Choose colour for ", i), 
                    value = i, palette = "limited",
                    showColour = "both")       
      })
    }
    
  })
  
  pca_color_input <- reactive({
    if (input$pcaRender > 0) {
      anno_data <- anno_data()
      lapply(unique(anno_data$annotation), function(i) {
        input[[paste("col", i, sep="_")]]
      })
    }
  })
  
  pcaColNames <- reactive({unlist(pca_color_input())})
  
  pca <- reactive({
    if (input$pcaRender > 0) {
      dat <- processed_data()
      dat$GeneNames <- NULL
      anno_data <- anno_data()
      idnames <- anno_data$axisLabels
      colnames(dat) <- as.character(idnames)
      pcaData <- prcomp(x = t(dat), scale. = TRUE)
      pcaData <- as.data.frame(pcaData$x)
      names <- anno_data$annotation
      pcaData$names <- names
      return(pcaData)
    }
  })
  
  pcaPlots <- reactive({ 
    pcaData <- pca()
    anno_data <- anno_data()
    cols <- pcaColNames()
    if (is.null(pcaColNames())) {
      cols <- rep("#000000", length(unique(anno_data$annotation)))
    } else {
      cols <- pcaColNames()
    }
    if (input$pcaRender > 0) {
      p <- ggplot(pcaData, aes(pcaData$PC1, pcaData$PC2)) +
        geom_point(pch = 21, 
                   size = input$pcaPlotPointSize, 
                   colour = "black", 
                   alpha = input$pcaAlphaChannel,
                   aes(fill = pcaData$names)) +
        scale_fill_manual(values = cols) + 
        theme_classic() +
        ylab("Principle component 2") + xlab("Principle component 1") + 
        labs(fill = NULL) +
        ggtitle(input$pcaPlotTitle) +
        theme(legend.position = input$pcaLegendPostition)
      return(p)
    }
  })
  
  output$pcaColChoice <- renderUI({pcaCols()})
  output$pcaPlot <- renderPlot({
    if (input$pcaRender == 0) {
      return(NULL)
    } else {pcaPlots()}
    
  })
  
  ###stats###
  #control system
  observeEvent(input$calculateStats, {
    enable("statCyclePrevious")
    enable("statCycleNext")
  })
  
  statsCycler <- reactiveValues(counter = 1)
  
  observeEvent(input$statCyclePrevious, {
    if (statsCycler$counter > 1) {
      statsCycler$counter <- statsCycler$counter - 1
    }
  })
  
  statComb <- reactive({
    anno_data <- anno_data()
    comb <- combn(unique(anno_data$annotation), 2, simplify = FALSE)
    contrast <- lapply(comb, function(i) {
      if (input$ComparisonSwitch == 1) {
        compare <- paste(i[1], i[2], sep = "-")
      } else {
        compare <- paste(i[2], i[1], sep = "-")
      }
      return(compare)
    })
    })
  
  output$statComparisonMat <- renderUI({
    pickerInput(inputId = "hypoTestMat", 
                label = "Choose conditions to compare",
                choices = unlist(statComb()), 
                multiple = TRUE, 
                selected = unlist(statComb()),
                options = list(`actions-box` = TRUE, 
                               `selected-text-format` = "count > 0"), choicesOpt = list(
                                 style = rep(("color: black;"),length(statComb())))
    )
  })
  
  observeEvent(input$statCycleNext, {
    if (statsCycler$counter < length(input$hypoTestMat)) {
      statsCycler$counter <- statsCycler$counter + 1
    } else if (statsCycler$counter == length(input$hypoTestMat)) {
      statsCycler$counter <- 1
    }
  })
  
 
  #infoboxes
  output$currentCompare <- renderInfoBox({
    if (input$calculateStats > 0) {
      hypoTestMat <- input$hypoTestMat
      infoBox(title = "Current comparison",
              value = hypoTestMat[statsCycler$counter],
              icon = icon("info"),
              color = "orange")
    } else {
      infoBox(title = "Perform hypotehsis testing",
              value = "Remember to choose comparisons",
              color = "aqua")
    }
  })
  
  #Limma stats
  statsTestedData <- reactive({
    processed_data <- processed_data()
    GeneNames <- processed_data$GeneNames
    processed_data$GeneNames <- NULL
    anno_data <- anno_data()
    colnames(processed_data) <- anno_data$ID
    anno_data$axisLabels <- NULL
    if (input$calculateStats > 0) {
      f.df <- factor(anno_data$annotation)
      design <- model.matrix(~0+f.df)
      colnames(design) <- levels(f.df)
      fit <- lmFit(processed_data, design)
      f.df <- factor(anno_data$annotation)
      design <- model.matrix(~0+f.df)
      colnames(design) <- levels(f.df)
      fit <- lmFit(processed_data, design)
      
      cont.matrix <- makeContrasts(contrasts = input$hypoTestMat, levels = design)
      
      fit2 <- contrasts.fit(fit, cont.matrix)
      fit2 <- eBayes(fit2)
      return(fit2)
      
    } else {
      return(NULL)
      }
  })
  
  statsOut <- reactive({
    fit2 <- statsTestedData()
    statComb <- statComb()
    d.out <- data.frame(ID = names(fit2$coefficients[,statsCycler$counter]),
                        pValue = fit2$p.value[,statsCycler$counter],
                        qValue = p.adjust(fit2$p.value[,statsCycler$counter], input$pvalAdjust),
                        EffectSize = fit2$coefficients[,statsCycler$counter],
                        comparison = statComb[statsCycler$counter])
    d.out <- mutate(d.out, 
                    significant = ifelse(test = round(d.out$qValue, 3) < input$UserSigCutoff & d.out$EffectSize > input$UserFCCutoff,
                                         yes = "Upregulated",
                                         ifelse(test =  round(d.out$qValue, 3) < input$UserSigCutoff & d.out$EffectSize < (input$UserFCCutoff * -1),
                                                yes = "Downregulated", no = "Non signifcant")))

    return(d.out)
  })
  
 
  
  output$downReg <- renderValueBox({
    if (input$calculateStats > 0) {
      d <- statsOut()
      valueBox(value = sum(round(d$qValue, 3) < input$UserSigCutoff & d$EffectSize < (input$UserFCCutoff * -1) ),
               subtitle = "Total significantly downregulated proteins",
               color = "orange")
    } else {
      valueBox(value = NULL,
               subtitle = "No calculations performed",
               color = "aqua")
    }
  })
  output$upReg <- renderValueBox({
    if (input$calculateStats > 0) {
      d <- statsOut()
      valueBox(value = sum(round(d$qValue, 3) < input$UserSigCutoff & d$EffectSize > input$UserFCCutoff ),
               subtitle = "Total significantly upregulated proteins",
               color = "orange")
    } else {
      valueBox(value = NULL,
               subtitle = "No calculations performed",
               color = "aqua")
    }
  })
  
  output$totalSig <- renderValueBox({
    if (input$calculateStats > 0) {
      d <- statsOut()
      valueBox(value = sum(round(d$qValue, 3) < input$UserSigCutoff & abs(d$EffectSize) > input$UserFCCutoff ),
               subtitle = "Total significantly regulated proteins",
               color = "orange")
    } else {
      valueBox(value = NULL,
               subtitle = "No calculations performed",
               color = "aqua")
    }
  })
  
  
  output$statsTable <- DT::renderDataTable({
    if (input$calculateStats == 0) {
      return(NULL)
    } else {
      hypoTestMat <- input$hypoTestMat
      datatable(statsOut(), extensions = 'Buttons',
                options = list( 
                  dom = "Blfrtip",
                  buttons = 
                    list("copy", list(
                      extend = "collection",
                      buttons = c("csv", "excel", "pdf"),
                      text = "Download", filename = hypoTestMat[statsCycler$counter]
                    ) ), # end of buttons customization
                  
                  # customize the length menu
                  lengthMenu = list( c(10, 20, -1) # declare values
                                       , c(10, 20, "All") # declare titles
                  ), # end of lengthMenu customization
                 pageLength = 10 
                ))
    }
  })
  
  ###volcplots###
  observeEvent(input$generateVolcs, {
    enable("volcCyclePrevious")
    enable("volcCycleNext")
    output$currentCompareText <- renderText(input$hypoTestMat[volcCycler$counter])
  })
  
  volcCycler <- reactiveValues(counter = 1)
  
  observeEvent(input$volcCyclePrevious, {
    if (volcCycler$counter > 1) {
      volcCycler$counter <- volcCycler$counter - 1
    }
  })
  
  observeEvent(input$volcCycleNext, {
    if (volcCycler$counter < length(input$hypoTestMat)) {
      volcCycler$counter <- volcCycler$counter + 1
    } else if (volcCycler$counter == length(input$hypoTestMat)) {
      volcCycler$counter <- 1
    }
  })
  
  volcPlotData <- reactive({
    fit2 <- statsTestedData()
    statComb <- statComb()
    d.out <- data.frame(ID = names(fit2$coefficients[,volcCycler$counter]),
                        pValue = fit2$p.value[,volcCycler$counter],
                        qValue = p.adjust(fit2$p.value[,volcCycler$counter], input$pvalAdjust),
                        EffectSize = fit2$coefficients[,volcCycler$counter],
                        comparison = statComb[volcCycler$counter])
    d.out <- mutate(d.out, 
                    sig = ifelse(d.out$EffectSize > input$UserFCCutoff & round(d.out$qValue, 3) < input$UserSigCutoff, "Upregulated",
                              ifelse(d.out$EffectSize < (input$UserFCCutoff * -1) & round(d.out$qValue, 3) < input$UserSigCutoff, "Downregulated", "Non significant")))
    
    return(d.out)
  })
  
  
  
  volcPlot <- reactive({
    if (input$generateVolcs > 0) {
      d <- volcPlotData()
      d.down = sum(round(d$qValue, 3) < input$UserSigCutoff & d$EffectSize < (input$UserFCCutoff * -1) )
      d.up = sum(round(d$qValue, 3) < input$UserSigCutoff & d$EffectSize > input$UserFCCutoff )
      p <- ggplot(d, aes(x=EffectSize, y=-log10(pValue), fill = sig)) +
        xlab("log2 fold change") + ylab("-log10 p-value") + labs(fill = NULL) +
        ggtitle(label = input$volcTitle) +
        theme_classic(base_size = 14) +
        geom_point(pch = 21, colour = "black", alpha = input$volcAlphaChannel, size = input$volcPlotPointSize) +
        scale_fill_manual(values=c("Non significant" = input$volcNS,
                                   "Downregulated" = input$volcDown, 
                                   "Upregulated" = input$volcUp)) +
        theme(legend.position = input$volcLegendPostition,
              plot.title = element_text(face  = input$VolcTitleFace,
                                        hjust = input$volcTitlePos))
      
      if (input$volcFeatures == "Lines") {
        p <- p +  geom_vline(aes(xintercept = (input$UserFCCutoff*-1)),
                        lty = input$volcLinesType, 
                        colour = input$volcDown,
                        lwd=input$volcLinesLWD) +
                  geom_vline(aes(xintercept = input$UserFCCutoff), 
                         lty = input$volcLinesType, colour =input$volcUp, 
                         lwd=input$volcLinesLWD) 
      }
      
      if (input$volcFeatures == "Counts") {
        p <- p + geom_text(aes(x = input$volcXdown, y= input$volcYdown, label=d.down)) +
                 geom_text(aes(x = input$volcXup, y= input$volcYup, label=d.up))
      }
      
      if (input$volcFeatures == "Both") {
        p <- p + geom_vline(aes(xintercept = (input$UserFCCutoff*-1)),
                            lty = input$volcLinesType, 
                            colour = input$volcDown,
                            lwd=input$volcLinesLWD) +
                geom_vline(aes(xintercept = input$UserFCCutoff), 
                           lty = input$volcLinesType, colour =input$volcUp, 
                           lwd=input$volcLinesLWD) + 
                geom_text(aes(x = input$volcXdown, 
                              y=input$volcYdown, 
                              label=d.down)) +
                geom_text(aes(x = input$volcXup, 
                              y= input$volcYup, 
                              label=d.up))
      } 
      
      if (input$volcFeatures == "None") {
        p <- p
      }
      return(p)
     
    } else {
      return(NULL)
    }
    
  })

  output$volcplotOut <-renderPlot(volcPlot())
 
} #server close

shinyApp(ui, server)
