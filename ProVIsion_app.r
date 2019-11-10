##
#shiny app for analysing mass spec data
#creators James Gallant, Tiaan Heunis
#Copyright 2019
#To do: clean up datahandling tab with solid interactions

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
                                                   br(), "the same identifier.",
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
                                               actionButton(inputId = "qqPrevious",
                                                            label = "Previous",
                                                            icon = icon("backward"),
                                                            style="display:inline-block;width:40%;text-align: center;"),
                                               actionButton(inputId = "qqNext",
                                                            label = "Next",
                                                            icon = icon("forward"),
                                                            style="display:inline-block;width:40%;text-align: center;"),
                                               downloadButton(outputId = "qqPlotDownload",
                                                              label = "Download",
                                                              style="display: block; margin: 0 auto; width: 200px;color: black;")),
                                      #scatter plots
                                      menuItem("Scatter plots",
                                               icon = icon("line-chart")),
                                      menuItem("Correllelogram",
                                               icon = icon("line-chart")),
                                      #Principle componet analysis
                                      menuItem("PCA plots",
                                               icon = icon("line-chart"))
                                    )), #QC metrics close
                   #figure construction
                   conditionalPanel(condition = "input.main_tabs == 'figures'",
                                    sidebarMenu())
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
                                         plotOutput("qqPlot"),
                                         plotOutput("ScatterPlot")
                                       ),
                                       fluidRow(
                                         plotOutput("correlloPlot"),
                                         plotOutput("pcaPlot"))
                                     )),
                            #statistics tab
                            tabPanel(title = "Statistics",
                                     value = "statistics",
                                     icon = icon("calculator")),
                            #figures tab
                            tabPanel(title = "Figure construction",
                                     value = "figures",
                                     icon = icon("chart-area")),
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
  #### Filtering #####
  #Automated filtered data modules
  ## minimum vals per rep not funtioning
  #######
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
                     annotation = colnames(d))
    return(d1)
  })
  
  output$defineReps <- renderRHandsontable({
    if (input$start_anno) {
      categorial_anno <- categorial_anno()
      categorial_anno$ID <- as.character(categorial_anno$ID)
      categorial_anno$annotation <- as.character(categorial_anno$annotation)
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
               color = "orange",
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
               color = "orange",
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
      paste("MyData1.txt", sep = ",")
    },
    content = function(file) {
      write.csv(processed_data(), file)
    }
  )
#################################################################################
######### Quality Metrics ############################
output$qqPlot <- renderPlot({
  processed_data <- processed_data()
  anno_data <- anno_data()
  GeneNames <- processed_data$GeneNames
  processed_data$GeneNames <- NULL
  colnames(processed_data) <- anno_data$ID
  normalDistro <- rnorm(n = nrow(processed_data))
  for (i in 1:ncol(processed_data)) {
    
    ylabname <- colnames(processed_data[i])
    print(ylabname)
   p <- qqnorm(y = processed_data[[i]], ylab = ylabname)
   return(p)
  }
  
})
  

} #server close

shinyApp(ui, server)
