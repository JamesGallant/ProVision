#shiny being unreasonable
#library(BiocManager)
#options(repos = BiocManager::repositories())
#Libraries we need

#Modal doesnt load when tut is loaded, need to rename files for it to work.
#Double check the javascript


require(shinydashboard)
require(shiny)
library(tidyr)
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
require(ggrepel)
require(pheatmap)
require(rJava)
require(zip)
require(xlsx)
require(WebGestaltR)
require(shinycssloaders)
require(httr)


#user interface starts here
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "ProVision"),
  dashboardSidebar(useShinyjs(),
                   useSweetAlert(),
                   #welcome side bar menu
                   conditionalPanel(condition = "input.main_tabs == 'welcome'",
                                    sidebarMenu(
                                     #menuitems
                                      menuItem("Tutorials",startExpanded = TRUE,
                                               radioButtons(inputId = "tutOptions",
                                                            label = "choose tutorial options",
                                                            choices = c("Quick start" = "quickStart",
                                                                        "Full" = "full"),
                                                            selected = "quickStart"),
                                               disabled(
                                                 radioButtons(inputId = "fullTutPages",
                                                              label = "Load tutorial pages",
                                                              choices = c("Getting started" = "gs",
                                                                          "Data processing" = "dp",
                                                                          "Quality metrics" = "qm",
                                                                          "Statistics" = "stats",
                                                                          "Main figures" = "mf",
                                                                          "Export" = "exp"),
                                                              selected = "gs")
                                               ),
                                               actionButton(inputId = "goTut",
                                                            icon = icon("play-circle"),
                                                            label = "Go",
                                                            style = "width:200px")), 
                                      menuItem("Citation", 
                                               startExpanded = T,
                                               radioButtons(inputId = "citation_options",
                                                            label = "Cite this software and it's dependencies",
                                                            choices = c("All",
                                                                        "ProVision" = "Provision",
                                                                        "Limma",
                                                                        "Webgestalt",
                                                                        "StringDB"),
                                                            selected = "All"),
                                               downloadButton(outputId = "citation_download", 
                                                              label = "Download",
                                                              style="display: block; margin: 0 auto; width: 200px;color: black;")))),
                   #data handling sidebar menu
                   conditionalPanel(condition = "input.main_tabs == 'data_handling'",
                                    sidebarMenu(
                                      actionButton(inputId = "DataProcTut",
                                                   label = "Show tutorial",
                                                   width = 200,
                                                   icon = icon("graduation-cap")),
                                      #this is where we handle file uploadinh
                                      menuItem("Upload your files", tabName = "file_upload", 
                                               icon = icon("upload"),
                                               fileInput("user_file", "Choose your file",
                                                         accept = c("text/csv",
                                                                    "text/comma-separated-value",
                                                                    ".txt")),
                                               radioButtons(inputId = "userQuants",
                                                            label = "Choose quantification",
                                                            choices = c("Intensity" = "intensity",
                                                                        "LFQ intensity" = "lfq",
                                                                        "TMT tags" = "tmt"),
                                                            selected = "lfq"),
                                               uiOutput("TMTplexUI"),
                                               actionButton(inputId = "fileReset",
                                                            label = "Reset all data",
                                                            width = 200,
                                                            icon = icon("redo"))),
                                      #Filtering comes here
                                      menuItem("Filter and Transform", tabName = "file_filter",
                                               icon = icon("filter"),
                                               prettySwitch(
                                                 inputId = "unlockCols",
                                                 label = "Enable column editing", 
                                                 status = "primary",
                                                 slim = TRUE),
                                               uiOutput("addRemCols"),
                                               #filtering buttons
                                               numericInput(inputId = "user_unique_pep",
                                                            value = 2, 
                                                            label = "Minimum unique peptides",
                                                            min = 1),
                                               checkboxInput(inputId = "logTransform", 
                                                             label = "Log2 transform data", 
                                                             value = TRUE),
                                               checkboxInput(inputId = "median_center",
                                                             label = "Subtract the median",
                                                             width = 200,
                                                             value = FALSE),
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
                                               img(src = 'media/annotations.png', width = '100%'),
                                               div(style = "text-align:center", "once done renaming press",
                                                   br(), "submit to lock in the annotation"),
                                               actionButton(inputId = "submit_anno",
                                                            label = "Submit",
                                                            icon = icon("running"),
                                                            width = 200)),
                                      menuItem("Filter valid values",
                                               icon = icon("filter"),
                                               numericInput(inputId = "min_val_user", 
                                                            label = "Minimum values per replicate", 
                                                            value = 2,
                                                            min = 1
                                               ),
                                               radioButtons(inputId = "in_one_user",
                                                            label = "Filter valid values",
                                                            choices = c("In at least one group" = "one_group",
                                                                        "In each group"= "each_group",
                                                                        "Remove all missing values" = "entire_df"), 
                                                            selected = "one_group"),
                                               actionButton(inputId = "filter_valids",
                                                            label = "filter",
                                                            icon = icon("filter"),
                                                            width = 200)),
                                      
                                      #imputation
                                      menuItem("Impute missing values",
                                               icon = icon("cogs"),
                                               br(),
                                               div(style = "text-align:left", 
                                                   tags$b("Control imputation metrics"), br(),
                                                   "enable at own risk"),
                                               switchInput(inputId = "EnableImputeControl",
                                                           onLabel = "Yes", offLabel = "No",
                                                           onStatus = "danger", offStatus = "primary",
                                                           value = FALSE),
                                               disabled(numericInput(inputId = "imputeWidth",
                                                                     label = "Select width", 
                                                                     value = 0.3),
                                                        numericInput(inputId = "imputeDS",
                                                                     label = "Select downshift", 
                                                                     value = 1.8)),
                                               actionButton(inputId = "start_imputation",
                                                            label = "Start imputing",
                                                            width = 200,
                                                            icon = icon("play-circle"))),
                                      menuItem("Download tables",
                                               icon = icon("download"),
                                               actionButton(inputId = "tut_dataPreProcExport",
                                                            label = "Show tutorial",
                                                            width = 200,
                                                            icon = icon("graduation-cap")),
                                               textInput(inputId = "ProcDataDownName",
                                                         label = "File name",
                                                         placeholder = "My awesome data"),
                                               radioButtons(inputId = "ProcDataDownType",
                                                            label = "File type",
                                                            choices = c("Excel" = "xlsx",
                                                                        "Text" = "txt"),
                                                            selected = "xlsx"),
                                               uiOutput("ProcDataSelectorUI"),
                                               downloadButton(outputId = "filt1_download",
                                                              label = "Download",
                                                              style="display: block; margin: 0 auto; width: 200px;color: black;")))),
                   #qualityMetrics sidebar
                   conditionalPanel(condition = "input.main_tabs == 'quality_metrics'",
                                    sidebarMenu(
                                      actionButton(inputId = "tut_QC",
                                                   label = "Show tutorial",
                                                   width = 200,
                                                   icon = icon("graduation-cap")),
                                      #QQ-plots
                                      menuItem("Normality plots",
                                               icon = icon("line-chart"),
                                               actionButton(inputId = "normRender",
                                                            label = "Render Plot",
                                                            icon = icon("play-circle"),
                                                            style ="display: block; margin: 0 auto; width: 200px;color: black;"),
                                               br(),
                                               div(style = "text-align:center; color: white, ", 
                                                   tags$b("Cycle through plots")),
                                               disabled(actionButton(inputId = "normPrevious",
                                                                     label = "Previous",
                                                                     icon = icon("backward"),
                                                                     style="display:inline-block;width:40%;text-align: center;"),
                                                        actionButton(inputId = "normNext",
                                                                     label = "Next",
                                                                     icon = icon("forward"),
                                                                     style="display:inline-block;width:40%;text-align: center;")),
                                               br(),
                                               radioButtons(inputId = "normPlotChoice",
                                                            label = "Choose plot type",
                                                            choices = c("Q-Q plot" = "qqPlot", 
                                                                        "Histogram" = "histogram"), 
                                                            selected = "qqPlot", inline = TRUE),
                                               colourInput(inputId = "normPlotCol",
                                                           label = "Choose border colour",
                                                           showColour = "both",
                                                           palette = "limited",
                                                           value = "#000000"),
                                               colourInput(inputId = "normPlotFill",
                                                           label = "Choose fill colour",
                                                           showColour = "both",
                                                           palette = "limited",
                                                           value = "#666666"),
                                               uiOutput("normality_control1"),
                                               uiOutput("normality_control2"),
                                               uiOutput("normality_control3"),
                                               uiOutput("normality_control4"),
                                               uiOutput("normality_control5"),
                                               br(),
                                               menuItem(text = "Downloads",
                                                        icon = icon("download"),
                                                        radioButtons(inputId = "normFigDownChoice",
                                                                     label = "Download:",
                                                                     choices = c("All", "Current"),
                                                                     selected = "Current", inline = TRUE),
                                                        selectInput(inputId = "normFigDownType",
                                                                    label = "Filetype",
                                                                    choices = c("tiff", "jpeg",
                                                                                "png", "pdf"),
                                                                    selected = "tiff"),
                                                        selectInput(inputId = "normFigRes",
                                                                    label = "Figure resolution",
                                                                    choices = c("High" = "retina",
                                                                                "Medium" = "print",
                                                                                "low" = "screen"),
                                                                    selected = "300"),
                                                        downloadButton(outputId = "normFigDownload", 
                                                                       label = "download", 
                                                                       style="display: block; margin: 0 auto; width: 200px;color: black;"))
                                      ),
                                      #scatter plots
                                      menuItem("Scatter plots",
                                               icon = icon("line-chart"),
                                               actionButton(inputId = "scatRender",
                                                            label = "Render Plot",
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
                                               textInput(inputId = "scatTitle", 
                                                         label = "Label plot",
                                                         value = "",
                                                         placeholder = "My scatter plot"),
                                               colourInput(inputId = "scat_point_col",
                                                           label = "plot point color", showColour = "both",
                                                           palette = "limited",
                                                           value = "#000000"),
                                               sliderInput(inputId = "scatPlotAlphaChannel", 
                                                           label = "Change transparency",
                                                           min = 0.1, max = 1, step = 0.1, 
                                                           value = 1),
                                               sliderInput(inputId = "scatPointSize", 
                                                           label = "Point size",
                                                           min = 0.5, max = 5, step = 0.5, 
                                                           value = 1),
                                               radioButtons(inputId = "scatCharcters",
                                                            label = "Select point type",
                                                            choices = c("Circle" = 21,
                                                                        "Square" = 22,
                                                                        "Diamond"= 23,
                                                                        "Triangle 1" = 24,
                                                                        "Triangle2" = 25),
                                                            selected = 21),
                                               menuItem(text = "Downloads",
                                                        icon = icon("download"),
                                                        radioButtons(inputId = "scatFigDownChoice",
                                                                     label = "Download:",
                                                                     choices = c("All", "Current"),
                                                                     selected = "Current", inline = TRUE),
                                                        selectInput(inputId = "scatFigDownType",
                                                                    label = "Filetype",
                                                                    choices = c("tiff", "jpeg",
                                                                                "png", "pdf"),
                                                                    selected = "tiff"),
                                                        selectInput(inputId = "scatFigRes",
                                                                    label = "Figure resolution",
                                                                    choices = c("High" = "retina",
                                                                                "Medium" = "print",
                                                                                "low" = "screen"),
                                                                    selected = ""),
                                                        downloadButton(outputId = "scatFigDownload", 
                                                                       label = "download", 
                                                                       style="display: block; margin: 0 auto; width: 200px;color: black;"))),
                                      menuItem("Correllelogram",
                                               icon = icon("line-chart"),
                                               actionButton(inputId = "corrRender",
                                                            label = "Render Plot",
                                                            icon = icon("play-circle"),
                                                            style ="display: block; margin: 0 auto; width: 200px;color: black;"),
                                               textInput(inputId = "corrPlotTitle", label = "Enter plot title"),
                                               checkboxInput(inputId = "corrValDisp", 
                                                             label = "Show correlation value",
                                                             value = TRUE ),
                                               radioButtons(inputId = "corrColChoice", 
                                                            label = "Choose correlation colour gradient",
                                                            choices = c("Red-Yellow-Blue" = "RdYlBu",
                                                                        "Red-Blue" = "RdBu",
                                                                        "Purple-Orange" = "PuOr",
                                                                        "Purple-Green" = "PRGn",
                                                                        "Purple-Yellow-Green" = "PiYG", 
                                                                        "Brown-Blue-Green" = "BrBG"),
                                                            selected = "RdYlBu"),
                                               radioButtons(inputId = "correlation_test",
                                                            label = "Correlation test", 
                                                            choices = c("Pearson" = "pearson",
                                                                        "Spearman" = "spearman"), 
                                                            selected = "pearson", 
                                                            inline = TRUE),
                                               sliderInput(inputId = "corrSlider", 
                                                           label = "Set lower correlation limit",
                                                           min = 0, max = 1, step = 0.05, value = 0),
                                               menuItem(text = "Downloads",
                                                        icon = icon("download"),
                                                        selectInput(inputId = "corrFigDownType",
                                                                    label = "Filetype",
                                                                    choices = c("tiff", "jpeg",
                                                                                "png", "pdf"),
                                                                    selected = "tiff"),
                                                        selectInput(inputId = "corrFigRes",
                                                                    label = "Figure resolution",
                                                                    choices = c("High" = "retina",
                                                                                "Medium" = "print",
                                                                                "low" = "screen"),
                                                                    selected = ""),
                                                        downloadButton(outputId = "corrFigDownload", 
                                                                       label = "download", 
                                                                       style="display: block; margin: 0 auto; width: 200px;color: black;"))),
                                      #Principle componet analysis
                                      menuItem("PCA plots",
                                               icon = icon("line-chart"),
                                               actionButton(inputId = "pcaRender",
                                                            label = "Render Plot",
                                                            icon = icon("play-circle"),
                                                            style ="display: block; margin: 0 auto; width: 200px;color: black;"),
                                               textInput(inputId = "pcaPlotTitle", label = "Enter plot title"),
                                               checkboxInput(inputId = "pcaZscore",
                                                             label = "Z-score data", 
                                                             value = TRUE),
                                               sliderInput(inputId = "pcaPlotPointSize", label = "change point size",
                                                           min = 1, max = 10, step = 1,
                                                           value = 4),
                                               menuItem(text = div(style = "text-align:left; color: white, ", 
                                                                   tags$b("Colour by group")),
                                                        icon = icon("swatchbook"),
                                                        uiOutput("pcaColChoice")),
                                               sliderInput(inputId = "pcaAlphaChannel", label = "Change transparency",
                                                           min = 0.1, max = 1, step = 0.1, value = 1),
                                               menuItem(text = "Downloads",
                                                        icon = icon("download"),
                                                        selectInput(inputId = "pcaFigDownType",
                                                                    label = "Filetype",
                                                                    choices = c("tiff", "jpeg",
                                                                                "png", "pdf"),
                                                                    selected = "tiff"),
                                                        selectInput(inputId = "pcaFigRes",
                                                                    label = "Figure resolution",
                                                                    choices = c("High" = "retina",
                                                                                "Medium" = "print",
                                                                                "low" = "screen"),
                                                                    selected = ""),
                                                        downloadButton(outputId = "pcaFigDownload", 
                                                                       label = "download", 
                                                                       style="display: block; margin: 0 auto; width: 200px;color: black;")))#PCA close
                                    )
                   ), #QC metrics close
                   #statistics sidebar
                   conditionalPanel(condition = "input.main_tabs == 'statistics'",
                                    sidebarMenu(
                                      actionButton(inputId = "tut_stats",
                                                   label = "Show tutorial",
                                                   width = 200,
                                                   icon = icon("graduation-cap")),
                                      menuItem(text = "Analyse data",
                                               icon = icon("calculator"),
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
                                                                     style="display:inline-block;width:40%;text-align: center;"))),
                                      menuItem("Download tables",
                                               icon = icon("download"),
                                               actionButton(inputId = "tut_statsExport",
                                                            label = "Show tutorial",
                                                            width = 200,
                                                            icon = icon("graduation-cap")),
                                               textInput(inputId = "SigDataDownName",
                                                         label = "File name",
                                                         placeholder = "My awesome data"),
                                               radioButtons(inputId = "WhichSigDataDown",
                                                            label = "Download:",
                                                            choices = c("All comparisons" = "all",
                                                                        "Current comparison" = "current"),
                                                            selected = "current"),
                                               radioButtons(inputId = "SigDataDownType",
                                                            label = "File type",
                                                            choices = c("Excel" = "xlsx",
                                                                        "Text" = "txt"),
                                                            selected = "xlsx"),
                                               uiOutput("SigDataSelectorUI"),
                                               downloadButton(outputId = "SigDownload",
                                                              label = "Download",
                                                              style="display: block; margin: 0 auto; width: 200px;color: black;"))
                                    )
                   ),
                   #figure construction sidebar
                   conditionalPanel(condition = "input.main_tabs == 'figures'",
                                    sidebarMenu(
                                      actionButton(inputId = "tut_mainFigs",
                                                   label = "Show tutorial",
                                                   width = 200,
                                                   icon = icon("graduation-cap")),
                                      menuItem("Volcano plots",
                                               icon = icon("chart-line"),
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
                                               menuItem(text = div(style = "text-align:left; color: white, ", 
                                                                   tags$b("Alter point characteristics")),
                                                        icon = icon("swatchbook"),
                                                        sliderInput(inputId = "volcPlotPointSize", label = "change point size",
                                                                    min = 1, max = 10, step = 1,
                                                                    value = 3),
                                                        sliderInput(inputId = "volcAlphaChannel", label = "Change transparency",
                                                                    min = 0.1, max = 1, step = 0.1, value = 1),
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
                                               menuItem(text = div(style = "text-align:left; color: white, ", 
                                                                   tags$b("Label significant proteins")),
                                                        icon = icon("tags"),
                                                        radioButtons(inputId = "volcSigLabels", 
                                                                     label = "Display labels", 
                                                                     choices = c("None", "Upregulated", "Downregulated", "Both"), 
                                                                     selected = "None"),
                                                        sliderInput(inputId = "volclabelNoOfSig",
                                                                    label = "Label top signficantly regulated proteins",
                                                                    min = 1, max = 30, step = 1, value = 0)),
                                               menuItem(text = div(style = "text-align:left; color: white, ", 
                                                                   tags$b("Plot title control")),
                                                        icon = icon("heading"),
                                                        textInput(inputId = "volcTitle",
                                                                  label = "Enter plot title",
                                                                  value = ""),
                                                        sliderInput(inputId = "volcTitlePos",
                                                                    label = "Set title position",
                                                                    min = 0, max = 1, step = 0.1,
                                                                    value = 0),
                                                        radioButtons(inputId = "VolcTitleFace",
                                                                     label = "Title face",
                                                                     choices = c("plain", "bold",
                                                                                 "italic", "bold.italic"))),
                                               menuItem(text = div(style = "text-align:left; color: white, ", 
                                                                   tags$b("Alter plot characteristics")),
                                                        icon = icon("gears"),
                                                        radioButtons(inputId = "volcLegendPostition", label = "Legend position",
                                                                     choices = c("right", "left", "top", "bottom", "none"), 
                                                                     selected = "top"),
                                                        radioButtons(inputId = "volcFeatures",
                                                                     label = "display:", 
                                                                     choices = c("Lines", "Counts", "Both", "None"),
                                                                     selected = "Both"))
                                      ),
                                      menuItem("Heatmaps",
                                               icon = icon('chart-line'),
                                               actionButton(inputId = "generateHM",
                                                            label = "Render Heatmap",
                                                            icon = icon("play-circle"),
                                                            style ="display: block; margin: 0 auto; width: 200px;color: black;"),
                                               br(),
                                               radioButtons(inputId = "HMAllorSig",
                                                            label = "Plot:",
                                                            choices = c("All proteins" = "All", "Significant proteins" = "Sig"),
                                                            selected = "Sig"),
                                               div(style = "text: align-left; color: white,", tags$b("Current comparison")),
                                               uiOutput("HMComparisonUI"),
                                               br(),
                                               disabled(actionButton(inputId = "HMCyclePrevious",
                                                                     label = "Previous",
                                                                     icon = icon("backward"),
                                                                     style="display:inline-block;width:40%;text-align: center;"),
                                                        actionButton(inputId = "HMCycleNext",
                                                                     label = "Next",
                                                                     icon = icon("forward"),
                                                                     style="display:inline-block;width:40%;text-align: center;")),
                                               menuItem(text = div(style = "text-align:left; color: white, ", 
                                                                   tags$b("Alter plot characteristics")),
                                                        icon = icon("tags"),
                                                        radioButtons(inputId = "HMdata",
                                                                     label = "Switch between data type",
                                                                     choices = c("Plot averages" = "averages",
                                                                                 "Plot replicates" = "reps"),
                                                                     selected = "reps"),
                                                        checkboxInput(inputId = "HMDispCol",
                                                                      label = "Show column label",
                                                                      value = TRUE),
                                                        checkboxInput(inputId = "HMDispRow",
                                                                      label = "Show row labels",
                                                                      value = TRUE),
                                                        radioButtons(inputId = "HMSigLabels", 
                                                                     label = "Isolate significant proteins", 
                                                                     choices = c("Upregulated", "Downregulated", "Both"), 
                                                                     selected = "Upregulated"),
                                                        checkboxInput(inputId = "HMzScore",
                                                                      label = "Z-score data", value = FALSE),
                                                        numericInput(inputId = "HMlabelNoOfSig",
                                                                     label = "No. of regulated proteins to display",
                                                                     min = 1, step = 1, value = 10)),
                                               menuItem(text = div(style = "text-align:left; color: white, ", 
                                                                   tags$b("Change colour scheme")),
                                                        icon = icon("swatchbook"),
                                                        radioButtons(inputId = "HMColChoice", 
                                                                     label = "Choose correlation colour gradient",
                                                                     choices = c("Red-Yellow-Blue" = "RdYlBu",
                                                                                 "Red-Blue" = "RdBu",
                                                                                 "Purple-Orange" = "PuOr",
                                                                                 "Purple-Green" = "PRGn",
                                                                                 "Purple-Yellow-Green" = "PiYG", 
                                                                                 "Brown-Blue-Green" = "BrBG"),
                                                                     selected = "RdYlBu"),
                                                        sliderInput(inputId = "HMcolScale",
                                                                    label = "Choose colour scale",
                                                                    min = 1, max = 11, step = 1, value = 11),
                                                        colourInput(inputId = "HMborderCol",
                                                                    label = "Border colour", showColour = "both",
                                                                    palette = "limited",
                                                                    value = "#000000")),
                                               menuItem(text = div(style = "text-align:left; color: white, ", 
                                                                   tags$b("Correlation")),
                                                        icon = icon("calculator"),
                                                        checkboxInput(inputId = "HMclustCols",
                                                                      label = "cluster columns", 
                                                                      value = TRUE),
                                                        checkboxInput(inputId = "HMclustRows",
                                                                      label = "cluster rows", 
                                                                      value = TRUE),
                                                        pickerInput(inputId = "HMClustMethod",
                                                                    label = "Clustering method", 
                                                                    choices = c("complete", "ward.D", "ward.D2",
                                                                                "single", "average", "median", 
                                                                                "centroid"),
                                                                    multiple = FALSE, 
                                                                    selected = "complete",
                                                                    choicesOpt = list(
                                                                      style = rep(("color: black;"),7)))
                                               )
                                      ),
                                      #Webgestalt controls
                                      menuItem(text = "Webgestalt enrichments",
                                               icon = icon("bezier-curve"),
                                               actionButton(inputId = "generateEnrichments",
                                                            label = "start",
                                                            icon = icon("play-circle"),
                                                            style ="display: block; margin: 0 auto; width: 200px;color: black;"),
                                               br(),
                                               div(style = "text: align-left; color: white,", tags$b("Current comparison")),
                                               verbatimTextOutput(outputId = "enrichmet_currentCompareText"),
                                               br(),
                                               disabled(actionButton(inputId = "enrichmentCyclePrevious",
                                                                     label = "Previous",
                                                                     icon = icon("backward"),
                                                                     style="display:inline-block;width:40%;text-align: center;"),
                                                        actionButton(inputId = "enrichmentCycleNext",
                                                                     label = "Next",
                                                                     icon = icon("forward"),
                                                                     style="display:inline-block;width:40%;text-align: center;")),
                                               br(),
                                               pickerInput(inputId = "webgesalt_orgs",
                                                           label = "Choose model organism", 
                                                           choices =  listOrganism(),
                                                           multiple = FALSE, 
                                                           selected = "hsapiens",
                                                           choicesOpt = list(
                                                             style = rep(("color: black;"),12))),
                                               radioButtons(inputId = "webgestalt_tests",
                                                            label = "Choose a test",
                                                            choices = c("ORA", "GSEA"),
                                                            inline = TRUE,
                                                            selected = "ORA"),
                                               uiOutput("webgestalt_yaxis_render"),
                                               uiOutput("webgestalt_enrichment_variations_render"),
                                               radioButtons(inputId = "webgestalt_db",
                                                            label = "Choose querry database",
                                                            choices = c("Gene onthology" = "gene_onthology",
                                                                        "Pathway" = "pathway"),
                                                            selected = "pathway"),
                                               uiOutput("webgestalt_function"),
                                               uiOutput("webgestalt_fdr_render"),
                                               uiOutput("top_enrichment_slider"),
                                               uiOutput("webgestalt_plotting_slider"),
                                               uiOutput("webgestalt_sig_slider"),
                                               uiOutput("webgestalt_fdr_options_render"),
                                               actionButton(inputId = "calculateEnrichments",
                                                            label = "Calculate",
                                                            icon = icon("play-circle"),
                                                            style ="display: block; margin: 0 auto; width: 200px;color: black;")),
                                      #string
                                      menuItem(text = "String controls",
                                               icon = icon("atom"),
                                               actionButton(inputId = "generateStringNetwork",
                                                            label = "start",
                                                            icon = icon("play-circle"),
                                                            style ="display: block; margin: 0 auto; width: 200px;color: black;"),
                                               br(),
                                               div(style = "text: align-left; color: white,", tags$b("Current comparison")),
                                               verbatimTextOutput(outputId = "string_currentCompareText"),
                                               br(),
                                               disabled(actionButton(inputId = "stringCyclePrevious",
                                                                     label = "Previous",
                                                                     icon = icon("backward"),
                                                                     style="display:inline-block;width:40%;text-align: center;"),
                                                        actionButton(inputId = "stringCycleNext",
                                                                     label = "Next",
                                                                     icon = icon("forward"),
                                                                     style="display:inline-block;width:40%;text-align: center;")),
                                               br(),
                                               pickerInput(inputId = "string_data_options",
                                                           label = "Choose data set", 
                                                           c("Upregulated" = "upregulated",
                                                             "Downregulated" = "downregulated", 
                                                             "Custom Uniprot ID" = "custom"),
                                                           selected = "upgegulated",
                                                           multiple = FALSE,
                                                           choicesOpt = list(
                                                             style = rep(("color: black;"),3))),
                                               uiOutput("string_data_inputs"),
                                               numericInput(inputId = "string_sig_threshold",
                                                            label = "siginifcance threshold",
                                                            min = 1, max = 1000, 
                                                            value = 950),
                                               numericInput(inputId = "string_max_nodes",
                                                            label = "max nodes",
                                                            min = 1, max = 100, 
                                                            value = 10)
                                               
                                               ), # string menu close
                                      menuItem(text = "Download options",
                                               icon = icon("download"),
                                               textInput(inputId = "mainFigDownTitle",
                                                         label = "file name",
                                                         placeholder = "Current comparison"),
                                               selectInput(inputId = "mainFigDownType",
                                                           label = "Filetype",
                                                           choices = c("tiff", "jpeg",
                                                                       "png", "pdf"),
                                                           selected = "tiff"),
                                               selectInput(inputId = "mainFigRes",
                                                           label = "Figure resolution",
                                                           choices = c("High" = "retina",
                                                                       "Medium" = "print",
                                                                       "low" = "screen"),
                                                           selected = "")))
                   ), #figures close
                   #about us panel
                   conditionalPanel(condition = "input.main_tabs == 'AboutUs'",
                                    sidebarMenu(
                                      
                                    ) #sidebar close
                                    
                   ) #about us close
  ), #sidebar close
  
  dashboardBody(useShinyjs(),
                tags$head(includeHTML("analytics.html")),
                tags$head(tags$script(HTML("
                    // Enable navigation prompt
                    window.onbeforeunload = function() {
                        return 'Carefull, your changes will be lost!';
                    };
                "))),
                tabsetPanel(id = "main_tabs",
                            #welcome tab/about tab
                            tabPanel(title = "Welcome",
                                     value = "welcome",
                                     icon = icon("door-open"),
                                     uiOutput("welcomeHTML")),
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
                                                dropdownButton(circle = TRUE,
                                                               status = "primary", 
                                                               tooltip = TRUE,
                                                               icon = icon("gears"),
                                                               label = "Click for more plotting options",
                                                               h4(tags$b("Q-Q plot options")),
                                                               colourInput(inputId = "qqLineCol",
                                                                           label = "Line colour",
                                                                           palette = "limited",
                                                                           value = "#000000"),
                                                               sliderInput(inputId = "qqLineWidth",
                                                                           label = "Line width",
                                                                           min = 0.1, max = 2, step = 0.1,
                                                                           value = 1),
                                                               radioButtons(inputId = "qqLinesType",
                                                                            label = "Choose line type",
                                                                            choices = c("solid", "dashed", 
                                                                                        "dotted", "dotdash", 
                                                                                        "longdash", "twodash"),
                                                                            selected = "solid", inline = TRUE),
                                                               h4(tags$b("Q-Q and histogram options")),
                                                               sliderInput(inputId = "normTitlePos",
                                                                           label = "Set title position",
                                                                           min = 0, max = 1, step = 0.1,
                                                                           value = 0),
                                                               radioButtons(inputId = "normTitleFace",
                                                                            label = "Title face",
                                                                            choices = c("plain", "bold",
                                                                                        "italic", "bold.italic"),
                                                                            selected = "plain",
                                                                            inline = TRUE),
                                                               h4(tags$b("Font sizes")),
                                                               fluidRow(column(4,
                                                                               numericInput(inputId = "normXsize",
                                                                                            label = "X-axis title",
                                                                                            min = 1, max = 30, step = 1,
                                                                                            value = 12)),
                                                                        column(4,
                                                                               numericInput(inputId = "normYsize",
                                                                                            label = "Y-axis title",
                                                                                            min = 1, max = 30, step = 1,
                                                                                            value = 12)),
                                                                        column(4,
                                                                               numericInput(inputId = "normTitleSize",
                                                                                            label = "Plot title",
                                                                                            min = 1, max = 30, step = 1,
                                                                                            value = 15)))),
                                                plotOutput("qqPlot")),
                                         column(6,
                                                dropdownButton(circle = TRUE,
                                                               status = "primary", 
                                                               tooltip = TRUE,
                                                               icon = icon("gears"),
                                                               label = "Click for more plotting options",
                                                               colourInput(inputId = "scatPointBorder",
                                                                           label = "change point border colour",
                                                                           palette = "limited",
                                                                           value = "#000000"),
                                                               sliderInput(inputId = "scatTitlePos",
                                                                           label = "Set title position",
                                                                           min = 0, max = 1, step = 0.1,
                                                                           value = 0),
                                                               radioButtons(inputId = "scatTitleFace",
                                                                            label = "Title face",
                                                                            choices = c("plain", "bold",
                                                                                        "italic", "bold.italic"),
                                                                            selected = "plain"),
                                                               h4(tags$b("Font sizes")),
                                                               fluidRow(column(4,
                                                                               numericInput(inputId = "scatXsize",
                                                                                            label = "X-axis title",
                                                                                            min = 1, max = 30, step = 1,
                                                                                            value = 12)),
                                                                        column(4,
                                                                               numericInput(inputId = "scatYsize",
                                                                                            label = "Y-axis title",
                                                                                            min = 1, max = 30, step = 1,
                                                                                            value = 12)),
                                                                        column(4,
                                                                               numericInput(inputId = "scatTitleSize",
                                                                                            label = "Plot title",
                                                                                            min = 1, max = 30, step = 1,
                                                                                            value = 15)))),
                                                plotOutput("scatPlot"))),
                                       br(),
                                       #corrplot
                                       fluidRow(
                                         column(6,
                                                dropdownButton(circle = TRUE,
                                                               status = "primary",
                                                               tooltip = TRUE,
                                                               icon = icon("gears"),
                                                               label = "Click for more plotting options",
                                                               h4(tags$b("Plot title options")),
                                                               sliderInput(inputId = "corrTitlePos",
                                                                           label = "Set title position",
                                                                           min = 0, max = 1, step = 0.1,
                                                                           value = 0),
                                                               radioButtons(inputId = "corrTitleFace",
                                                                            label = "Title face",
                                                                            choices = c("plain", "bold",
                                                                                        "italic", "bold.italic"),
                                                                            selected = "plain", inline = TRUE),
                                                               h4(tags$b("plot aesthetics")),
                                                               numericInput(inputId = "corrDecimalPos", 
                                                                            label = "Correlation decimal positions", 
                                                                            value = 2, min = 1),
                                                               sliderInput(inputId = "corrColourScale",
                                                                           label = "Set colour scale",
                                                                           min = 3, max = 11, step = 1,
                                                                           value = 11),
                                                               h4(tags$b("Font sizes")),
                                                               fluidRow(column(4,
                                                                               numericInput(inputId = "corrXSize",
                                                                                            label = "X-axis",
                                                                                            min = 0, max = 30, step = 0.5,
                                                                                            value = 12)),
                                                                        column(4,
                                                                               numericInput(inputId = "corrYSize",
                                                                                            label = "Y-axis",
                                                                                            min = 0, max = 30, step = 0.5,
                                                                                            value = 12)),
                                                                        column(4,
                                                                               numericInput(inputId = "corrTitleSize",
                                                                                            label = "Plot title",
                                                                                            min = 0, max = 30, step = 0.5,
                                                                                            value = 15)))
                                                ), #dropdownclose
                                                plotOutput("corrPlot")),
                                         column(6,
                                                dropdownButton(circle = TRUE,
                                                               status = "primary",
                                                               tooltip = TRUE,
                                                               icon = icon("gears"),
                                                               label = "Click for more plotting options",
                                                               h4(tags$b("Plot aesthetics")),
                                                               radioButtons(inputId = "pcaLegendPostition", label = "Position legend",
                                                                            choices = c("right", "left", "top", "bottom", "none"), 
                                                                            selected = "top", inline = TRUE),
                                                               radioButtons(inputId = "pcaCharcters",
                                                                            label = "Select point type",
                                                                            choices = c("Circle" = 21,
                                                                                        "Square" = 22,
                                                                                        "Diamond"= 23,
                                                                                        "Triangle 1" = 24,
                                                                                        "Triangle2" = 25),
                                                                            selected = 21, inline = TRUE),
                                                               h4(tags$b("Plot title options")),
                                                               sliderInput(inputId = "pcaTitlePos",
                                                                           label = "Set title position",
                                                                           min = 0, max = 1, step = 0.1,
                                                                           value = 0),
                                                               radioButtons(inputId = "pcaTitleFace",
                                                                            label = "Title face",
                                                                            choices = c("plain", "bold",
                                                                                        "italic", "bold.italic"),
                                                                            selected = "plain", inline = TRUE),
                                                               h4(tags$b("Font sizes")),
                                                               fluidRow(column(4,
                                                                               numericInput(inputId = "pcaXSize",
                                                                                            label = "X-axis",
                                                                                            min = 0, max = 30, step = 0.5,
                                                                                            value = 12)),
                                                                        column(4,
                                                                               numericInput(inputId = "pcaYSize",
                                                                                            label = "Y-axis",
                                                                                            min = 0, max = 30, step = 0.5,
                                                                                            value = 12)),
                                                                        column(4,
                                                                               numericInput(inputId = "pcaTitleSize",
                                                                                            label = "Plot title",
                                                                                            min = 0, max = 30, step = 0.5,
                                                                                            value = 15)))),
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
                                     fluidPage(
                                       fluidRow(column(6,
                                                       dropdownButton(circle = TRUE,
                                                                      status = "primary", 
                                                                      tooltip = TRUE,
                                                                      icon = icon("gears"),
                                                                      label = "Click for more plotting options",
                                                                      sliderInput(inputId = "volcLinesLWD", 
                                                                                  label = "Set line width",
                                                                                  min = 0, max = 5, step = 0.1, value = 1.4),
                                                                      radioButtons(inputId = "volcLinesType",
                                                                                   label = "Choose line type",
                                                                                   choices = c("solid", "dashed", 
                                                                                               "dotted", "dotdash", 
                                                                                               "longdash", "twodash"),
                                                                                   selected = "longdash", inline = TRUE),
                                                                      h4(tags$b("Font sizes")),
                                                                      fluidRow(column(4,
                                                                                      numericInput(inputId = "VolcXSize",
                                                                                                   label = "X-axis",
                                                                                                   min = 0, max = 30, step = 0.5,
                                                                                                   value = 12)),
                                                                               column(4,
                                                                                      numericInput(inputId = "VolcYSize",
                                                                                                   label = "Y-axis",
                                                                                                   min = 0, max = 30, step = 0.5,
                                                                                                   value = 12)),
                                                                               column(4,
                                                                                      numericInput(inputId = "VolcTitleSize",
                                                                                                   label = "Plot title",
                                                                                                   min = 0, max = 30, step = 0.5,
                                                                                                   value = 15))),
                                                                      h4(tags$b("Control position of significant counts:")),
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
                                                       plotOutput("volcplotOut"),
                                                       downloadButton(outputId = "VolcDownloader", 
                                                                      label = "Download Volcano plot",
                                                                      style="color: black;")
                                       ),
                                       column(6,
                                              dropdownButton(circle = TRUE,status = "primary", tooltip = TRUE,
                                                             icon = icon("gears"),
                                                             label = "Click for more plotting options",
                                                             h4(tags$b("Font sizes")),
                                                             fluidRow(column(4,
                                                                             numericInput(inputId = "HMColFontSize",
                                                                                          label = "Column font size",
                                                                                          value = 12)),
                                                                      column(4,
                                                                             numericInput(inputId = "HMRowFontSize",
                                                                                          label = "Row font size",
                                                                                          value = 12))),
                                                             radioButtons(inputId = "HMcolAngle",
                                                                          label = "Column text angle",
                                                                          choices = c(0, 45, 90, 270, 315),
                                                                          selected = 90, inline = TRUE),
                                                             sliderInput(inputId = "HMColTreeHeight",
                                                                         label = "Decrease column dendogram height",
                                                                         min = 0, max = 50, step = 2, value = 50),
                                                             sliderInput(inputId = "HMRowTreeHeight",
                                                                         label = "Decrease row dendogram height",
                                                                         min = 0, max = 50, step = 2, value = 50)),
                                              tags$head(tags$style(".shiny-output-error{color: black;}")),
                                              plotOutput("Heatmap"),
                                              downloadButton(outputId = "HMDownloader",
                                                             label = "Download Heatmap",
                                                             style="color: black;"))),
                                       fluidRow(column(12,
                                                       tabsetPanel(id = "enrichment_plot_tabs",
                                                                   tabPanel(title = "Webgestalt",
                                                                            fluidRow(
                                                                              column(9,
                                                                                     plotOutput("webgestalt_plot") %>%
                                                                                       withSpinner(type = 3, color.background = '#ECEFF4')),
                                                                              column(3,
                                                                                     dropdownButton(circle = TRUE,status = "primary", tooltip = TRUE,
                                                                                                    icon = icon("gears"),
                                                                                                    label = "click for plotting options",
                                                                                         fluidPage(
                                                                                           fluidRow(
                                                                                             column(6,
                                                                                                    textInput(inputId = "webgestalt_x_label",
                                                                                                              label = "x-axis label",
                                                                                                              placeholder = "Enrichment Ratio")
                                                                                             ),
                                                                                             column(6,
                                                                                                    textInput(inputId = "webgestalt_y_label",
                                                                                                              label = "y-axis label",
                                                                                                              placeholder = "Description")
                                                                                             ),
                                                                                             fluidRow(
                                                                                               div(style = "margin-left:15px;
                                                                                                            margin-right:15px;",
                                                                                                   column(3,
                                                                                                          numericInput(inputId = "enrichment_x_axis_font_size",
                                                                                                                       label = "X-axis size",
                                                                                                                       min = 1, max = 30,
                                                                                                                       value = 10)),
                                                                                                   column(3,
                                                                                                          numericInput(inputId = "enrichment_y_axis_font_size",
                                                                                                                       label = "Y-axis size",
                                                                                                                       min = 1, max = 30, 
                                                                                                                       value = 8)),
                                                                                                   column(3,
                                                                                                          numericInput(inputId = "enrichment_x_title_font_size",
                                                                                                                       label = "Y-title size",
                                                                                                                       min = 1, max = 30, 
                                                                                                                       value = 16)),
                                                                                                   column(3,
                                                                                                          numericInput(inputId = "enrichment_y_title_font_size",
                                                                                                                       label = "X-axis size",
                                                                                                                       min = 1, max = 30, 
                                                                                                                       value = 16)))
                                                                                               
                                                                                             ),
                                                                                             fluidRow(column(6,
                                                                                                             div(style = "margin-left:10px;",
                                                                                                                 colourInput(inputId = "webgestalt_colour_fill",
                                                                                                                             label = "fill colour",
                                                                                                                             showColour = "both",
                                                                                                                             palette = "limited",
                                                                                                                             value = "blue")
                                                                                                             )
                                                                                                             
                                                                                             ),
                                                                                             column(6,
                                                                                                    actionButton(inputId = "webgestalt_tables",
                                                                                                                 label = "Show data",
                                                                                                                 style ="display: block;
                                                                                                                    background-color:white;
                                                                                                                    margin-top: 25px;
                                                                                                                    marin-right: 15px;
                                                                                                                    width: 150px;
                                                                                                                    font-face:bold;
                                                                                                                    color: black;"))),
                                                                                             fluidRow(column(12,
                                                                                                             div(style = "margin-left:25px",
                                                                                                                 sliderInput(inputId = "webgestalt_top_n_slider",
                                                                                                                             label = "Plot top n enrichments",
                                                                                                                             min = 1, max = 30, step = 1,
                                                                                                                             value = 10,
                                                                                                                             width = 300))
                                                                                             ))))
                                                                                     )  #dropdown close
                                                                                     )
                                                                                      ),
                                                                            downloadButton(outputId = "webgestalt_downloader", 
                                                                                         label = "Download enrichment plot",
                                                                                         style="color: black;")
                                                                            ),
                                                                   tabPanel(title = "String",
                                                                            fluidRow(radioButtons(inputId = "string_image_extention",
                                                                                                  label = "", 
                                                                                                  choices = c("png" = "highres_image",
                                                                                                              "svg" = "svg"), 
                                                                                                  selected = "highres_image",
                                                                                                  inline = TRUE)),
                                                                            fluidRow(
                                                                              downloadButton(outputId = "string_download_image",
                                                                                             label = "Download",
                                                                                             style="color: black;")
                                                                            ),
                                                                            uiOutput("string_image") %>%
                                                                              withSpinner(type = 3, color.background = '#ECEFF4')
                                                                            ))
                                                       )
                                                )
                                     ) #fluidpage close
                            ),#Figs close
                            #About us tab
                            tabPanel(title = "About us",
                                     value = "AboutUs",
                                     icon = icon("user-tie"),
                                     includeHTML("www/HTML/about.html")
                            )
                          )
                
  )#Body close
) #user interface close

#server starts here
server <- function(input, output, session) {
  
  filterValidVals <- function(x, in_one, user_val, anno_data) {
    #count reps and get groups
    if (in_one == "one_group") {
      #need to keep genenames indexed
      gene.names <- x$GeneNames
      x$GeneNames <- NULL
      
      colnames(x) <- anno_data$annotation
      
      conditions <- as.data.frame(table(unlist(names(x))))
      conditions <- conditions$Var1
      
      cond.filter <- sapply(levels(conditions), function(i) {
        df2 <- x[, grepl(i, names(x))]
        counts <- rowSums(is.finite(as.matrix(df2)))
        counts >= user_val
      })
      
      x$keep = apply(cond.filter, 1, any)
      
      x$GeneNames <- gene.names
      #filter
      
      x <- x[!(x$keep=="FALSE"),]
      x$keep <- NULL
      x$geneNames <- NULL
    }
    
    if (in_one == "each_group") {
      anno_data <- anno_data()
      #need to keep genenames indexed
      gene.names <- x$GeneNames
      x$GeneNames <- NULL
      
      colnames(x) <- anno_data$annotation
      
      
      conditions <- as.data.frame(table(unlist(names(x))))
      conditions <- conditions$Var1
      
      cond.filter <- sapply(levels(conditions), function(i) {
        df2 <- x[, grepl(i, names(x))]
        counts <- rowSums(is.finite(as.matrix(df2)))
        counts >= user_val
      })
      
      x$keep = apply(cond.filter, 1, all)
      
      x$GeneNames <- gene.names
      #filter
      
      x <- x[!(x$keep=="FALSE"),]
      x$keep <- NULL
      x$geneNames <- NULL
    }
    #in matrix
    
    if (in_one == "entire_df") {
      anno_data <- anno_data()
      #need to keep genenames indexed
      gene.names <- x$GeneNames
      x$GeneNames <- NULL
      
      colnames(x) <- anno_data$annotation
      rows <- rownames(x)
      
      x = do.call(data.frame, lapply(x, function(dat) replace(dat, is.infinite(dat), NA)))
      
      rownames(x) <- rows
      colnames(x) <- anno_data$annotation
      x$GeneNames <- gene.names
      x = na.omit(x)
      
    }
    return(x)
    
  }
  ######Upload limit#####
  options(shiny.maxRequestSize=30*1024^2)
  options(shiny.sanitize.errors = FALSE)
 
  
  source(file = "www/scripts/utility_functions.R")
  
  ######Welcome tab######

  
  tab1Counters <- reactiveValues(ClickCounter = 0)
  
  observeEvent(input$goTut, {
    tab1Counters$ClickCounter <- tab1Counters$ClickCounter + 1 
  })
  
  observeEvent(input$main_tabs != 'welcome', {
    tab1Counters$ClickCounter = 0
  })
  
  observe({
    if (input$tutOptions == "full") {
      enable("fullTutPages")
    } else {
      disable("fullTutPages")
    }
  })
  
  HTMLdata <- reactive({
    if (tab1Counters$ClickCounter == 0) {
      includeHTML("www/HTML/welcome.html")
    } else if (input$tutOptions == "quickStart") {
      includeHTML("www/HTML/quickstart.html")
    } else if (input$tutOptions == "full" & input$fullTutPages == "gs") {
      includeHTML("www/HTML/gettingStarted.html")
    } else if (input$tutOptions == "full" & input$fullTutPages == "dp") {
      includeHTML("www/HTML/dataProcessing.html")
    } else if (input$tutOptions == "full" & input$fullTutPages == "qm") {
      includeHTML("www/HTML/qc.html")
    } else if (input$tutOptions == "full" & input$fullTutPages == "stats") {
      includeHTML("www/HTML/stats.html")
    } else if (input$tutOptions == "full" & input$fullTutPages == "mf") {
      includeHTML("www/HTML/mainFigs.html")
    } else if (input$tutOptions == "full" & input$fullTutPages == "exp") {
      includeHTML("www/HTML/export.html")
    } else {
      return(NULL)
    }
  })
  
  output$welcomeHTML <- renderUI({
    HTMLdata()
  })
  
  #tutModal_preprocess-------------------------------------------------------------------------------------------------->
  observeEvent(input$DataProcTut, {
    showModal(tutmodal_preprocessing())
  })
  
  tutmodal_preprocessing <- function(){
    modalDialog(includeHTML("www/HTML/dataProcessing.html"),
                size = "l", 
                easyClose = TRUE,
                fade = TRUE)
  }
  
  
  #TutModal_QC----------------------------------------------------------------------------------------------------------->
  observeEvent(input$tut_QC, {
    showModal(tutmodal_qc())
  })
  
  tutmodal_qc <- function(failed = FALSE){
    modalDialog(includeHTML("www/HTML/qc.html"),
                size = "l", 
                easyClose = TRUE,
                fade = TRUE)
  }
  
  
  #tutmodal_stats-------------------------------------------------------------------------------------------------------->

  observeEvent(input$tut_stats, {
    showModal(tutmodal_stats())
  }
  )
  
  tutmodal_stats <- function(failed = FALSE){
    modalDialog(includeHTML("www/HTML/stats.html"),
                size = "l", 
                easyClose = TRUE,
                fade = TRUE)
  }
  
  #tutmodal_mainfigs------------------------------------------------------------------------------------------------------->
  
  observeEvent(input$tut_mainFigs, {
    showModal(tutmodal_mainfigs())
  }
  )
  
  tutmodal_mainfigs <- function(failed = FALSE){
    modalDialog(includeHTML("www/HTML/mainFigs.html"),
                size = "l", 
                easyClose = TRUE,
                fade = TRUE)
  }
  
  
  #tutmodal_preprocessing_export------------------------------------------------------------------------------------------>
  
  observeEvent(input$tut_dataPreProcExport, {
    showModal(tutmodal_export_preprocessing())
  })
  
  tutmodal_export_preprocessing <- function(failed = FALSE){
    modalDialog(includeHTML("www/HTML/processedDataExportModal.html"),
                size = "l", 
                easyClose = TRUE,
                fade = TRUE)
  }
  
  #tutmodal_stats_export-------------------------------------------------------------------------------------------------->
  observeEvent(input$tut_statsExport, {
    showModal(tutmodal_export_stats())
  })
  
  tutmodal_export_stats <- function(failed = FALSE){
    modalDialog(includeHTML("www/HTML/statsDataExportModal.html"),
                size = "l", 
                easyClose = TRUE,
                fade = TRUE)
  }
  
  
  ###### DATA INPUT #####
  file_upload <- reactive({
    data <- read.csv(input$user_file$datapath, 
                     stringsAsFactors = FALSE,
                     colClasses = "character",
                     sep = "\t",
                     header = TRUE)
  
    return(data)
  })

  output$TMTplexUI <- renderUI({
    if (input$userQuants == "tmt") {
      radioButtons(inputId = "tmtPlex",
                   label = "Choose TMT plex",
                   choices = c("TMT Six plex" = 5,
                               "TMT Ten plex" = 9,
                               "TMT eleven plex" = 10,
                               "TMT sixteen plex" = 16),
                   selected = 9)
    }
  })
  
  colPickData <- reactive({
    if (!is.null(input$user_file)) {
      df <- file_upload()
      
      if (input$userQuants == "lfq") {
        intensity.names = grep("^LFQ.intensity", names(df), value = TRUE)
      } else if (input$userQuants == "intensity") {
        intensity.names = grep("^Intensity.", names(df), value = TRUE)
      } else {
        intensity.names = c()
        for (i in 0:input$tmtPlex) {
          pat <- paste0("^Reporter.intensity.corrected.", i, "$")
          tmt <- grep(pattern = pat, names(df), value = TRUE)
          intensity.names <- c(intensity.names, tmt)
        }
        
      }
      return(intensity.names)
    }
  })
  
  output$addRemCols <- renderUI({
    if (!is.null(input$user_file) & input$unlockCols == TRUE) {
      pickerInput(inputId = "RemCols",
                  label = "Remove specific columns",
                  choices = unlist(colPickData()),
                  multiple = TRUE,
                  selected = unlist(colPickData()),
                  options = list(`actions-box` = TRUE, 
                                 `selected-text-format` = "count > 0"), choicesOpt = list(
                                   style = rep(("color: black;"),length(colPickData()))))
      
    } else {return(NULL)}
  })
  
  #### Data handling ################################################################
  
  #control imputations
  observe({
    if (input$EnableImputeControl == TRUE) {
      enable("imputeWidth")
      enable("imputeDS")
    } else {
      reset("imputeWidth")
      reset("imputeDS")
      reset("median_center")
      disable("imputeWidth")
      disable("imputeDS")
    }
  })
  
  #TMT needs subtraction of the median
  observe({
    if (input$userQuants == "tmt") {
      updateCheckboxInput(session = session,
                          inputId = "median_center",
                          value = TRUE)
    } else {
      updateCheckboxInput(session = session,
                          inputId = "median_center",
                          value = FALSE)
    }
  })
  
  # control data flow allows to ignore the cached data
  dataControl = reactiveValues(activateFilter = 0,
                               uploadState = NULL,
                               filterValids = 0,
                               imputation = 0,
                               annoStart = 0,
                               annoSubmit = 0)
  
  observeEvent(input$activate_filter, {
   dataControl$activateFilter <- dataControl$activateFilter + 1
  })
  
  observeEvent(input$user_file, {
    dataControl$uploadState <- "uploaded"
  })
  
  observeEvent(input$filter_valids, {
    dataControl$filterValids <-  dataControl$filterValids + 1
  })
  
  observeEvent(input$start_imputation, {
    dataControl$imputation <-  dataControl$imputation + 1
  })
  
  observeEvent(input$start_anno, {
    dataControl$annoStart <-  dataControl$annoStart + 1
  })
  
  observeEvent(input$submit_anno, {
    dataControl$annoSubmit <-  dataControl$annoSubmit + 1
  })
  
  processed_data <- reactive({
    if (dataControl$activateFilter > 0) {
      raw <- file_upload()
      uniquePep <- isolate(input$user_unique_pep)
      logTrans <- isolate(input$logTransform)
      
      # Filter identifications
      
      validate(
        need(raw$Potential.contaminant,
             message = "File error: is this a MaxQuant output file?")
      )
      
      df = raw %>%
        filter(Potential.contaminant != "+") %>%
        filter(Reverse != "+") %>%
        filter(Only.identified.by.site != "+") 
      
      df <- subset(df, df$Unique.peptides > (uniquePep-1))
      
      # Extract names of quant columns
      if (input$unlockCols == TRUE) {
        intensity.names = input$RemCols
      } else {
        if (input$userQuants == "lfq") {
          intensity.names = grep("^LFQ.intensity", names(df), value = TRUE)
        } else if (input$userQuants == "intensity") {
          intensity.names = grep("^Intensity", names(df), value = TRUE)
        } else {
          intensity.names = c()
          for (i in 0:input$tmtPlex) {
            pat <- paste0("^Reporter.intensity.corrected.", i, "$")
            tmt <- grep(pattern = pat, names(df), value = TRUE)
            intensity.names <- c(intensity.names, tmt)
          }
        }
      }
      
      # Cast as numeric
      df[intensity.names] = sapply(df[intensity.names], as.numeric)
      
      #logTransfomation
      if (logTrans == TRUE) {
        df[intensity.names] = log2(df[intensity.names])
        
      } 
  
      
      #create new dataframe from LFQ intensities
      df2 <-  df[intensity.names]
      
      
      if (input$median_center == TRUE) {
        df2 <- center_med(x = df2)
      }
      #assign protein IDs from oringinal this will be majority prt IDs in the end
      df2$Protein.IDs <- df$Protein.IDs
      df2$Majority.protein.IDs <- df$Majority.protein.IDs
      df2$Fasta.headers <- df$Fasta.headers
      #need to get everything before a ; character first
      #check for fasta col, if present use that
      parseRule <- grepl(">", df2$Fasta.headers)
      if (TRUE %in% parseRule ) {
        fasta <- word(df2$Fasta.headers, 1, sep = ";")
      } else {
        fasta <- word(df2$Majority.protein.IDs, 1, sep = ";")
      }
      
      sanityRule <- grepl("\\|", fasta)
      if (TRUE %in% sanityRule) {
        df2$UniprotID <- str_extract(fasta,"(?<=\\|)(.+)(?=\\|)")
        temp1_genename <- str_extract(fasta,"(?<=\\|)(.+)(?=\\_)")
        df2$GeneNames <- sapply(strsplit(temp1_genename, "\\|"), "[", 2)
      } else {
        df2$UniprotID <- fasta
        df2$GeneNames <- fasta
      }
      df2$Fasta.headers <- NULL
      
      
      #get the uniprot ID
      
      #rename cols
      if (input$userQuants == "lfq") {
        names(df2) = gsub(pattern = "LFQ.intensity.", replacement = "", x = names(df2))
      } else if (input$userQuants == "intensity") {
        names(df2) = gsub(pattern = "Intensity.", replacement = "", x = names(df2))
        df2$Intensity <- NULL 
      } else {
        names(df2) = gsub(pattern = "Reporter.intensity.corrected", replacement = "Reporter",
                          x = names(df2))
      }
      
      #rownames as IDs for later
      #df2 <- df2[1:nrow(df2)-1,]
      rownames(df2) <- df2$UniprotID
      
      #remove redundant col
      df2$UniprotID <- NULL
      df2$Protein.IDs <- NULL
      df2$Majority.protein.IDs <- NULL
      orig.col.names <- colnames(df2[names(df2) != "GeneNames"])
      
      
      if (dataControl$filterValids > 0) {
        #x <- df2
        anno_data_func <- anno_data()
        #need to keep genenames indexed
        #gene.names <- x$GeneNames
        #x$GeneNames <- NULL
        
        #colnames(x) <- anno_data$annotation
        
        
        min_val_user <- isolate(input$min_val_user)
        #in_one_user <- isolate(input$in_one)
        df2 <- filterValidVals(x = df2, 
                                user_val = min_val_user, 
                                in_one = input$in_one_user, 
                                anno_data = anno_data_func)
        
        #add gene names again
        #dat2$GeneNames <- gene.names
        #filter
        
        #dat2 <- dat2[!(dat2$keep=="FALSE"),]
        #dat2$keep <- NULL
        #dat2$geneNames <- NULL
        #df2 <- dat2
        
        if (dataControl$imputation > 0) {
          #centering
          impute_dat <- isolate(input$impute_choices)
          gene.names <- df2$GeneNames
          df2$GeneNames <- NULL
          colnames(df2) <- orig.col.names
          
          df2 <- imputeFunc(x = df2, 
                            width = input$imputeWidth,
                            downshift = input$imputeDS)
          anno_data <- anno_data()
          colnames(df2) <- anno_data$ID
          df2$GeneNames <- gene.names
          
        }
      }
      return(tryCatch(df2, error = function(e) stop(safeError(""))))
    }
    return(tryCatch(df2, error = function(e) stop(safeError(""))))
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
    if (dataControl$annoStart > 0) {
      categorial_anno <- categorial_anno()
      categorial_anno$ID <- as.character(categorial_anno$ID)
      categorial_anno$annotation <- as.character(categorial_anno$annotation)
      categorial_anno$axisLabels <- as.character(categorial_anno$axisLabels)
      tab <- rhandsontable(categorial_anno) %>%
          hot_col("ID", readOnly = T)
      return(tab)
    } else {
      return(NULL)
    }
  })
  
  #get data from user
  #displays need to signal data is submitted
  anno_data <- reactive({
    if (dataControl$annoSubmit > 0) {
      reps <- isolate(input$defineReps)
      repsOut <- hot_to_r(reps)
      return(repsOut)
    } else {
      return(NULL)
    }
  })
  
  redundant <- function(x){
    anno_data <- eventReactive(input$submit_anno, {
      reps <- isolate(input$defineReps)
      repsOut <- hot_to_r(reps)
      return(repsOut)
    })
  }
  
  #This controls enabling and disabling anno button
  
  observe({
    if (dataControl$annoSubmit > 0) {
      sendSweetAlert(
        session = session,
        title = "Success",
        text = "Your data is submitted",
        type = "success", 
        closeOnClickOutside = TRUE,
        width = 400
      )
     # hide("defineReps")
      disable("submit_anno")
    } else {
     # show("defineReps")
      enable("submit_anno")
    }
  })
  
  #render the data table
  output$user_data_in <-  DT::renderDataTable({
    
    #this is to remove error message in display
    if (is.null(dataControl$uploadState)) {
      return(NULL)
    } 
    
    if (dataControl$activateFilter == 0) {
      #first display
      file_upload <- file_upload()
      
      validate(
        need(file_upload$Potential.contaminant,
             message = "Certain key columns were not detected, are you sure the file is correct?")
      )
      
      datatable(file_upload, options = list(searching = F,
                                              pageLength = 20,
                                              lengthMenu = c(5, 10, 15, 20), 
                                              scrollX = T,
                                              autoWidth = TRUE
      ))
    } else if (dataControl$activateFilter > 0) {
      datatable(processed_data(),  options = list(searching = TRUE,
                                                  pageLength = 20,
                                                  lengthMenu = c(5, 10, 15, 20), 
                                                  scrollX = T,
                                                  autoWidth = TRUE
      ))
    }
  })

  #information boxes: Handlers
  
  infovals = reactiveValues(countervalue = 0)
  
  observeEvent(input$activate_filter, {
    infovals$countervalue <- infovals$countervalue + 1
  })
  
  observeEvent(input$submit_anno, {
    infovals$countervalue <- infovals$countervalue + 1
  })
  
  observeEvent(input$filter_valids, {
    infovals$countervalue <- infovals$countervalue + 1
  })
  
  observeEvent(input$start_imputation, {
    infovals$countervalue <- infovals$countervalue + 1
  })
  
  #render the info boxes
  output$data_handling_info <- renderValueBox({
    #check if data is loaded
    if (is.null(dataControl$uploadState)) {
      infoBox(title = "Information",
              value = "Upload proteinGroups.txt file",
              icon = icon("info"),
              color = "olive")
      
    } else if (infovals$countervalue == 0) {
      infoBox(title = "Information",
              value = div("Next step:",
                          br(),
                          "Filter erroneous IDs"),
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
  
  #valueboxes 1
  output$protein_ids_count <- renderValueBox({
    #Display unique peptides here
    if (is.null(dataControl$uploadState)) {
      valueBox(value = "No file loaded",
               subtitle = "Number of proteins",
               color = "aqua",
               icon = icon("list-ol"))
    } else {
      if (dataControl$activateFilter == 0) {
        
        file_upload <- file_upload()
        
        validate(
          need(file_upload$Potential.contaminant,
               message = "")
        )
        
        valueBox(value = nrow(file_upload),
                 subtitle = "Number of proteins",
                 color = "orange",
                 icon = icon("list-ol")) 
      } else {
        if (dataControl$activateFilter > 0) {
          
          valueBox(value = nrow(processed_data()),
                   subtitle = "Number of proteins",
                   color = "orange",
                   icon = icon("list-ol")) 
        }
      } 
    }
  }) #protein ID close
  
  #valuebox 2
  output$contaminants_count <- renderValueBox({
    
    if (is.null(dataControl$uploadState)) {
      valueBox(value = "No file loaded",
               subtitle = "Potential errouneuos protein IDs",
               color = "aqua",
               icon = icon("exclamation-triangle"))
    } else {
      if (dataControl$activateFilter == 0) {
        rawFile <- file_upload()
        
        validate(
          need(rawFile$Majority.protein.IDs, 
               message = "Protein IDs column not detected, is this a MaxQuant file?"))
        
        contam1 <- nrow(subset(rawFile, Potential.contaminant == "+"))
        contam2 <- nrow(subset(rawFile, Reverse == "+"))
        contam3 <- nrow(subset(rawFile, Only.identified.by.site == "+"))
        contam <- sum(contam1,contam2, contam3)
        
        valueBox(value = contam,
                 subtitle = "Potential errouneuos protein IDs",
                 icon = icon("exclamation-triangle"),
                 color = "orange")
      } else {
        if (dataControl$activateFilter > 0) {
          valueBox(value = 0,
                   subtitle = "Potential errouneuos protein IDs",
                   icon = icon("exclamation-triangle"),
                   color = "orange")
        }
      }
    } 
    
  }) #valuebox 2 close
  
  observeEvent(input$fileReset, {
    reset("user_file")
    dataControl$uploadState <- NULL
    dataControl$activateFilter <- 0
    dataControl$filterValids <- 0
    dataControl$imputation <- 0
    dataControl$annoStart <- 0
    dataControl$annoSubmit <- 0
    infovals$countervalue <- 0
  })

  #QC_plots------------------------------------------------------------------------------------------------->
  #ui_logic------------------------------------------------>
  
  output$normality_control1 <- renderUI({
    if (input$normPlotChoice == "qqPlot") {
      ui_widget <- textInput(inputId = "qqPlotTitle",
                             label = "Plot title",
                             value = "",
                             placeholder = "Q-Q plot of ...")
    } else {
      ui_widget <- textInput(inputId = "HistoTitle",
                             label = "Plot title",
                             value = "",
                             placeholder = "Histogram of ...")
    }
    return(ui_widget)
  })
  output$normality_control2 <- renderUI({
    if (input$normPlotChoice == "qqPlot") {
      ui_widget <- sliderInput(inputId = "qqPointSize",
                                label = "Change point size",
                                min = 1, max = 10, step = 1, 
                                value = 2)
    } else {
      ui_widget <- sliderInput(inputId = "HistoBinWidth",
                               label = "Select bin width",
                               min = 1, max = 100, step = ,
                               value = 30)
    }
    return(ui_widget)
  })
  output$normality_control3 <- renderUI({
    if (input$normPlotChoice == "qqPlot") {
      ui_widget <- sliderInput(inputId = "qqPlotAlphaChannel", 
                                label = "Change transparency",
                                min = 0.1, max = 1, step = 0.1, 
                                value = 1)
    } else {
      ui_widget <- prettySwitch(
        inputId = "HistoPlotDensity",
        label = "Add density distribution", 
        status = "primary",
        slim = TRUE)
    }
    return(ui_widget)
  })
  output$normality_control4 <- renderUI({
    if (input$normPlotChoice == "qqPlot") {
      return(NULL)
    } else {
      return(colourInput(inputId = "normDensityFill",
                         label = "Density plot colour",
                         palette = "limited",
                         value = "#666666"))
    }
  })
  output$normality_control5 <- renderUI({
    if (input$normPlotChoice == "qqPlot") {
      return(NULL)
    } else {
      return(sliderInput(inputId = "DensPlotAlphaChannel", 
                         label = "Change transparency",
                         min = 0.1, max = 1, step = 0.1, 
                         value = 0.4))
    }
  })
  
  #server_logic--------------------------------------------->
  Counter <- reactiveValues(normcounter = 1,
                            scatcounter = 1)

  
  observeEvent(input$normRender, {
    enable("normPrevious")
    enable("normNext")
  })
  
  observeEvent(input$normPrevious, {
    if (Counter$normcounter > 1) {
      Counter$normcounter <- Counter$normcounter - 1
    }
  })
  
  observeEvent(input$normNext, {
    #processed_data <- processed_data()
    if (Counter$normcounter < ncol(processed_data()) - 1) {
      Counter$normcounter <- Counter$normcounter + 1
    }
  })
  
  NormalityPlot <- reactive({
    
    if (is.null(input$user_file)) {
      return(NULL)
    }
    
    processed_data <- processed_data()
    
    processed_data$GeneNames <- NULL

    anno_data <- anno_data()
    colnames(processed_data) <- anno_data$axisLabels
    ylabname <- colnames(processed_data[Counter$normcounter])
    index <- Counter$normcounter
    qqplotList <- list()
    histPlotList <- list()
    
    #init plot cylce on render to save computing time
    if (input$normRender > 0) {
      #run for selected plots only
      if (input$normPlotChoice == "qqPlot") {
        for (i in 1:ncol(processed_data)) {
          dat <- data.frame(qqnorm(processed_data[,i], plot.it = FALSE))
          axisname <- colnames(processed_data[i])
          p <- ggplot(dat, aes(x, y)) + 
            geom_point(pch = 21,
                       alpha = input$qqPlotAlphaChannel,
                       size = input$qqPointSize,
                       colour = input$normPlotCol, 
                       fill = input$normPlotFill) + 
            geom_qq_line(aes(sample = y), 
                         lty = input$qqLinesType, 
                         lwd = input$qqLineWidth, 
                         colour = input$qqLineCol) +
            ylab(axisname) + 
            xlab("Theoretical quantiles") +
            ggtitle(input$qqPlotTitle) +
            theme_classic(base_size = 14) +
            theme(plot.title = element_text(hjust = input$normTitlePos, 
                                            face = input$normTitleFace,
                                            size = input$normTitleSize),
                  axis.title.x = element_text(size = input$normXsize),
                  axis.title.y = element_text(size = input$normYsize))
          
          qqplotList[[i]] = p
        }
        
        return(qqplotList)
        
      } else {
        #histograms
        for (i in 1:ncol(processed_data)) {
          axisName <- colnames(processed_data[i])
          
          p <- ggplot(processed_data, aes_string(processed_data[,i])) + 
            geom_histogram(aes(y = ..density..),
                           bins = input$HistoBinWidth, fill = input$normPlotFill,
                           colour = input$normPlotCol) +
            xlab(axisName) + 
            ggtitle(input$HistoTitle) +
            theme_classic() +
            theme(plot.title = element_text(hjust = input$normTitlePos, 
                                            face = input$normTitleFace,
                                            size = input$normTitleSize),
                  axis.title.x = element_text(size = input$normXsize),
                  axis.title.y = element_text(size = input$normYsize))
          
          if (input$HistoPlotDensity == TRUE) {
            p <- p + geom_density(fill = input$normDensityFill,
                                  alpha = input$DensPlotAlphaChannel)
          }
          
          
          histPlotList[[i]] = p
        }
        
        return(histPlotList)
      }
      
    } else {
      return(NULL)
    }
  })
  
  output$qqPlot <- renderPlot({NormalityPlot()[Counter$normcounter]})
  
  ###scatterplots###
  scatter_user <- reactive({
    if (is.null(input$user_file)) {
      return(NULL)
    }
    
    processed_data <- processed_data()
    anno_data <- anno_data()
    
    anno_data$axisLabels <- sapply(anno_data$axisLabels, function(data){
      data <- str_replace(pattern = "-", replacement = ".", string = data)
      return(data)
    })
    
    colnames(processed_data) <- as.character(anno_data$axisLabels)
    index <- Counter$scatcounter
    plot_list <- list()
    plot.col <- input$scat_point_col
    for(i in unique(anno_data$annotation)){
      COLS=anno_data$axisLabels[anno_data$annotation ==i]
      plot_combinations <- combn(COLS,
                                 2,
                                 simplify = FALSE)
      
      for (a in 1:length(plot_combinations)) {
        p = ggplot(processed_data,
                   aes_string(x = plot_combinations[[a]][1], 
                              y = plot_combinations[[a]][2])) +
          geom_point(pch = as.integer(input$scatCharcters), 
                     colour = input$scatPointBorder, 
                     size = input$scatPointSize,
                     fill = plot.col,
                     alpha = input$scatPlotAlphaChannel) + 
          ggtitle(input$scatTitle) +
          theme_classic(base_size = 14) +
          theme(plot.title = element_text(hjust = input$scatTitlePos,
                                          face = input$scatTitleFace,
                                          size = input$scatTitleSize),
                axis.title.x = element_text(size = input$scatXsize),
                axis.title.y = element_text(size = input$scatYsize))
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
  
  corrProcessing <- reactive({
    processed_data <-processed_data()
    
    anno_data <- anno_data()
    processed_data$GeneNames <- NULL
    colnames(processed_data) <- anno_data$axisLabels
    d <- processed_data
    
    checks <- sapply(d, function(x) {is.finite(x)})
    
    if (FALSE %in% checks) {
      
      sanityRule <- "fail"
    } else {
      sanityRule <- "pass"
    }
    
    validate(
      need(sanityRule == "pass", message = "This plot requires data with no missing values")
    )
    
    cormatrix = rcorr(as.matrix(d), type=input$correlation_test)
    cordata = melt(cormatrix$r)
    cordata$labelr = abbreviateSTR(melt(cormatrix$r)$value, 'r')
    cordata$labelP = abbreviateSTR(melt(cormatrix$P)$value, 'P')
    cordata$label = paste(cordata$labelr, "\n", 
                          cordata$labelP, sep = "")
    cordata$strike = ""
    cordata$strike[cormatrix$P > 0.05] = "X"
    
    txtsize <- par('din')[2] / 2
    
    cordata$value <- round(cordata$value, digits = input$corrDecimalPos)
    
    return(cordata)
  })
  
  correllelogram <- reactive({
  
    if (input$corrRender == 0) {
      return(NULL)
    } else {
      cordata <- corrProcessing()
      p = ggplot(cordata, aes(x=Var1, y=Var2, fill=value)) +
        geom_tile() + 
        scale_fill_gradientn(colours = brewer.pal(input$corrColChoice, 
                                                  n = input$corrColourScale), 
                             name = "Pearson",
                             limits = c(input$corrSlider, 1)) +
        xlab(NULL) + ylab(NULL) + ggtitle(input$corrPlotTitle) +
        theme_classic(base_size = 14) +
        theme(plot.title = element_text(hjust = input$corrTitlePos, 
                                        face = input$corrTitleFace,
                                        size = input$corrTitleSize),
              axis.text.x = element_text(angle=90, hjust = 1, size = input$corrXSize),
              axis.text.y = element_text(size = input$corrYSize)) 
      
      if (input$corrValDisp == TRUE) {
        txtsize <- par('din')[2] / 2
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
      
      anno_data <- anno_data()
      idnames <- anno_data$ID
      dat$GeneNames <- NULL
      colnames(dat) <- as.character(idnames)
      if (input$pcaZscore == TRUE) {
        dat <- scale(dat, scale = TRUE)
      }
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
        geom_point(pch = as.integer(input$pcaCharcters), 
                   size = input$pcaPlotPointSize, 
                   colour = "black", 
                   alpha = input$pcaAlphaChannel,
                   aes(fill = pcaData$names)) +
        scale_fill_manual(values = cols) + 
        theme_classic() +
        ylab("Principle component 2") + xlab("Principle component 1") + 
        labs(fill = NULL) +
        ggtitle(input$pcaPlotTitle) +
        theme(legend.position = input$pcaLegendPostition,
              plot.title = element_text(hjust = input$pcaTitlePos,
                                        face = input$pcaTitleFace,
                                        size = input$pcaTitleSize),
              axis.title.x = element_text(size = input$pcaXSize),
              axis.title.y = element_text(size = input$pcaYSize))
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
    rownames(processed_data) <- paste(rownames(processed_data), 
                                      processed_data$GeneNames,
                                      sep = "_")
    processed_data$GeneNames <- NULL
    anno_data <- anno_data()
    
    validate(
      need(!is.null(anno_data),
           message = "No groups assigned")
    )
    
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
                                                yes = "Downregulated", no = "Non significant")))
    
    d2 <- data.frame(d.out,
                     colsplit(string = d.out$ID, 
                              pattern = "_", 
                              names = c("UniprotID", "GeneName")))
    rownames(d2) <- d2$UniprotID
    d2$ID <- NULL
    d2$UniprotID <- NULL
    
    return(d2)
  })
  
  output$downReg <- renderValueBox({
    if (input$calculateStats > 0) {
      d <- statsOut()
      
      valueBox(value = sum(round(d$qValue, 3) < input$UserSigCutoff & d$EffectSize < (input$UserFCCutoff * -1),
                           na.rm = TRUE),
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
      valueBox(value = sum(round(d$qValue, 3) < input$UserSigCutoff & d$EffectSize > input$UserFCCutoff, 
                           na.rm = TRUE ),
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
      valueBox(value = sum(round(d$qValue, 3) < input$UserSigCutoff & abs(d$EffectSize) > input$UserFCCutoff,
                           na.rm = TRUE ),
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
  volcPlotData <- reactive({
    datList <- list()
    for (i in 1:length(input$hypoTestMat)) {
      if (input$generateVolcs > 0) {
        fit2 <- statsTestedData()
        statComb <- statComb()
        
        d.out <- data.frame(ID = names(fit2$coefficients[,i]),
                            pValue = fit2$p.value[,i],
                            qValue = p.adjust(fit2$p.value[,i], input$pvalAdjust),
                            EffectSize = fit2$coefficients[,i],
                            comparison = statComb[i])
        d.out <- mutate(d.out, 
                        sig = ifelse(d.out$EffectSize > input$UserFCCutoff & round(d.out$qValue, 3) < input$UserSigCutoff, "Upregulated",
                                     ifelse(d.out$EffectSize < (input$UserFCCutoff * -1) & round(d.out$qValue, 3) < input$UserSigCutoff, "Downregulated", "Non significant")))
        
        
        d2 <- data.frame(d.out,
                         colsplit(string = d.out$ID, 
                                  pattern = "_", 
                                  names = c("UniprotID", "GeneName")))
        
        
        if (input$volclabelNoOfSig > 0) {
          
          if (input$volcSigLabels == "Upregulated") {
            d.up = d2 %>% 
              filter(sig == "Upregulated") %>%
              arrange(desc(EffectSize)) %>%
              slice(1:input$volclabelNoOfSig) %>%
              select(UniprotID, GeneName)
            
            d2 <- merge(d2, d.up, by = "UniprotID", all.x  = TRUE)
          } else if (input$volcSigLabels == "Downregulated") {
            d.down = d2 %>% 
              filter(sig == "Downregulated") %>%
              arrange(EffectSize) %>%
              slice(1:input$volclabelNoOfSig) %>%
              select(UniprotID, GeneName)
            
            d2 <- merge(d2, d.down, by = "UniprotID", all.x  = TRUE)
            
          } else if (input$volcSigLabels == "Both") {
            d.up = d2 %>% 
              filter(sig == "Upregulated") %>%
              arrange(desc(EffectSize)) %>%
              slice(1:input$volclabelNoOfSig) %>%
              select(UniprotID, GeneName) 
            
            colnames(d.up)[colnames(d.up)=="GeneName"] <- "GeneNameup"
            
            d.down = d2 %>% 
              filter(sig == "Downregulated") %>%
              arrange(EffectSize) %>%
              slice(1:input$volclabelNoOfSig) %>%
              select(UniprotID, GeneName) 
            
            colnames(d.down)[colnames(d.down)=="GeneName"] <- "GeneNamedown"
            
            df.down <- merge(d2, d.down, by = "UniprotID", all.x = TRUE)
            df.down$GeneName <- NULL
            d2 <- merge(df.down, d.up, by = "UniprotID", all.x = TRUE)
          }
        }
      }
      rownames(d2) <- d2$UniprotID
      d2$ID <- NULL
      d2$UniprotID <- NULL
      
      datName <- input$hypoTestMat[i]
      datList[[datName]] = d2
      
    } #for loop close
    return(datList)
  })
  
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
  
  volcPlot <- reactive({
    if (input$generateVolcs > 0) {
      #plotList <- list()
      d <- volcPlotData()[[volcCycler$counter]]
      #volcSigLabel <- volcSigLabel()
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
                                        hjust = input$volcTitlePos,
                                        size = input$VolcTitleSize),
              axis.title.x = element_text(size = input$VolcXSize),
              axis.title.y = element_text(size = input$VolcYSize))
      
      if (input$volcSigLabels == "Upregulated" || input$volcSigLabels == "Downregulated") {
        p <- p + geom_text_repel(aes(label = d$GeneName.y), show.legend = FALSE)
      }
      
      if (input$volcSigLabels == "Both") {
        p <- p + geom_text_repel(aes(label = d$GeneNamedown), show.legend = FALSE) +
          geom_text_repel(aes(label = d$GeneNameup), show.legend = FALSE)
      }
      
      
      
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
  
  ###heatmaps###
  HMPlotData <- reactive({
    fit2 <- statsTestedData()
    processed_data <- processed_data()
    anno_data <- anno_data()
    processed_data$GeneName <- NULL
    colnames(processed_data) <- anno_data$ID
    processed_data$UniprotID <- rownames(processed_data)
    
    statComb <- statComb()
    datList <- list()
    for (i in 1:length(input$hypoTestMat)) {
      if (input$generateHM > 0) {
        d.out <- data.frame(ID = names(fit2$coefficients[,i]),
                            pValue = fit2$p.value[,i],
                            qValue = p.adjust(fit2$p.value[,i], input$pvalAdjust),
                            EffectSize = fit2$coefficients[,i],
                            comparison = statComb[i])
        d.out <- mutate(d.out, 
                        sig = ifelse(d.out$EffectSize > input$UserFCCutoff & round(d.out$qValue, 3) < input$UserSigCutoff, "Upregulated",
                                     ifelse(d.out$EffectSize < (input$UserFCCutoff * -1) & round(d.out$qValue, 3) < input$UserSigCutoff, "Downregulated", "Non significant")))
        
        
        d2 <- data.frame(d.out,
                         colsplit(string = d.out$ID, 
                                  pattern = "_", 
                                  names = c("UniprotID", "GeneName")))
        
        if (input$HMAllorSig == "Sig") {
          if (input$HMSigLabels == "Upregulated") {
            d.up = d2 %>% 
              filter(sig == "Upregulated") %>%
              arrange(desc(EffectSize)) %>%
              slice(1:input$HMlabelNoOfSig) %>%
              select(UniprotID, GeneName)
            colnames(d.up)[colnames(d.up)=="GeneName"] <- "GeneNameup"
            
            d2 <- merge(d2, d.up, by = "UniprotID", all.x  = TRUE)
            d2$GeneName <- NULL
            d3 <- merge(processed_data, d2[, c("UniprotID", "GeneNameup")], by = "UniprotID", 
                        all.x = TRUE)
            d3 = d3 %>% 
              drop_na(GeneNameup)
            
            rownames(d3) <- d3$GeneNameup
            d3$GeneNameup <- NULL
            d3$UniprotID <- NULL
            d3 <- d3[,1:nrow(anno_data)]
            
          } else if (input$HMSigLabels == "Downregulated") {
            d.down = d2 %>% 
              filter(sig == "Downregulated") %>%
              arrange(EffectSize) %>%
              slice(1:input$HMlabelNoOfSig) %>%
              select(UniprotID, GeneName)
            
            colnames(d.down)[colnames(d.down)=="GeneName"] <- "GeneNamedown"
            
            d2 <- merge(d2, d.down, by = "UniprotID", all.x  = TRUE)
            d2$GeneName <- NULL
            d3 <- merge(processed_data, d2[, c("UniprotID", "GeneNamedown")], 
                        by = "UniprotID", 
                        all.x = TRUE)
            d3 = d3 %>% 
              drop_na(GeneNamedown)
            
            rownames(d3) <- d3$GeneNamedown
            d3$GeneNameup <- NULL
            d3$UniprotID <- NULL
            d3 <- d3[,1:nrow(anno_data)]
            
          } else if (input$HMSigLabels == "Both") {
            d.up = d2 %>% 
              filter(sig == "Upregulated") %>%
              arrange(desc(EffectSize)) %>%
              slice(1:input$HMlabelNoOfSig) %>%
              select(UniprotID, GeneName) 
            
            colnames(d.up)[colnames(d.up)=="GeneName"] <- "GeneNameup"
            
            d.down = d2 %>% 
              filter(sig == "Downregulated") %>%
              arrange(EffectSize) %>%
              slice(1:input$HMlabelNoOfSig) %>%
              select(UniprotID, GeneName) 
            
            colnames(d.down)[colnames(d.down)=="GeneName"] <- "GeneNamedown"
            
            df.down <- merge(d2, d.down, by = "UniprotID", all.x = TRUE)
            df.down$GeneName <- NULL
            
            d2 <- merge(df.down, d.up, by = "UniprotID", all.x = TRUE)
            
            d2$GeneName <- coalesce(d2$GeneNameup, d2$GeneNamedown)
            
            d3 <- merge(processed_data, d2, by = "UniprotID",
                        all.x = TRUE)
            d3 = d3 %>%
              drop_na(GeneName)
            
            rownames(d3) <- d3$GeneName
            
            d3$UniprotID <- NULL
            
            d3 <- d3[,1:nrow(anno_data)]
            
          }
          
          if (input$HMzScore == TRUE) {
            d4 <- scale(d3, scale = TRUE)
          } else {
            d4 <- d3
          }
          
          datName <- input$hypoTestMat[i]
          datList[[datName]] = d4
        }
        
      } #GenerateHM close
      
    } #for loop close
    return(datList)
  })
  
  HMCycler <- reactiveValues(counter = 1)
  
  observeEvent(input$generateHM, {
    enable("HMCyclePrevious")
    enable("HMCycleNext")
    output$HMcurrentCompareText <- renderText(input$hypoTestMat[HMCycler$counter])
  })
  
  output$HMComparisonUI <- renderUI({
    if (input$HMAllorSig == "Sig") {
      
      verbatimTextOutput(outputId = "HMcurrentCompareText")
    }
  })
  
  observe({
    if (input$HMAllorSig == "All") {
      updateCheckboxInput(session, "HMDispRow", value = FALSE)
    } else {
      updateCheckboxInput(session, "HMDispRow", value = TRUE)
    }
  })
  
  observeEvent(input$HMCyclePrevious, {
    if (HMCycler$counter > 1) {
      HMCycler$counter <- HMCycler$counter - 1
    }
  })
  
  observeEvent(input$HMCycleNext, {
    if (HMCycler$counter < length(input$hypoTestMat)) {
      HMCycler$counter <- HMCycler$counter + 1
    } else if (HMCycler$counter == length(input$hypoTestMat)) {
      HMCycler$counter <- 1
    }
  })
  
  allHeatMapData <- reactive({
    d <- processed_data()
    anno_data <- anno_data()
    d$GeneNames <- NULL
    colnames(d) <- anno_data$ID
    d$UniprotID <- rownames(processed_data)
    d$UniprotID <- NULL
    
    if (input$HMzScore == TRUE) {
      d2 <- scale(d, scale = TRUE)
    } else {
      d2 <- d
    }
    
    return(d2)
  })
  
  UserHeatmap <- reactive({
    anno_data <- anno_data()
    if (input$generateHM > 0) {
      if (input$HMAllorSig == "All") {
        d1 <- allHeatMapData()
      } else {
        d1 <- HMPlotData()[[HMCycler$counter]]
        #d1 <- SigheatmapData()
      }
      
      
      if (input$HMdata == "averages") {
        colnames(d1) <- anno_data$annotation
        d2 <- sapply(split.default(d1, names(d1)), rowSums, na.rm = TRUE)
      } else {
        colnames(d1) <- anno_data$axisLabels
        d2 <- d1
      }
      
      validate(
        need(dim(d2)[1] != 0, message = "This comparison has no significant proteins"), 
        errorClass = ".shiny-output-error-validation {
        color: red;
    }"
      )
      p <- pheatmap(d2, color = rev(brewer.pal(input$HMColChoice, 
                                           n = input$HMcolScale)),
                    border_color = input$HMborderCol,
                    fontsize_col = input$HMColFontSize,
                    fontsize_row = input$HMRowFontSize,
                    angle_col = input$HMcolAngle,
                    cluster_cols = input$HMclustCols,
                    cluster_rows = input$HMclustRows,
                    clustering_method = input$HMClustMethod,
                    show_colnames = input$HMDispCol,
                    show_rownames = input$HMDispRow, 
                    treeheight_col = input$HMColTreeHeight,
                    treeheight_row = input$HMColTreeHeight)
      
      return(p)
  }
    
  })
  
  output$Heatmap <- renderPlot({
    if (!is.null(input$user_file)) {
      UserHeatmap()
    } else {
      return(NULL)
    }
   
    })
  
  #Webgestalt---------------------------------------------------------------------------------------->
  webbestaltEnrichment <- function(data){
    if (input$webgestalt_tests == "ORA") {
      if (input$webgestalt_fdr == "fdr") {
        WebGestaltR(enrichMethod = "ORA",
                    interestGene = data,
                    isOutput = FALSE,
                    interestGeneType = "uniprotswissprot",
                    enrichDatabase = input$webgestalt_function_picker,
                    organism = input$webgesalt_orgs,
                    referenceSet = "genome_protein-coding",
                    projectName = "User",
                    sigMethod = input$webgestalt_fdr,
                    fdrMethod = input$webgestalt_fdr_options,
                    fdrThr = as.numeric(input$webgestalt_sig_radio_out)
        )
      } else {
        WebGestaltR(enrichMethod = "ORA",
                    interestGene = data,
                    isOutput = FALSE,
                    interestGeneType = "uniprotswissprot",
                    enrichDatabase = input$webgestalt_function_picker,
                    organism = input$webgesalt_orgs,
                    referenceSet = "genome_protein-coding",
                    projectName = "User",
                    sigMethod = input$webgestalt_fdr,
                    fdrMethod = input$webgestalt_fdr_options,
                    topThr = input$webgestalt_top_calc_slider
        )
      }
      
    } else if (input$webgestalt_tests == "GSEA") {
      
      tryCatch(WebGestaltR(enrichMethod = "GSEA",
                           interestGene = data,
                           isOutput = FALSE,
                           interestGeneType = "uniprotswissprot",
                           enrichDatabase = input$webgestalt_function_picker,
                           organism = input$webgesalt_orgs,
                           referenceSet = "genome_protein-coding"
      ), error = function(e) {
        stop(safeError(paste0(error_message_webgestalt(inputCall = paste0("No",
                                                                          " ",
                                                                          error_message_webgestalt(inputCall = input$webgestalt_function_picker),
                                                                          "\'s",
                                                                          " ",
                                                                          "were enriched, try another method or pathway")))))
      })
    }
    
    
  }
  
  enrichmentCycler <- reactiveValues(counter = 1)
  
  observeEvent(input$enricmentCyclePrevious, {
    if (enrichmentCycler$counter > 1) {
      enrichmentCycler$counter <- enrichmentCycler$counter - 1
    }
  })
  
  observeEvent(input$enrichmentCycleNext, {
    if (enrichmentCycler$counter < length(input$hypoTestMat)) {
      enrichmentCycler$counter <- enrichmentCycler$counter + 1
    } else if (enrichmentCycler$counter == length(input$hypoTestMat)) {
      enrichmentCycler$counter <- 1
    }
  })
  
  observeEvent(input$generateEnrichments, {
    enable("enrichmentCyclePrevious")
    enable("enrichmentCycleNext")
    output$enrichmet_currentCompareText <- renderText(input$hypoTestMat[enrichmentCycler$counter])
  })
  
  
  output$webgestalt_enrichment_variations_render <- renderUI({
    if (input$webgestalt_tests == "ORA") {
      pickerInput(inputId = "enrichment_data_options_ora",
                  label = "What should be enriched", 
                  c("All protein IDs" = "all",
                    "Upregulated proteins" = "up", 
                    "Downregulated proteins" = "down"),
                  selected = "all",
                  multiple = FALSE,
                  choicesOpt = list(
                    style = rep(("color: black;"),3)))
    } else {
      pickerInput(inputId = "enrichment_data_options_gsea",
                  label = "What should be enriched", 
                  c("Strict" = "strict", 
                    "Lenient" = "lenient",
                    "All protein IDs" = "all"),
                  selected = "strict",
                  multiple = FALSE,
                  choicesOpt = list(
                    style = rep(("color: black;"),3)))
    }
    
  })
  
  output$webgestalt_function <- renderUI({
    if (input$webgestalt_db == "pathway") {
      pickerInput(inputId = "webgestalt_function_picker",
                  label = "Choose pathway database", 
                  choices = c("KEGG" = "pathway_KEGG",
                              "Panther" = "pathway_Panther", 
                              "Reactome" = "pathway_Reactome",
                              "Wiki pathway" = "pathway_Wikipathway"),
                  selected = "pathway_KEGG",
                  choicesOpt = list(
                    style = rep(("color: black;"),4)))
    } else {
      pickerInput(inputId = "webgestalt_function_picker",
                  label = "Choose enrichment database", 
                  choices = c("Biological processes" = "geneontology_Biological_Process",
                              "Molecular function" = "geneontology_Molecular_Function",
                              "Cellular component" = "geneontology_Cellular_Component"),
                  choicesOpt = list(
                    style = rep(("color: black;"),3)))
    } 
  })
  
  output$webgestalt_fdr_render <- renderUI({
    if (input$webgestalt_tests == "ORA") {
      radioButtons(inputId = "webgestalt_fdr",
                   label = "Choose correction method",
                   choices = c("False discovery rate" = "fdr",
                               "Top" = "top"),
                   selected = "fdr",
                   inline = TRUE)
    } else {
      return(NULL)
    }
  })
  
  output$webgestalt_fdr_options_render <- renderUI({
    if (input$webgestalt_tests == "ORA" && input$webgestalt_fdr == "fdr") {
      pickerInput(inputId = "webgestalt_fdr_options",
                  label = "Choose enrichment FDR cut off", 
                  choices = c(p.adjust.methods[1:6]),
                  selected = "BH",
                  choicesOpt = list(
                  style = rep(("color: black;"),6)))
    } else {
      return(NULL)
    }
  })
  
  output$webgestalt_sig_slider <- renderUI({
    if (input$webgestalt_tests == "ORA" && input$webgestalt_fdr == "fdr") {
      radioButtons(inputId = "webgestalt_sig_radio_out",
                   label = "Choose test FDR cut-off",
                   choices = c(0.01, 0.05),
                   selected = 0.05,
                   inline = TRUE)
    }
  })
  
  output$webgestalt_yaxis_render <- renderUI({
    if (input$webgestalt_tests == "ORA") {
      pickerInput(inputId = "webgestalt_yaxis",
                  label = "Choose plot data",
                  choices = c("Enrichment ratio" = "enrichmentRatio",
                              "P-value" = "pValue",
                              "False discovery rate" = "FDR"),
                  selected = "enrichmentRatio",
                  multiple = FALSE,
                  
                  options = list(`actions-box` = TRUE, 
                                 `selected-text-format` = "count > 0"),
                  choicesOpt = list(
                    style = rep(("color: black;"), 3)))
    } else {
      pickerInput(inputId = "webgestalt_yaxis",
                  label = "Y axis",
                  choices = c("Enrichment score" = "enrichmentScore",
                              "Normalised enrichment score" = "normalizedEnrichmentScore",
                              "p-value" = "pValue",
                              "False discovery rate" = "FDR"),
                  selected = "normalizedEnrichmentScore",
                  multiple = FALSE,
                  
                  options = list(`actions-box` = TRUE, 
                                 `selected-text-format` = "count > 0"),
                  choicesOpt = list(
                    style = rep(("color: black;"), 4)))
      }
    })
  
  output$top_enrichment_slider <- renderUI({
    if (input$webgestalt_tests == "ORA" && input$webgestalt_fdr == "top") {
      sliderInput(inputId = "webgestalt_top_calc_slider",
                  label = "Plot top n enrichments",
                  min = 1, max = 30, step = 1,
                  value = 10)
      
    }
  })
     

  encrichment_input_data <- reactive({
    if (input$generateEnrichments > 0) {
      enrichment_data_list <- list()
      for (i in 1:length(input$hypoTestMat)) {
        fit2 <- statsTestedData()
        statComb <- statComb()
        
        d.out <- data.frame(ID = names(fit2$coefficients[,i]),
                            pValue = fit2$p.value[,i],
                            qValue = p.adjust(fit2$p.value[,i], input$pvalAdjust),
                            EffectSize = fit2$coefficients[,i],
                            comparison = statComb[i])
        d.out <- mutate(d.out, 
                        significant = ifelse(d.out$EffectSize > input$UserFCCutoff & round(d.out$qValue, 3) < input$UserSigCutoff, "Upregulated",
                                     ifelse(d.out$EffectSize < (input$UserFCCutoff * -1) & round(d.out$qValue, 3) < input$UserSigCutoff, "Downregulated", "Non_significant")))
        
        
        d2 <- data.frame(d.out,
                         colsplit(string = d.out$ID, 
                                  pattern = "_", 
                                  names = c("UniprotID", "GeneName")))
        
        enrichment_data_name <- input$hypoTestMat[i]
        enrichment_data_list[[enrichment_data_name]] = d2
        
      } # for loop close
      
      
      enrichment_target_df <- enrichment_data_list[[enrichmentCycler$counter]]
      validate(need(enrichment_target_df, 
                    message = "Start the calculations first"))
    }
   
    return(enrichment_target_df)
  })
  
  enrichment_data <- eventReactive(input$calculateEnrichments, {
    encrichment_input_data <- encrichment_input_data()
    if (input$webgestalt_tests == "ORA") { 
      if (input$enrichment_data_options_ora == "up") {
        enrichment_target_list_up <- ora_list(ora_data = encrichment_input_data,
                                              regulation = "Upregulated")
        
        
        enrichment_out_up <- webbestaltEnrichment(data = enrichment_target_list_up)

        
        enrichment_out_data <- enrichment_out_up
        
        
      } else if (input$enrichment_data_options_ora == "down") {
        
        enrichment_target_list_down <- ora_list(ora_data = encrichment_input_data,
                                             regulation = "Downregulated")
        
        
        enrichment_out_down <- webbestaltEnrichment(data = enrichment_target_list_down)
        
        
        enrichment_out_data <- enrichment_out_down

        
      } else {
        
        enrichment_target_list_up <- ora_list(ora_data = encrichment_input_data,
                                              regulation = "Upregulated")
        
        enrichment_target_list_down <- ora_list(ora_data = encrichment_input_data,
                                                regulation = "Downregulated")
        
        
        enrichment_out_up <- webbestaltEnrichment(data = enrichment_target_list_up)
        

        
        enrichment_out_down <- webbestaltEnrichment(data = enrichment_target_list_down)
        
        
        if (!is.null(enrichment_out_down)) {
          enrichment_out_down$enrichmentRatio <- enrichment_out_down$enrichmentRatio * - 1
          
        }
        
        
        enrichment_out_data <- rbind(enrichment_out_up, enrichment_out_down)
        
        
      }
     
    } else {
      #GSEA starts here
      
      gsea_data <- gsea_df(gsea_data = encrichment_input_data,
                           enrichment_options = input$enrichment_data_options_gsea, 
                           sig_cutoff = input$UserSigCutoff)
      
      enrichment_out_data <- webbestaltEnrichment(data = gsea_data)
      
      
    }
    return(enrichment_out_data)
  })
  
  enrichment_data_filtered <- reactive({
    enrichment_data <- enrichment_data()
    if (input$webgestalt_tests == "ORA") {
      
      validate(
        need(enrichment_data$enrichmentRatio,
             message = paste0("recalculate")))
     
      if (input$enrichment_data_options_ora == "up") {
        if (!is.null(enrichment_data)) {
          enrichment_data = enrichment_data %>%
            arrange(desc(enrichmentRatio)) %>%
            slice(1:input$webgestalt_top_n_slider)
        }
        
        validate(
          need(enrichment_data$FDR,
               message = paste0("No",
                                " ",
                                error_message_webgestalt(inputCall = input$webgestalt_function_picker),
                                " ",
                                "were enriched, try another method or pathway"))
        )
        
      } else if (input$enrichment_data_options_ora == "down") {
        if (!is.null(enrichment_data)) {
          enrichment_data = enrichment_data %>%
            arrange(enrichmentRatio) %>%
            slice(1:input$webgestalt_top_n_slider)
        }
        
        validate(
          need(enrichment_data$FDR,
               message = paste0("No",
                                " ",
                                error_message_webgestalt(inputCall = input$webgestalt_function_picker),
                                " ",
                                "were enriched, try another method or pathway"))
        )
        
      } else {
        # merged up and down
        if (!is.null(enrichment_data)) {
          enrichment_up = enrichment_data %>%
            filter(enrichmentRatio > 0) %>%
            arrange(desc(enrichmentRatio)) %>%
            slice(1:input$webgestalt_top_n_slider)
          
          enrichment_down = enrichment_data %>%
            filter(enrichmentRatio < 0) %>%
            arrange(enrichmentRatio) %>%
            slice(1:input$webgestalt_top_n_slider)
          
          enrichment_data <- rbind(enrichment_up, enrichment_down)
        }
        
        
        validate(
          need(enrichment_data$FDR,
               message = paste0("No",
                                " ",
                                error_message_webgestalt(inputCall = input$webgestalt_function_picker),
                                " ",
                                "were enriched, try another method or pathway"))
        )
        
      }
    } else {
      #GSEA
      
      enrichment_data <- gsea_webgestalt_df(data = enrichment_data, topN = input$webgestalt_top_n_slider)
      
      
      validate(
        need(enrichment_data$FDR,
             message = paste0("No",
                              " ",
                              error_message_webgestalt(inputCall = input$webgestalt_function_picker),
                              " ",
                              "were enriched, try another method or pathway"))
      )
    }
    
    return(enrichment_data)
    
  })
  
  observeEvent(input$webgestalt_tables, {
    showModal(tags$div(id="ORA", data_table))
  })
  
  data_table <- modalDialog(
    
    fluidPage(
      h3(strong("Enrichment table"), align="left"),
      dataTableOutput('table_render')
    ),
    size="l", 
    easyClose = TRUE,
    fade = TRUE 
  )
  
  output$table_render <-  DT::renderDataTable({
    datatable(enrichment_data_filtered(), extensions = 'Buttons',
              options = list(
                scrollX = T,
                autoWidth = TRUE,
                dom = "Blfrtip",
                buttons = 
                  list("copy", list(
                    extend = "collection",
                    buttons = c("csv", "excel", "pdf"),
                    text = "Download", filename = paste0(input$webgestalt_tests,
                                                         "_", 
                                                         input$hypoTestMat[enrichmentCycler$counter],
                                                         "_",
                                                         input$webgestalt_function_picker)
                  ) ), # end of buttons customization
                
                # customize the length menu
                lengthMenu = list( c(10, 20, -1) # declare values
                                   , c(10, 20, "All") # declare titles
                ), # end of lengthMenu customization
                pageLength = 10 
              ))

  })
  
  enrichment_plot <- reactive({
    enrichment_data <- enrichment_data_filtered()
    if (input$webgestalt_tests == "GSEA") {
      validate(need(enrichment_data$normalizedEnrichmentScore,
                    message = "Recalculate the enrichment"))
    } else {
      validate(need(enrichment_data$enrichmentRatio,
                    message = "Recalculate the enrichment"))
    }
    
    p <- ggplot(enrichment_data, aes_string(x = paste0("reorder(description", 
                                                       ",",
                                                       "",
                                                       "-",
                                                       input$webgestalt_yaxis,
                                                       ")"),
                                            y = input$webgestalt_yaxis)) +
      geom_bar(stat = "identity", fill = input$webgestalt_colour_fill, colour = "black") +
      ylab(input$webgestalt_x_label) + xlab(input$webgestalt_y_label) + 
      coord_flip() +
      theme_classic(base_size = 14) +
      theme(axis.text.x = element_text(size = input$enrichment_x_axis_font_size),
            axis.text.y = element_text(size = input$enrichment_y_axis_font_size, face = "bold"),
            axis.title.x = element_text(size = input$enrichment_x_title_font_size),
            axis.title.y = element_text(size = input$enrichment_y_title_font_size))
        
    
    return(p)
  })
  
  output$webgestalt_plot <- renderPlot({
    if (input$calculateEnrichments == 0) {
      return(NULL)
    } else {
      enrichment_plot()
    }
  })
  

#stringDB-------------------------------------------------------------------------------------->
#basic buid: construct a base html and hit the stringt api using httr
# 
  # Server side UI logic
  
  stringCycler <- reactiveValues(counter = 1)
  
  observeEvent(input$stringCyclePrevious, {
    if (stringCycler$counter > 1) {
      stringCycler$counter <- stringCycler$counter - 1
    }
  })
  
  observeEvent(input$stringCycleNext, {
    if (stringCycler$counter < length(input$hypoTestMat)) {
      stringCycler$counter <- stringCycler$counter + 1
    } else if (stringCycler$counter == length(input$hypoTestMat)) {
      stringCycler$counter <- 1
    }
  })
  
  observeEvent(input$generateStringNetwork, {
    enable("stringCyclePrevious")
    enable("stringCycleNext")
    output$string_currentCompareText <- renderText(input$hypoTestMat[stringCycler$counter])
  })
  
  
  # server sude UI rendereing
  output$string_data_inputs <- renderUI({
    if (input$string_data_options == "upregulated" || input$string_data_options == "downregulated") {
      sliderInput(inputId = "string_no_of_proteins",
                  label = "Number of proteins",
                  min = 1, max = 50, step = 1,
                  value = 1)
    } else {
      textInput(inputId = "string_proteins_user",
                label = "Enter proteins comma separated",
                placeholder = "O69732,P9WJC1")
    }
  })
  
  # data input
  
  string_full_sig_data <- reactive({
    if (input$generateStringNetwork > 0) {
      string_data_list <- list()
      for (i in 1:length(input$hypoTestMat)) {
        fit2 <- statsTestedData()
        statComb <- statComb()
        
        d.out <- data.frame(ID = names(fit2$coefficients[,i]),
                            pValue = fit2$p.value[,i],
                            qValue = p.adjust(fit2$p.value[,i], input$pvalAdjust),
                            EffectSize = fit2$coefficients[,i],
                            comparison = statComb[i])
        d.out <- mutate(d.out, 
                        significant = ifelse(d.out$EffectSize > input$UserFCCutoff & round(d.out$qValue, 3) < input$UserSigCutoff, "upregulated",
                                             ifelse(d.out$EffectSize < (input$UserFCCutoff * -1) & round(d.out$qValue, 3) < input$UserSigCutoff, "downregulated", "non_significant")))
        
        
        d2 <- data.frame(d.out,
                         colsplit(string = d.out$ID, 
                                  pattern = "_", 
                                  names = c("UniprotID", "GeneName")))
        
        string_data_name <- input$hypoTestMat[i]
        string_data_list[[string_data_name]] = d2
        
      } # for loop close
      
      
      string_target_df <- string_data_list[[stringCycler$counter]]
      validate(need(string_target_df, 
                    message = "Start the calculations first"))
    }
    
    return(string_target_df)
  })
  
  
  string_URL_data <- reactive({
    df <- string_full_sig_data()
    if (input$string_data_options == "custom") {
      string_df <- input$string_proteins_user
    } else if (input$string_data_options == "upregulated") {
      string_df <- df %>%
        filter(significant == input$string_data_options) %>%
        arrange(EffectSize) %>%
        select(UniprotID) %>%
        slice(1:input$string_no_of_proteins)
      
    } else if (input$string_data_options == "downregulated") {
      string_df <- df %>%
        filter(significant == input$string_data_options) %>%
        arrange(EffectSize) %>%
        slice(1:input$string_no_of_proteins)
       

    }
    return(string_df)
  })
  
  string_url <- reactive({
    
    if (input$string_data_options == "custom") {
      URL <- string_url_builder(sig_thresh = input$string_sig_threshold,
                                max_nodes = input$string_max_threshold, 
                                protein_querry = reformat_proteinID(input_ID = string_URL_data(),
                                                                    data_options = input$string_data_options))
    } else {
      string_df <- string_URL_data()
      URL <- string_url_builder(sig_thresh = input$string_sig_threshold,
                                max_nodes = input$string_max_threshold, 
                                protein_querry = reformat_proteinID(input_ID = string_df$UniprotID,
                                                                    data_options = input$string_data_options))
    }
    
    return(URL)
  })
  
  # render image
  output$string_image <- renderUI({
    if (input$generateStringNetwork > 0) {
      
      div(
        tags$img(src=string_url(),
                 id="stringImage",
                 width="auto",
                 height="auto",
                 align="left",
                 style = "position: center;"
        ))
  
    } else {
      return(c("Start calculations first"))
    }


  })

  

  #downlowding--------------------------------------------------------------------------------------------------------->
  #Citations------------------------------------------------------------->
  citation_filename <- reactive({
    switch (input$citation_options,
      All = {return("all_citations.ris")},
      Provision = {return("Provision_citation.ris")},
      StringDB = {return("StringDB_citation.ris")},
      Webgestalt = {return("Webgestalt_citation.ris")},
      Limma = {return("Limma_citation.ris")}
      
    )
  })
  output$citation_download <- downloadHandler(
    filename <- function() {
      citation_filename()
    },
    
    content <- function(file) {
      citation_file <- input$citation_options
      file.copy(str_glue("www/data/{citation_file}.ris"), file)
    },
    contentType = "application/ris"
  )
  
  #processed data-------------------------------------------------------->
  output$ProcDataSelectorUI <- renderUI({
    if (input$ProcDataDownType == "txt") {
      selectInput(inputId = "ProcDataDownSep",
                  label = "Choose separator",
                  choices = c("Tab" = "\t",
                              "Comma" = ",",
                              "Space" = " ",
                              "Colon" = ";"),
                  selected = "\t")
    }
  })
  dataFileName <- reactive({
    if (input$ProcDataDownName == "") {
      if (input$ProcDataDownType == "xlsx") {
        
        n = paste(Sys.Date(), "-", "processedMQ", ".", "xlsx", sep = "")
      } else {
        n = paste(Sys.Date(), "-", "processedMQ", ".", "txt", sep = "")
      }
    } else {
      if (input$ProcDataDownType == "xlsx") {
        n = paste(input$ProcDataDownName, ".", "xlsx", sep = "")
      } else {
        n = paste(input$ProcDataDownName, ".txt", sep = "")
      }
    }
    return(n)
  })
  dataoutprocess <- reactive({
    d <- processed_data()
    anno_data <- anno_data()
    gene.names <- d$GeneNames
    d$GeneNames <- NULL
    colnames(d) <- anno_data[1:nrow(anno_data),3]
    d$UniprotID <- rownames(d)
    d$GeneNames <- gene.names
    return(d)
  })
  
  output$filt1_download <- downloadHandler(
    filename = function() {
      dataFileName()
    },
    content = function(file) {
      if (input$ProcDataDownType == "xlsx") {
        write.xlsx2(dataoutprocess(), 
                    file = file, 
                    sheetName = "Sheet1",
                    col.names = TRUE, 
                    row.names = FALSE, 
                    append = FALSE)
      } else {
        write.table(dataoutprocess(), 
                    file = file, 
                    sep = input$ProcDataDownSep,
                    row.names = F)
        
      }
      
    }
  )
  
  #Significant data
  output$SigDataSelectorUI <- renderUI({
    if (input$SigDataDownType == "txt") {
      selectInput(inputId = "SigDataDownSep",
                  label = "Choose separator",
                  choices = c("Tab" = "\t",
                              "Comma" = ",",
                              "Space" = " ",
                              "Colon" = ";"),
                  selected = "\t")
    }
  })
  SigFileName <- reactive({
    
    if (input$WhichSigDataDown == "current") {
      
      if (input$SigDataDownName == "") {
        if (input$SigDataDownType == "xlsx") {
          
          n = paste(Sys.Date(), "-", "Statistics-",input$hypoTestMat[statsCycler$counter], ".", "xlsx", sep = "")
        } else {
          n = paste(Sys.Date(), "-", "Statistics-",input$hypoTestMat[statsCycler$counter], ".", "txt", sep = "")
        }
      } else {
        if (input$SigDataDownType == "xlsx") {
          n = paste(input$SigDataDownName, ".", "xlsx", sep = "")
        } else {
          n = paste(input$SigDataDownName, ".txt", sep = "")
        }
      }
      
    } else {
      
      n <- "Statistics.zip"
      
    }
    
    return(n)
  })
  dataoutSig <- reactive({
    if (input$WhichSigDataDown == "current") {
      d <- statsOut()
      d$UniprotID <- rownames(d)
      return(d)
    } else {
      fit2 <- statsTestedData()
      datList <- lapply(1:length(statComb()), function(i) {
        d.out <- paste("d-", i, sep = "")
        d.out <- data.frame(ID = names(fit2$coefficients[,i]),
                            pValue = fit2$p.value[,i],
                            qValue = p.adjust(fit2$p.value[,i], input$pvalAdjust),
                            EffectSize = fit2$coefficients[,i],
                            comparison = statComb()[i])
        
        d.out <- mutate(d.out, 
                        sig = ifelse(d.out$EffectSize > input$UserFCCutoff & round(d.out$qValue, 3) < input$UserSigCutoff, "Upregulated",
                                     ifelse(d.out$EffectSize < (input$UserFCCutoff * -1) & round(d.out$qValue, 3) < input$UserSigCutoff, "Downregulated", "Non significant")))
        dat <- data.frame(d.out,
                          colsplit(string = d.out$ID, 
                                   pattern = "_", 
                                   names = c("UniprotID", "GeneName")))
        dat$ID <- NULL
        return(dat)
        
      })
      return(datList)
    }
  })
  
  output$SigDownload <- downloadHandler(
    filename = function() {
      SigFileName()
    },
    content = function(file) {
      if (input$WhichSigDataDown == "current") {
        if (input$SigDataDownType == "xlsx") {
          write.xlsx2(dataoutSig(), 
                      file = file, 
                      sheetName = "Sheet1",
                      col.names = TRUE, 
                      row.names = FALSE, 
                      append = FALSE)
        } else {
          write.table(dataoutSig(), 
                      file = file, 
                      sep = input$SigDataDownSep,
                      row.names = F)
          
        }
      } else {
        files <- NULL;
        withProgress(message = "Saving files", value = 0, {
          for (i in 1:length(dataoutSig())) {
            if (input$SigDataDownType == "xlsx") {
              fileName = paste(Sys.Date(), "-", "Statistics", "-", statComb()[i], ".", "xlsx", sep = "")
              write.xlsx2(dataoutSig()[i], 
                          file = fileName, 
                          sheetName = "Sheet1",
                          col.names = TRUE, 
                          row.names = FALSE, 
                          append = FALSE)
            } else {
              fileName = paste(Sys.Date(), "-", "Statistics", "-", statComb()[i], ".", "txt", sep = "")
              write.table(dataoutSig()[i], 
                          file = fileName, 
                          sep = input$SigDataDownSep,
                          row.names = F)
            } #file naming close
            files <- c(fileName, files)
            incProgress(1/length(dataoutSig()), 
                        detail = paste("Adding file:", i, sep = " "))
          } # for loop close
        })
        
        zip(file, files)
      }
      
    }
  )
  #normaldistrubtion--------------------------------------------------------------------------------------------->
  normFileName <- reactive({
    if (input$normFigDownChoice == "Current") {
      p <- NormalityPlot()[Counter$normcounter]
      axisTitleList <- as.character(unlist(p[[1]][["labels"]]))
      
      if (input$normPlotChoice == "qqPlot") {
        name <- paste("qqPlot-", 
                      axisTitleList[3], ".", 
                      input$normFigDownType,
                      sep = "")
      } else {
        name <- paste("Histogram-", 
                      axisTitleList[2], ".", 
                      input$normFigDownType,
                      sep = "")
      }
      
    } else {
      if (input$normPlotChoice == "qqPlot") {
        name <- "qqplots.zip"
      } else {
        name <- "Histograms.zip"
      }
      
    }
    return(name)
  })
  
  output$normFigDownload <- downloadHandler(
    filename = function() { normFileName() },
    content = function(file) {
      if (input$normFigDownChoice == "Current") {
        ggsave(file,
               plot = NormalityPlot()[[Counter$normcounter]],
               device = isolate(input$normFigDownType),
               dpi = isolate(input$normFigRes)
        )
      } else {
        files <- NULL;
        withProgress(message = "Saving files", value = 0, {
          for (i in 1:length(NormalityPlot())) {
            p <- NormalityPlot()[[i]]
            axisTitleList <- as.character(unlist(p[["labels"]]))
            
            if (input$normPlotChoice == "qqPlot") {
              FileName <- paste("qqPlot-", axisTitleList[3], ".", 
                                input$normFigDownType,
                                sep = "")
            } else{
              FileName <- paste("Histogram-", axisTitleList[2], ".", 
                                input$normFigDownType,
                                sep = "")
            }
            
            ggsave(filename = FileName,
                   plot = NormalityPlot()[[i]],
                   device = isolate(input$normFigDownType),
                   dpi = isolate(input$normFigRes)
            )
            #files
            files <- c(FileName, files)
            incProgress(amount = 1/length(NormalityPlot()), 
                        detail = paste("Adding plot:", i, sep = " "))
          }
        })
        zip(file, files)
      }
    }
  )
  
  #scatterplots------------------------------------------------------------------------------------------->
  scatFileName <- reactive({
    if (input$scatFigDownChoice == "Current") {
      p <- scatter_user()[[Counter$scatcounter]]
      axisTitleList <- as.character(unlist(p[["labels"]]))
      axisTitle <- paste(axisTitleList[1], axisTitleList[2], sep = "-")
      name <- paste("Scatter-", axisTitle, ".", input$scatFigDownType,
                    sep = "")
    } else {
      name <- "Scatterplots.zip"
    }
    return(name)
  })
  
  output$scatFigDownload <- downloadHandler(
    filename = function() { scatFileName() },
    content = function(file) {
      if (input$scatFigDownChoice == "Current") {
        ggsave(file,
               plot = scatter_user()[[Counter$scatcounter]],
               device = isolate(input$scatFigDownType),
               dpi = isolate(input$scatFigRes)
        )
      } else {
        files <- NULL;
        withProgress(message = "Saving files", value = 0, {
          for (i in 1:length(scatter_user())) {
            p <- scatter_user()[[i]]
            axisTitleList <- as.character(unlist(p[["labels"]]))
            axisTitle <- paste(axisTitleList[1], axisTitleList[2], sep = "-")
            FileName <- paste("Scatter-", axisTitle, ".", input$scatFigDownType,
                              sep = "")
            
            ggsave(filename = FileName,
                   plot = scatter_user()[[i]],
                   device = isolate(input$scatFigDownType),
                   dpi = isolate(input$scatFigRes)
            )
            #files
            files <- c(FileName, files)
            incProgress(1/length(scatter_user()), detail = paste("Adding plot:", i, sep = " "))
          }
        })
        
        zip(file, files)
      }
    }
  )
  
  #correlation----------------------------------------------------------------------------------------------------->
  corrFileName <- reactive({
    name <- paste("Correlogram", ".", input$corrFigDownType,
                  sep = "")
    
    return(name)
  })
  
  output$corrFigDownload <- downloadHandler(
    #getting input is not working for filename
    filename = function() {corrFileName() },
    content = function(file) {
      ggsave(file,
             plot = correllelogram(),
             device = isolate(input$corrFigDownType),
             dpi = isolate(input$corrFigRes))
    }
  )
  
  #pcaPlotDownloads
  pcaFileName <- reactive({
    name <- paste("PCA-plot", ".", input$pcaFigDownType,
                  sep = "")
    
    return(name)
  })
  
  output$pcaFigDownload <- downloadHandler(
    #getting input is not working for filename
    filename = function() {pcaFileName() },
    content = function(file) {
      ggsave(file,
             plot = pcaPlots(),
             device = isolate(input$pcaFigDownType),
             dpi = isolate(input$pcaFigRes))
    }
  )
  #volcanos---------------------------------------------------------------------------------------------->
  volcFileName <- reactive({
    if (input$mainFigDownTitle == "") {
      
      n <- paste("Volcano-", input$hypoTestMat[volcCycler$counter], ".", input$mainFigDownType,
                 sep = "")
    } else {
      
      n <- paste("Volcano-", input$mainFigDownTitle, ".", input$mainFigDownType,
                 sep = "")
    }
    
    return(n)
  })
  output$VolcDownloader <- downloadHandler(
    #getting input is not working for filename
    filename = function() {volcFileName() },
    content = function(file) {
      ggsave(file, 
             plot = volcPlot(),
             device = isolate(input$mainFigDownType),
             dpi = isolate(input$mainFigRes))
    }
  )
  
  #heatmap------------------------------------------------------------------------------------------>
  heatmapFileName <- reactive({
    if (input$mainFigDownTitle == "") {
      n <- paste("Heatmap-", input$hypoTestMat[HMCycler$counter], ".", input$mainFigDownType,
                 sep = "")
    } else {
      
      n <- paste("Heatmap-", input$mainFigDownTitle, ".", input$mainFigDownType,
                 sep = "")
    }
    return(n)
  })
  
  
  output$HMDownloader <- downloadHandler(
    #getting input is not working for filename
    filename = function() {heatmapFileName() },
    content = function(file) {
      ggsave(file, 
             plot = UserHeatmap(),
             device = isolate(input$mainFigDownType),
             dpi = isolate(input$mainFigRes))
    }
  )
  #----------------------------------------------------------------------------------------------->
  #Webgestalt download
  #volcanos
  enrichmentFileName <- reactive({
    if (input$mainFigDownTitle == "") {
      
      n <- paste("Webgestalt-", input$hypoTestMat[enrichmentCycler$counter], ".", input$mainFigDownType,
                 sep = "")
    } else {
      
      n <- paste("Webgestalt-", input$mainFigDownTitle, ".", input$mainFigDownType,
                 sep = "")
    }
    
    return(n)
  })
  output$webgestalt_downloader <- downloadHandler(
    #getting input is not working for filename
    filename = function() {enrichmentFileName() },
    content = function(file) {
      ggsave(file, 
             plot = enrichment_plot(),
             device = isolate(input$mainFigDownType),
             dpi = isolate(input$mainFigRes))
    }
  )
  
  #StringDownload------------------------------------------------------------------------------------>

  string_url_dl <- reactive({
    
    if (input$string_data_options == "custom") {
      URL <- string_url_builderDL(sig_thresh = input$string_sig_threshold,
                                max_nodes = input$string_max_threshold, 
                                protein_querry = reformat_proteinID(input_ID = string_URL_data(),
                                                                    dtype = input$string_image_extention,
                                                                    data_options = input$string_data_options))
    } else {
      string_df <- string_URL_data()
      URL <- string_url_builderDL(sig_thresh = input$string_sig_threshold,
                                max_nodes = input$string_max_threshold,
                                dtype = input$string_image_extention,
                                protein_querry = reformat_proteinID(input_ID = string_df$UniprotID,
                                                                    data_options = input$string_data_options))
    }
    
    return(URL)
  })
  
  output$string_download_image <- downloadHandler(
    filename = function() {
      if (input$string_image_extention == "highres_image") {
        paste0("String_network", Sys.Date(), ".png")
      } else {
        paste0("String_network", Sys.Date(), ".svg")
      }
      
    },
    content = function(file) {
      GET(string_url_dl(), write_disk(file))
    }
  )

  ###about us tab ###

  
} #server close

shinyApp(ui, server)

