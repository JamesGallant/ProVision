# ProVision
<div>
  <img src="/www/media/vidTut.svg" width="400">
  <img src="/www/media/vidTut.svg" width="400">
</div>
  
ProVision is a data analysis dashboard written in the R-shiny framework. This dashboard is designed to analyse label free and TMT proteomics data downstream from maxQuant.
If you are intersted in trying it out, you can find the online version here: https://provision.shinyapps.io/provision/

The aim of this dashboard is to assist researchers in rapidly analysing their proteomics data without requiring prior knowledge of R or the data analysis workflow for proteomics. It is possible to go from file upload to analysed data and corresponding figures within minutes by following the defaults and some minor customising of the graphs. For those who like to spend more time, it is also possible to play around with the parameters and see what works optimally for a specific experiment by leveraging the reactive environment provided by R-shiny.

If you used and liked our application, please cite us: https://doi.org/10.1093/bioinformatics/btaa620 or download the <a href="www/data/Provision_citation.ris" download="provision_citation.ris">citation file</a> and add it to your citation manager. 
## Requirements 
1.ProteinGroups.txt file from MaxQuant. We have also provided tutorial data to play with. 

2.Sometimes you will need java, because exporting to excel is weird. If the export to excel does not work updata java by following this link:https://www.java.com/en/download/ 

## Outputs:
Statistical plots:
1: Quantile of Quantile plots

2: Histograms

3: Scatterplots

4: Correlation heatmaps

5: Principle component analysis

Main figures:

1: Volcano plots

2: Heatmaps

3: Enrichments

5: String networks

Data:

1: Data processed at each pre-processing step

2: Differential expression

## Documentation

Full documentation as well as a quick start guide is availible online.

## Credits
Packages that makes everything possible: Limma, tidyverse, shiny, shinyjs, shinydashboard, xlsx, pheatmap, data.table, rhandsontable. Also the [webgestalt](http://www.webgestalt.org/) and [STRING](https://string-db.org/) API's for their heavy lifting. 
