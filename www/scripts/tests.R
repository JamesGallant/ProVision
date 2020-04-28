library(WebGestaltR)
library(dplyr)
enrichment_target_df <- read.delim("stats.txt", header = TRUE, stringsAsFactors = FALSE, sep = "\t")


enrichment_target_df_down = enrichment_target_df %>%
  filter(Significant == "downregulated") %>%
  select(Uniprot)


enrichment_target_df_up = enrichment_target_df %>%
                                 filter(Significant == "upregulated") %>%
                                 select(Uniprot)


enrichment_target_listf_up <- lapply(1:length(rownames(enrichment_target_df_up)), function(x){
  return(enrichment_target_df_up[x,])
})

enrichment_target_listf_down <- lapply(1:length(rownames(enrichment_target_df_down)), function(x){
  return(enrichment_target_df_down[x,])
})


#ORA FDR
enrichment_out_up <- WebGestaltR(enrichMethod = "ORA",
                              interestGene = enrichment_target_listf_up,
                              isOutput = FALSE,
                              interestGeneType = "uniprotswissprot",
                              enrichDatabase = "geneontology_Biological_Process",
                              organism = "hsapiens",
                              referenceSet = "genome_protein-coding",
                              projectName = "User",
                              sigMethod = "fdr",
                              fdrMethod = "BH",
                              fdrThr = 0.05
                              )

enrichment_out_down <- WebGestaltR(enrichMethod = "ORA",
                                 interestGene = enrichment_target_listf_down,
                                 isOutput = FALSE,
                                 interestGeneType = "uniprotswissprot",
                                 enrichDatabase = "geneontology_Biological_Process",
                                 organism = "hsapiens",
                                 referenceSet = "genome_protein-coding",
                                 projectName = "User",
                                 sigMethod = "fdr",
                                 fdrMethod = "BH",
                                 fdrThr = 0.05
)

if (!is.null(enrichment_out_up)) {
  enrichment_out_up = enrichment_out_up %>%
    arrange(desc(enrichmentRatio)) %>%
    top_n(10)
}

if (!is.null(enrichment_out_down)) {
  enrichment_out_down = enrichment_out_down %>%
    arrange(desc(enrichmentRatio)) %>%
    top_n(10)
  
  enrichment_out_down$enrichmentRatio <- enrichment_out_down$enrichmentRatio * - 1
  
}

enrichment_out_data <- rbind(enrichment_out_up, enrichment_out_down)


enrichment_df_ora <- rbind(enrichment_out_down, enrichment_out_up)
#ORA top
enrichment_out <- WebGestaltR(enrichMethod = "ORA",
                              outputDirectory = getwd()
                              interestGene = listsof,
                              interestGene = listsof,
                              interestGeneType = "uniprotswissprot",
                              enrichDatabase = "geneontology_Biological_Process",
                              organism = "hsapiens",
                              referenceSet = "genome_protein-coding",
                              projectName = "User",
                              sigMethod = "Top",
                              topThr = 10
                              
)

