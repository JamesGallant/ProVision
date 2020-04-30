library(WebGestaltR)
library(dplyr)
enrichment_target_df <- read.delim("2020-04-29-processedMQ.txt", header = TRUE, stringsAsFactors = FALSE, sep = "\t")


enrichment_target_df_down = enrichment_target_df %>%
  filter(significant == "Downregulated") %>%
  select(UniprotID)


enrichment_target_df_up = enrichment_target_df %>%
                                 filter(significant == "Upregulated") %>%
                                 select(UniprotID)


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

ggplot(enrichment_out_data, aes(x = reorder(description,
                                            enrichmentRatio),
                                y = enrichmentRatio)) +
  geom_bar(stat = "identity") +
  coord_flip()


enrichment_out_up <- data.frame(description=c("Hello", "This is a thing",
                                              "It should not be here", "lol still going"),
                                enrichmentRatio=c(55,10,35,20),
                                FDR_plot=c(TRUE, TRUE, FALSE, TRUE))
enrichment_out_down <- data.frame(description=c("Hahaha", "Please stop",
                                                "why why why", "disgusting"),
                                  enrichmentRatio=c(-40,-22,-15,-25),
                                  FDR_plot = c(FALSE, FALSE, TRUE, TRUE))

