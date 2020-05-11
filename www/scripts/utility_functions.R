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

imputeFunc = function(x, width, downshift, centerMedian) {
  kol.name <- as.data.frame(table(unlist(names(x))))
  kol.name <- as.character(kol.name$Var1)
  
  
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

ora_list <- function(ora_data, regulation) {
  df = ora_data %>% 
    filter(significant == regulation) %>%
    select(UniprotID)
  
  enrichment_list <- lapply(1:length(rownames(df)), function(x){
    return(df[x,])
  })
  
  return(enrichment_list)
}

gsea_df <- function(gsea_data, enrichment_options, sig_cutoff){
  if (enrichment_options == "strict") {
    df = gsea_data %>% 
      filter(significant != "Non_significant") %>%
      select(UniprotID, EffectSize)
  } else if (enrichment_options == "lenient") {
    df = gsea_data %>%
      filter(qValue < sig_cutoff) %>%
      select(UniprotID, EffectSize)
  } else if (enrichment_options == "all") {
    df = gsea_data %>%
      select(UniprotID, EffectSize)
  }
  
  return(df)
}

gsea_webgestalt_df <- function(data, topN){
  BooleanEnrichments <- sapply(data$normalizedEnrichmentScore, function(x){
    if (x > 0) {
      return(TRUE)
    } else{
      return(FALSE)
    }
  })
  
  
  if (TRUE %in% BooleanEnrichments) {
    df_out = data %>%
      filter(normalizedEnrichmentScore > 0) %>%
      arrange(desc(normalizedEnrichmentScore)) %>%
      top_n(topN)
  } else if (FALSE %in% BooleanEnrichments) {
    df_out = data %>%
      filter(normalizedEnrichmentScore < 0) %>%
      arrange(normalizedEnrichmentScore) %>%
      top_n(topN)
  } else if (TRUE %in% BooleanEnrichments && FALSE %in% BooleanEnrichments) {
    df_down = data %>%
      filter(normalizedEnrichmentScore < 0) %>%
      arrange(normalizedEnrichmentScore) %>%
      top_n(topN)
    
    df_up = data %>%
      filter(normalizedEnrichmentScore > 0) %>%
      arrange(desc(normalizedEnrichmentScore)) %>%
      top_n(topN)
    
    df_out <- rbind(df_down, df_up)
  }
  
  validate(need(df_out$normalizedEnrichmentScore, 
                message = "Recalculate enrichment"))
  return(df_out)
}

error_message_webgestalt <- function(inputCall) {
  err_input <- unlist(strsplit(inputCall, "_"))
  return(paste(err_input[2], err_input[1], sep = " ")) 
}

dynamic_stats_dataframe <- function(generator, 
                                    test_matrix, 
                                    bayesian_fit, 
                                    combination, 
                                    correction, 
                                    q_value_cutoff, 
                                    FC_cutoff, 
                                    counter, 
                                    raiseError) {
  if (generator > 0) {
    data_list <- list()
    for (test in 1:length(test_matrix)) {
      fit2 <- bayesian_fit
      stat_Comb <- combination
      df <- data.frame(ID = names(fit2$coefficients[,test]),
                       pValue = fit2$p.value[,test],
                       qValue = p.adjust(fit2$p.value[,test], correction),
                       EffectSize = fit2$coefficients[,test],
                       comparison = statComb[test])
      
      df<- mutate(df, 
                  significant = ifelse(df$EffectSize > FC_cutoff & round(df$qValue, 3) < q_value_cutoff, 
                                       "Upregulated",
                                       ifelse(df$EffectSize < (dFC_cutofff * -1) & round(df$qValue, 3) < q_value_cutoff, 
                                              "Downregulated", "Non_significant")))
      
      d2 <- data.frame(df,
                       colsplit(string = df$ID, 
                                pattern = "_", 
                                names = c("UniprotID", "GeneName")))
      
      data_name <- test_matrix[test]
      data_list[[data_name]] = d2
    }
    target_df <- data_list[[counter]]
    
    validate(need(target_df, 
                  message = raiseError))
    
    return(target_df)
  }
}


reformat_proteinID <- function(input_ID, data_options){
  if (data_options == "custom") {
    out <- gsub(pattern = ",", replacement = "%0A", x = input_ID)
    
  } else {
    out <- gsub(pattern = " ",
                "%0A", 
                x =  paste(input_ID, collapse = " "))
    
  }
  return(out)
}

string_url_builder <- function(protein_querry, sig_thresh, max_nodes) {
  validate(need(protein_querry > 0,
                message = "No proteins availible"))
  if (protein_querry == 1) {
    data <- "network?identifier="
    
  } else {
    data <- "networkList?identifiers="
    
    protein_querry <- paste(protein_querry,collapse=" ")
    
  }
  
  
  URL <- paste0("http://string-db.org/api/image/",
                data,
                protein_querry,
                "&required_score=",
                sig_thresh,
                "&limit=",
                max_nodes,
                "&network_flavor=evidence")
  
  return(URL)
  
  
}
