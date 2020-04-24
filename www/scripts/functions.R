
filterValidVals <- function(x, in_one, user_val) {
  #count reps and get groups
  if (in_one == "one_group") {
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



