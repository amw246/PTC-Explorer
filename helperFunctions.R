

cellSizeRule <- 
  function(metric, cellSize){
    if (cellSize <= 100){
      return("C.S.")
    } else {
      return(formatC(metric ,format='f', digits=1, big.mark = ","))
    }
  }

xtableSummary= function (x){
  if( is.numeric(x)!=TRUE) {stop("Supplied X is not numeric")}
  mysummary = data.frame(
    "Min." =as.numeric( min(x, na.rm = TRUE)),
    "1st Qu." = quantile(x, na.rm = TRUE)[2],
    "Median" = median(x, na.rm = TRUE),
    "Mean" = mean(x, na.rm = TRUE),
    "3rd Qu." = quantile(x, na.rm = TRUE)[4],
    "Max." = max(x, na.rm = TRUE),
    "NA's" = sum(as.numeric(is.na(x))),
    row.names=""
    
  )
  names(mysummary) = c("Min.","1st Qu.","Median","Mean","3rd Qu.","Max.")
  return( mysummary )
}

xtableSummary2vars= function (x, y, df){
  # if( is.numeric(x)!=TRUE) {stop("Supplied X is not numeric")}
  mysummary = data.frame(
    # "Min." =as.numeric(aggregate(x ~ y, data = df, fun = min),
    # "1st Qu." = quantile(x, na.rm = TRUE)[2],
    # "Median" = median(x, na.rm = TRUE),
    # "Mean" = mean(x, na.rm = TRUE),
    # "3rd Qu." = quantile(x, na.rm = TRUE)[4],
    # "Max." = max(x, na.rm = TRUE),
    # "NA's" = sum(as.numeric(is.na(x))),
    aggregate(x ~ y, data = df, FUN = quantile),
    row.names=levels(y)
    
  )
  names(mysummary) = c("Min.","1st Qu.","Median","3rd Qu.","Max.")
  mysummary$Mean <- aggregate(x ~ y, data = df, FUN = mean)[2]
  return( mysummary )
}


ftable2data.frame <- function(x,...){
  y <- format(x,quote=FALSE)
  z <- data.frame(y[-1,],stringsAsFactors=FALSE)
  names(z) <- y[1,]
  z
}


createMetricTable <- 
  function(df, varList){
    #trim the blanks
    varList <- varList[varList != ""]
  if(length(varList) == 3 ){
    var1 <- df[[ varList[[1]] ]]
    var2 <- df[[ varList[[2]] ]]
    var3 <- df[[ varList[[3]] ]]
    
    metricMatrix <- ftable(prop.table(table(var1, var2, var3),2)*100)
    cellSizeMatrix <- ftable(table(var1, var2, var3))
    print(metricMatrix)
    print(cellSizeMatrix)
    finalMatrix <- mapply(cellSizeRule, metricMatrix, cellSizeMatrix)
    print(finalMatrix)
    dim(finalMatrix) <- dim(cellSizeMatrix)
    print(finalMatrix)
    print(rownames(cellSizeMatrix))
    rownames(finalMatrix) <- rownames(cellSizeMatrix)
    # 
    # colNameVector <- levels(interaction(var2, var3, sep = " ", drop = TRUE, lex.order = TRUE))
    # colnames(finalMatrix) <- colNameVector


    # colnames(finalMatrix) <- colnames(cellSizeMatrix)
    finalMatrix <- as.data.frame(finalMatrix)
    return(finalMatrix)
  } else  if(length(varList) == 2 ){
    var1 <- df[[ varList[[1]] ]]
    var2 <- df[[ varList[[2]] ]]
    metricMatrix <- prop.table(table(var1, var2),2)*100
    cellSizeMatrix <- table(var1, var2)
    finalMatrix <- mapply(cellSizeRule, metricMatrix, cellSizeMatrix)  
    dim(finalMatrix) <- dim(cellSizeMatrix)
    rownames(finalMatrix) <- rownames(cellSizeMatrix)
    colnames(finalMatrix) <- colnames(cellSizeMatrix)
    finalMatrix <- as.data.frame(finalMatrix)
    return(finalMatrix)
  } else if(length(varList) == 1 ){
    var1 <- df[[ varList[[1]] ]]
    if(class(var1) == "character" | class(var1) == "factor" | class(var1) == "Date"){
      metricMatrix <- prop.table(table(var1))*100
      cellSizeMatrix <- table(var1)
      finalMatrix <- mapply(cellSizeRule, metricMatrix, cellSizeMatrix)
      finalMatrix <- as.matrix(finalMatrix)
      colnames(finalMatrix) <- "%"
      # finalMatrix <- t(finalMatrix)
      return(finalMatrix)
    } else if (class(var1) == "numeric"){
      # finalMatrix <- summary(var1)
      # print(finalMatrix)
      # return(finalMatrix)
      xtableSummary(var1)
    }
    
  }
}

