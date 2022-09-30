if(require("rjson") == F){
  install.packages("rjson")
}

if(require("reshape2") == F){
  install.packages("reshape2")
}
   
phenoMLtoCaseB <- function(inputDir, varNames = c("value", "well", "concentration", "agent")){
  
  groupingVariables <- varNames[-which(varNames == "value")]
  
  if(!"time" %in% varNames){sequenceAsTime <- T}
  
  data_in <- fromJSON(file = inputDir, simplify = T)
  
  data_out <- reshape2::melt(data_in)
  
  if(exists("varNames"))if(length(varNames) == ncol(data_out)){colnames(data_out) <- varNames}else{stop("varNames does not agree with input data")}
  
  if(!"value" %in% colnames(data_out)){stop("column value is missing from input data")}
  
  if(sequenceAsTime){
    
    data_out <- base::split(data_out, do.call(paste, data_out[,groupingVariables]))
    
    lapply(data_out, function(idx){
      
      idx$time <- 1:nrow(idx)
      
      return(idx)
      
    }) -> data_out
    
    data_out <- do.call(rbind, data_out)
    
  }
  
  rownames(data_out) <- NULL
  
  return(data_out)
  
}
