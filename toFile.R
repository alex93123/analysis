toFile<- function(filename, data){
  j<-2
  k<-1
  for (i in 1:(length(data)/2)){
    if (nchar(data[[k]])<29){
      writeWorksheetToFile(filename, list(data[[k]]), paste0(i,") ",data[[k]]),startRow = 1, startCol = 1)
      writeWorksheetToFile(filename, list(rownames(data[[j]])), paste0(i,") ",data[[k]]),startRow = 4, startCol = 1)
      writeWorksheetToFile(filename, list(data[[j]]), paste0(i,") ",data[[k]]),startRow = 4, startCol = 2)
    } else { 
      writeWorksheetToFile(filename, list(data[[k]]), paste("sheet",i),startRow = 1, startCol = 1)
      writeWorksheetToFile(filename, list(rownames(data[[j]])), paste("sheet",i),startRow = 4, startCol = 1)
      writeWorksheetToFile(filename, list(data[[j]]), paste("sheet",i),startRow = 4, startCol = 2)
    }
    j<-j+2
    k<-k+2
  }
}
