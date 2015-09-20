complete <- function(directory, id = 1:332) {
  ## 'directory' 是长度为1的字符向量，指明
  ##  CSV 文件的位置
  
  ## 'id' 是正整数向量，指明监测点的ID号，
  ## 将要被使用的
  
  ## 返回以下格式的数据帧：
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## 其中'id' 是监测点ID编号，而'nobs'是
  ## 完整案例的数量
  ids <- c()
  nobs <- c()
  for(i in id){
    if(i<10){
      filename <- paste(directory, "/00", i, ".csv", sep = "")
    } else if(i<100){
      filename <- paste(directory, "/0", i, ".csv", sep = "") 
    } else {
      filename <- paste(directory, "/", i, ".csv", sep = "")
    }
    data <- read.csv(filename)
    class(data)
    data <- data[!is.na(data[,c(2)]),]
    data <- data[!is.na(data[,c(3)]),]
    ids <- c(ids, i)
    nobs <- c(nobs, length(data[,1]))
  }
  data.frame(ids,nobs)
}