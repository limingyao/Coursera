corr <- function(directory, threshold = 0) {
  ## 'directory' 是长度为1的字符向量，指明
  ##  CSV 文件的位置
  
  ## 'threshold' 是长度为1的数值向量，指明
  ## 完整观测的案例的数量 (针对所有
  ## 变量) 是必须的，为了计算这两个的相关性：
  ## 硝酸盐(nitrate)和硫酸盐(sulfate); 默认值为 0
  
  ## 返回相关性的数值向量
  
  ret <- complete(directory)
  id <- ret[ret[,2] > threshold,1]  
  corlist <- c()
  for(i in id){
    if(i<10){
      filename <- paste(directory, "/00", i, ".csv", sep = "")
    } else if(i<100){
      filename <- paste(directory, "/0", i, ".csv", sep = "") 
    } else {
      filename <- paste(directory, "/", i, ".csv", sep = "")
    }
    data <- read.csv(filename)
    data <- data[!is.na(data[,2]),]
    data <- data[!is.na(data[,3]),]
    corlist <- c(corlist, cor(data[,2],data[,3]))
  }
  corlist
}