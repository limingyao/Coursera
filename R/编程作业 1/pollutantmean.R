pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' 是长度为1的字符向量，指明
  ## CSV文件的位置
  
  ## 'pollutant' 是长度为1的字符向量，指明
  ## 污染物的名称，我们将会计算其
  ## 平均值; 要么是“硫酸盐(sulfate)”要么是“硝酸盐(nitrate)”
  
  ## 'id'是正整数向量，指明监测点的ID，
  ## 将要被要使用的
  
  ## 返回列表内的所有监测点的污染物平均值，
  ## “id”向量中的 (忽略 NA值)
  
  #filenames <- list.files(directory, pattern = ".csv")
  #datalist <- lapply(filenames, function(name) {
  #  read.csv(paste(directory,"/", name, sep = ""))
  #})
  datalist <- c()
  for(i in id){
    if(i<10){
      filename <- paste(directory, "/00", i, ".csv", sep = "")
    } else if(i<100){
      filename <- paste(directory, "/0", i, ".csv", sep = "")
    } else {
      filename <- paste(directory, "/", i, ".csv", sep = "")
    }
    data <- read.csv(filename)
    data <- data[pollutant]
    datalist <- c(datalist, data[!is.na(data)])
  }
  mean(datalist)
}