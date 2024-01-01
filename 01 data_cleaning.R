while(TRUE){
  
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(httpgd)
  library(rsq) 
  library(DescTools)
  library(ggthemes)
  library(ggh4x)
  break
}

data <- tibble(read.csv("raw_data/all_data_long_3.csv", header = TRUE, sep = ","))

# add a small number to the nac and dfo column to avoid 0 values
data <- mutate(data, nac = nac+0.00001 , dfo = dfo+0.00001)
write.csv(data, "clean_data/all_data_long_4_non_0.csv", row.names = FALSE)
