library(readxl)
library(dplyr)
library(janitor)


Incendie <- read_excel("data/Incendie.xlsx") %>%
  janitor::clean_names()

cumulative <- Incendie %>% 
  select(-uw_year) %>% 
  apply(., MARGIN = 1, FUN = cumsum) %>% 
  t()

developpementFactor <- function(cumulativeTriangle = cumulative){
  n <- ncol(cumulativeTriangle)
  devfactor <- sum(cumulativeTriangle[1:(n-1), 2])/sum(cumulativeTriangle[1:(n-1), 1])
  for( i in 2:(n-1)){
    devfactor <- c(devfactor,
                   sum(cumulativeTriangle[1:(n-i), i + 1])/sum(cumulativeTriangle[1:(n-i), i]))
  }
  return(devfactor)
}

df <- developpementFactor()

chainladder <- function(cumulativetriangle= cumulative, developpementfactor = df){
  rows <- nrow(cumulativetriangle)
  for(i in 1:(rows-1)){
    for(j in i:(rows-1)){
      cumulativetriangle[rows-i+1, j + 1] <- cumulativetriangle[rows-i+1, j] * developpementfactor[j]
    }
  }
  dif <- 0
  for(i in 2:rows){
    dif <- c(dif, cumulativetriangle[i, rows] - cumulativetriangle[i,rows - i+1])
  }
  return(list(CumulativeMatrix = cumulativetriangle,
              developpementfactor = developpementfactor,
              diff = dif))
}


result <- chainladder()
result