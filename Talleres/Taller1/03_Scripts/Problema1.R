library(readr)
library(purrr)
library(dplyr)
library(tidyr)
library(foreign)

readDBF <- function(filename){
  print(paste0("opening ",filename))
  filepath <- paste0("Data/Mortalidad/DBF_Files/",filename)
  df <- read.dbf(filepath, as.is=T)
  df <- df %>% mutate(across(everything(), as.character))
  df$fileName <- filename
  return(df)
}

file.names <- list.files(path="Data/Mortalidad/DBF_Files/")
combinedData <- file.names %>% map(readDBF) %>% list_rbind()



x0 <- 108727; a <- 7**5; m <- 2**31 - 1;

x  <- x0; 
for (jj in 2:30){
  x[jj] <- (a * x[jj-1]) %% m
}
samples <- x / m

Pn <- ecdf(samples)
