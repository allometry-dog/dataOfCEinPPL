rm(list = ls())

choose.dir()# Selected the system path of data 

library(xlsx)
library(tidyverse)
xylemdata <- read.xlsx("data.xlsx", sheetName = "Sheet1", colIndex = 1:10)



tradeoff = function(df)
{
  #Vector O′ = (1, 1) represents a reference vector, and is the most important variable in the framework, which is coincident with CE axis.
  y= c(1,1) 
  
  Ks <- df$Ks
  P50 <- df$P50
  df1 <- data.frame(P50,Ks)
  
  #min‐max normalization to rescale the range in [0, 1]
  df1[,1] <- (df1[,1]-min(df1[,1]))/(max(df1[,1])-min(df1[,1]))
  df1[,2] <- (df1[,2]-min(df1[,2]))/(max(df1[,2])-min(df1[,2]))
 
  y_norm <- sqrt(sum(y^2)) 
  r_norm <- sqrt(rowSums(df1^2)) 
  a_mod <- as.matrix(df1)%*%y/y_norm
  p1 = c(0,0)
  p2 = y
  p3 = df1
  S = (p1[1]-p3[,1])*(p2[2]-p3[,2])-(p1[2]-p3[,2])*(p2[1]-p3[,1])

  a_mod <- as.data.frame(a_mod) %>%
    mutate(sita = case_when(
      S > 0  ~  acos(a_mod/r_norm),
      S < 0 ~   -acos(a_mod/r_norm),
      S ==0 ~ 0))
  a_mod <- as.data.frame(a_mod) %>%
    mutate(seto = a_mod[,1]*tan(a_mod$sita))
  
  df <- mutate(as.data.frame(df), CE = a_mod[, 1])
  df <- mutate(df, θ = a_mod[, 2])
  df <- mutate(df, SETO = a_mod[, 3])
}

library(plyr)
CEandSETO <- ddply(xylemdata, .(organ,species, group2), tradeoff)
CEandSETO <- CEandSETO[order(CEandSETO$group2), ] 
