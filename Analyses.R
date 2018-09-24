library(readxl)
Data_I_II_no_outliers <- read_excel("C:/Users/Emily Elliot/Desktop/projects/WmMusic/Data_I_II_no_outliers.xlsx")
View(Data_I_II_no_outliers)
Data_I_II <- Data_I_II_no_outliers

install.packages("apaTables", dependencies = TRUE)
install.packages("tidyverse", dependencies = TRUE)
install.packages("ez", dependencies = TRUE)

library(apaTables)
library(tidyverse)
#--------------------------------------------------
library(data.table)
library(ggplot2)
library(psych)
library(stringr)

glimpse(Data_I_II)
#dbl means double-precision floating point number from Stanley & Spence, 2018

gmsiwmcgf<- subset(Data_I_II, select=c("GENERAL", "ACTIVE", "PERCEPTUAL", "MUSICAL", "SINGING", "EMOTIONS",
                                            "RavenTotal","OspanPartialScore", "SspanPartiaScore", 
                                            "TspanPartScore"))

#create correlation table from Stanley & Spence, 2018
table1 <- apa.cor.table(gmsiwmcgf, filename = "Table1.doc", table.number = 1)
print(table1)
##regression of working memory span tasks predicting General Score
WMCspan = lm(formula = GENERAL ~ OspanPartialScore + SspanPartiaScore + TspanPartScore, data = gmsiwmcgf)
WMCspan
summary(WMCspan)
table2 <- apa.reg.table(WMCspan, filename = "Table2.doc", table.number = 2)

block1 <- lm(formula = GENERAL ~ OspanPartialScore + SspanPartiaScore + TspanPartScore, data = gmsiwmcgf)
block2 <- lm(formula = GENERAL ~ OspanPartialScore + SspanPartiaScore + TspanPartScore + RavenTotal, data = gmsiwmcgf)
table3 <- apa.reg.table(block1, block2, filename = "Table3.doc", table.number = 3)

WMC_only<- subset(Data_I_II, select=c("OspanPartialScore", "OspanMathError", "SspanPartiaScore", "SymmErrTot", "TspanPartScore", 
                                      "ToneMathErrTot"))
appendix <- apa.cor.table(WMC_only, filename = "Appendix.doc", table.number = 8)
