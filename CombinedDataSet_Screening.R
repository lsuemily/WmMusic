#start with the library to read an excel file and the psych package#

library(readxl)
FullSetWMmusic <- read_excel("C:/Users/Emily Elliot/Desktop/projects/FullSetWMmusic.xlsx")
View(FullSetWMmusic)
Install.packages("psych")
library(psych)

#describe gives descriptive statistics for the entire dataset
#describe.by gives descriptive statistics for the entire dataset by a grouping variable
describeBy(FullSetWMmusic, FullSetWMmusic$Exp)

#sapply tells you what type of variables you have, such as numeric or character
sapply(FullSetWMmusic, class)

FullSetWMmusic$GENERAL<-as.numeric(FullSetWMmusic$GENERAL)
#correlation of tone span with general score
corr.test (FullSetWMmusic$TspanPartScore, FullSetWMmusic$GENERAL, use = "pairwise")