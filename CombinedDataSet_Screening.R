#start with the library to read an excel file and the psych package#

library(readxl)
FullSetWMmusic <- read_excel("C:/Users/Emily Elliot/Desktop/projects/FullSetWMmusic.xlsx")
View(FullSetWMmusic)
install.packages("psych")
install.packages("ggplot2")
install.packages("stringr")
install.packages("data.table")
library(psych)
library(ggplot2)
library(stringr)
library(data.table)

#describe gives descriptive statistics for the entire dataset
#describe.by gives descriptive statistics for the entire dataset by a grouping variable
describeBy(FullSetWMmusic, FullSetWMmusic$Exp)

#sapply tells you what type of variables you have, such as numeric or character
sapply(FullSetWMmusic, class)

FullSetWMmusic$GENERAL<-as.numeric(FullSetWMmusic$GENERAL)
FullSetWMmusic$ACTIVE<-as.numeric(FullSetWMmusic$ACTIVE)
FullSetWMmusic$PERCEPTUAL<-as.numeric(FullSetWMmusic$PERCEPTUAL)
FullSetWMmusic$MUSICAL<-as.numeric(FullSetWMmusic$MUSICAL)
FullSetWMmusic$SINGING<-as.numeric(FullSetWMmusic$SINGING)
FullSetWMmusic$EMOTIONS<-as.numeric(FullSetWMmusic$EMOTIONS)
#correlation of tone span with general score
corr.test (FullSetWMmusic$TspanPartScore, FullSetWMmusic$GENERAL, use = "pairwise")

#separate by experiment variable
exp1 <- FullSetWMmusic[which(FullSetWMmusic$Exp=='1'),names(FullSetWMmusic) %in% c("TspanPartScore", "GENERAL")]
corr.test (exp1, use = "pairwise")
exp2 <- FullSetWMmusic[which(FullSetWMmusic$Exp=='2'),names(FullSetWMmusic) %in% c("TspanPartScore", "GENERAL")]
corr.test (exp2, use = "pairwise")

pairs.panels(exp1, lm = TRUE, stars = TRUE)
pairs.panels(exp2, lm = TRUE, stars = TRUE)

FullSetWMmusic$Gender <- str_to_lower(FullSetWMmusic$Gender)
FullSetWMmusic$Gender <- str_replace(FullSetWMmusic$Gender,"^.*f.*$", "Female")
FullSetWMmusic$Gender <- str_replace(FullSetWMmusic$Gender,"^m.*$", "Male")
FullSetWMmusic$Gender <- str_replace(FullSetWMmusic$Gender,"^country$", "No Response")
FullSetWMmusic$Gender[30] <- "No Response"
FullSetWMmusic$Gender[63] <- "No Response"


FullSetWMmusic$Gender <- str_replace(FullSetWMmusic$Gender,"^nonbinary$", "No Response")
FullSetWMmusic$Gender <- str_replace(FullSetWMmusic$Gender,"^sheher$", "No Response")
FullSetWMmusic$Gender <- str_replace(FullSetWMmusic$Gender,"^agender$", "No Response")
FullSetWMmusic$Gender <- as.factor(FullSetWMmusic$Gender)
table(FullSetWMmusic$Gender)

#look for univariate outliers in GMSI subscales and General score
pairs.panels(FullSetWMmusic[,3:8], lm = TRUE)
#make a histogram
hist(FullSetWMmusic$GENERAL)
#make z scores
gmsi.z.scores<-apply(FullSetWMmusic[3:8],2,scale)
FullSetWMmusic.with.z <- cbind(FullSetWMmusic, gmsi.z.scores)
#look for values larger than 2.5
table(gmsi.z.scores > 2.5)
#view the names of the variables in the new data and then look for individual values that might be a problem
names(FullSetWMmusic.with.z)
gmsi.z.indexer<-gmsi.z.scores >2
gmsi.z.scores[gmsi.z.indexer]


