## This R file will be used to run a cluster analysis exploring the number of behaviors endorsed wihtin each of the DPICs tasks
## I will use the same classification from the HMM analyses -- but instead of one Pride, Neutral, and Dont count taken across all tasks 
## I will grab a Pride, Neutral, and Dont within each task
## I will then cluster these counts in 2 different manners:
## 1. Wave 1 versus Wave 3
## 2. Wave 1 & Wave 3 together

## Clean slate
rm(list = ls())

## Load library(s)
library("readxl")
library("ggplot2")
library("tidyverse")
source("~/Documents/Github/adroseHelperScripts/R/afgrHelpFunc.R")
library("reshape2")
library("ggstatsplot")

## Load data
more.dat1 <- foreign::read.spss("~/Documents/oregonDPICS/data/W1_rawDataEpochNums.sav", to.data.frame = TRUE)
more.dat2 <- foreign::read.spss("~/Documents/oregonDPICS/data/W2_rawDataEpochNums.sav", to.data.frame = TRUE)
more.dat3 <- foreign::read.spss("~/Documents/oregonDPICS/data/W3_rawDataEpochNums.sav", to.data.frame = TRUE)
caps.data <- readRDS("~/Documents/eegOregon/data/capsData.RDS")

## Clean the .sav files
more.dat1$Behavior <- str_squish(more.dat1$Behavior)
more.dat2$Behavior <- str_squish(more.dat2$Behavior)
more.dat3$Behavior <- str_squish(more.dat3$Behavior)
more.dat1$Behavior <- gsub(pattern = " ", replacement = "_", x = more.dat1$Behavior)
more.dat2$Behavior <- gsub(pattern = " ", replacement = "_", x = more.dat2$Behavior)
more.dat3$Behavior <- gsub(pattern = " ", replacement = "_", x = more.dat3$Behavior)
## Now make the noncompliable commands a distinct flavor
more.dat1$Behavior[which(more.dat1$Modifier_1=="No Opportunity")] <- paste(more.dat1$Behavior[which(more.dat1$Modifier_1=="No Opportunity")], "_NO", sep='') 
more.dat2$Behavior[which(more.dat2$Modifier_1=="No Opportunity")] <- paste(more.dat2$Behavior[which(more.dat2$Modifier_1=="No Opportunity")], "_NO", sep='') 
more.dat3$Behavior[which(more.dat3$Modifier_1=="No Opportunity")] <- paste(more.dat3$Behavior[which(more.dat3$Modifier_1=="No Opportunity")], "_NO", sep='') 
## Now make all of the observations into subj indiciators
more.dat1$participant <- str_extract(more.dat1$Observation, "CAPS[0-9][0-9][0-9]")
more.dat2$participant <- str_extract(more.dat2$Observation, "CAPS[0-9][0-9][0-9]")
more.dat3$participant <- str_extract(more.dat3$Observation, "CAPS[0-9][0-9][0-9]")

## Now try to organize data, so the columns are the actions that can be taken from the parent (indirect command, neutral talk.. et cetera), the rows are still the time, but it is a binary indiciator for when an event occurs
more.dat1$EPOCH <- 1
## Fix some broken dpics taks values here
more.dat1[which(!more.dat1$DPICS_Task %in% c("CLED", "PLED", "CLUP")),"DPICS_Task"] <- "PLED"
wide.form1.T <- reshape2::dcast(more.dat1, participant ~ Behavior + DPICS_Task, fun.aggregate = sum, value.var = "EPOCH")

more.dat2$EPOCH <- 1
wide.form2.T <- reshape2::dcast(unique(more.dat2), participant ~ Behavior + DPICS_Task, fun.aggregate = sum, value.var = "EPOCH")

more.dat3$EPOCH <- 1
wide.form3.T <- reshape2::dcast(unique(more.dat3), participant ~ Behavior + DPICS_Task, fun.aggregate = sum, value.var = "EPOCH")

## Now create the pride neutral and dont columns within each task
## potentially separate the neutral talk and questions from the neutral values
## Here is the coding taken from the HMM code
# all.counts$PrideCL <- rowSums(all.counts[,c("LP", "Re", "BD")])
# all.counts$PrideCL <- rowSums(all.counts[,c("LP", "Re", "BD","UP")])
# all.counts$NeutralCL <- all.counts[,c("NT")]
# all.counts$DontCL <- rowSums(all.counts[,c("DC", "DCn", "IC","ICn","NeT", "QU")])
# all.counts$PridePL <- rowSums(all.counts[,c("LP", "UP","Re", "BD", "DC", "IC")])
# all.counts$NeutralPL <- rowSums(all.counts[,c("NT", "QU")])
# all.counts$DontPL <- rowSums(all.counts[,c("NeT", "DCn","ICn")])
## Transcript here:
# Indirect_Command:IC
# Neutral_Talk:NT
# Question:QU
# Reflection:Re
# Unlabeled_Praise: UP
# Behavior_Description:BD
# Direct_Command: DC
# Negative_Talk: NeT
# Labeled_Praise: LP
# Direct_Command_NO: DCn
# Indirect_Command_no: ICn
## Automate this process down here
collpaseVars <- list()
collpaseVars[[1]] <- c("Behavior_Description_CLED", "Labeled_Praise_CLED", 
                       "Reflection_CLED", "Unlabeled_Praise_CLED")
collpaseVars[[2]] <- c("Neutral_Talk_CLED")
collpaseVars[[3]] <- c("Direct_Command_CLED", "Direct_Command_NO_CLED", 
                       "Indirect_Command_CLED", "Indirect_Command_NO_CLED",
                       "Negative_Talk_CLED", "Question_CLED")
collpaseVars[[4]] <- c("Behavior_Description_XXXX", "Labeled_Praise_XXXX", 
                       "Reflection_XXXX", "Unlabeled_Praise_XXXX",
                       "Direct_Command_XXXX", "Indirect_Command_XXXX") # rowSums(all.counts[,c("LP", "UP","Re", "BD", "DC", "IC")])
collpaseVars[[5]] <- c("Neutral_Talk_XXXX", "Question_XXXX")
collpaseVars[[6]] <- c("Direct_Command_NO_XXXX", "Indirect_Command_NO_XXXX","Negative_Talk_XXXX") # rowSums(all.counts[,c("NeT", "DCn","ICn")])
for(i in 1:9){
  list.ind <- i
  blockVal <- "CLED"
  if(i %in% c(1,4,7)){colVal <- "PRIDE_"}
  if(i %in% c(2,5,8)){colVal <- "Neutral_"}
  if(i %in% c(3,6,9)){colVal <- "Dont_"}
  if(list.ind>3){blockVal <- "PLED"}
  if(list.ind>6){
    list.ind <- list.ind - 3
    blockVal <- "CLUP"
  }
  for(z in 1:3){
    ## Grab the vars to collapse across
    tmp.vals <- get(paste("wide.form", z, ".T", sep=''))
    outputVals <- rep(NA, dim(tmp.vals)[1])
    varsInt <- collpaseVars[[list.ind]]
    if(blockVal>3){
      varsInt <- gsub(pattern = "XXXX", replacement = blockVal, x = varsInt)
    }
    valsSum <- tmp.vals[,varsInt]
    if(!identical(NULL, dim(valsSum))){
      outputVals <- rowSums(tmp.vals[,varsInt])
    }else{
      outputVals <- valsSum
    }
    out.col <- paste(colVal, blockVal, sep='')
    tmp.vals[out.col] <- outputVals
    assign(paste("wide.form", z, ".T", sep=''), tmp.vals)
  }
}

## Now look for univaraite outliers

findoutlier <- function(x) {
  return(x < quantile(x, .25) - 1.5*IQR(x) | x > quantile(x, .75) + 1.5*IQR(x))
}

plot.dat <- melt(wide.form1.T[,c(1,39:47)], id.vars = "participant") %>% 
  group_by(variable) %>% 
  mutate(outlier = ifelse(findoutlier(value), participant, NA))

## Now plot the bivaraite relationships
GGally::ggpairs(wide.form1.T[,c(39:47)])

plot.dat3 <- melt(wide.form3.T[,c(1,38:46)], id.vars = "participant") %>% 
  group_by(variable) %>% 
  mutate(outlier = ifelse(findoutlier(value), participant, NA))

GGally::ggpairs(wide.form3.T[,c(38:46)])

p1 <- ggplot(plot.dat, aes(x=variable, y=value)) +
  geom_boxplot() +
  ggrepel::geom_label_repel(aes(label=outlier), na.rm=TRUE) +
  ggtitle("WAVE 1")

## Here are the W1 egregious outliers:
# CAPS 405
# CAPS 271
# CAPS 111
## Now view each of these
View(more.dat1[which(more.dat1$participant=="CAPS405" & more.dat1$DPICS_Task=="PLED"),])
View(more.dat1[which(more.dat1$participant=="CAPS271" & more.dat1$DPICS_Task=="CLUP"),])
View(more.dat1[which(more.dat1$participant=="CAPS111" & more.dat1$DPICS_Task=="CLUP"),])

p3 <- ggplot(plot.dat3, aes(x=variable, y=value)) +
  geom_boxplot() +
  #geom_text(aes(label=outlier), na.rm=TRUE, hjust=-.25) +
  ggrepel::geom_label_repel(aes(label=outlier), na.rm=TRUE) +
  ggtitle("WAVE 3")
## No outliers above 100 for the W3
## Here are the W3 egregious outliers:
# CAPS 271
# CAPS 285

## Now view each of these
# View(more.dat3[which(more.dat3$participant=="CAPS271" & more.dat3$DPICS_Task=="CLUP"),])

multiplot(p1, p3, cols = 1)
