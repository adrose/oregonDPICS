## Clean slate
rm(list = ls())

## Load library(s)
library("readxl")
library("ggplot2")
library("depmixS4")
library("mHMMbayes")
library("tidyverse")
source("~/Documents/Github/adroseHelperScripts/R/afgrHelpFunc.R")
library("reshape2")
library("rstan")

## Load data
more.dat1 <- foreign::read.spss("~/Documents/oregonDPICS/data/W1_rawDataEpochNums.sav", to.data.frame = TRUE)
more.dat2 <- foreign::read.spss("~/Documents/oregonDPICS/data/W2_rawDataEpochNums.sav", to.data.frame = TRUE)
more.dat3 <- foreign::read.spss("~/Documents/oregonDPICS/data/W3_rawDataEpochNums.sav", to.data.frame = TRUE)

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


## Now try to organize data, so the columns are the actions that can be taken from the parent (indirect command, neutral talk.. et cetera), the rows are still the time, but it is a binary indiciator for when an event occurs
wide.form1.T <- reshape2::dcast(more.dat1, Observation + Time_Relative_sf ~ Behavior, fun.aggregate = length)
## Some values == 2?
## Observation CAPS181-CLUP-H2 ## Time: 292.08600
more.dat1[more.dat1$Observation=="CAPS181-CLUP-H2" & more.dat1$Time_Relative_sf =="292.086",]
#       Observation Time_Relative_sf Time_Relative_hmsf       Behavior     Modifier_1  FAMILY DPICS_Task EPOCH
# 31874 CAPS181-CLUP-H2          292.086              292.1 Direct_Command No Opportunity CAPS181       CLUP    10
# 31875 CAPS181-CLUP-H2          292.086              292.1 Direct_Command No Opportunity CAPS181       CLUP    10
## Looks like I do have identical rows -- I am just going to take all unique rows
wide.form1.T <- reshape2::dcast(unique(more.dat1), Observation + Time_Relative_sf ~ Behavior, fun.aggregate = length)
wide.form2.T <- reshape2::dcast(unique(more.dat2), Observation + Time_Relative_sf ~ Behavior, fun.aggregate = length)

## Still seeing values > 1 with the unique fix for values from 2
## Observation: CAPS195-CLUP-H2 & Time_Relative_sf: 5.57212
more.dat2[more.dat2$Observation=="CAPS195-CLUP-H2" & more.dat2$Time_Relative_sf=="5.57212",]
#     Observation Time_Relative_sf Time_Relative_hmsf       Behavior     Modifier_1  FAMILY DPICS_Task EPOCH
# 8270 CAPS195-CLUP-H2          5.57212                5.6 Direct_Command Comply         CAPS195       CLUP     1
# 8426 CAPS195-CLUP-H2          5.57212                5.6 Direct_Command Comply         CAPS194       CLUP     1
## Looks like the family got messed up
df.2.index <- duplicated(more.dat2[,c("Observation", "Time_Relative_sf", "Behavior", "FAMILY")])
wide.form2.T <- reshape2::dcast(unique(more.dat2), Observation + Time_Relative_sf ~ Behavior, fun.aggregate = length)
wide.form3.T <- reshape2::dcast(unique(more.dat3), Observation + Time_Relative_sf ~ Behavior, fun.aggregate = length)

## go through and correct the 2 values
iter.vals <- colnames(wide.form2.T)[3:15]
for(i in iter.vals){
  length.fix <- sum(wide.form2.T[,i] > 1)
  if(length.fix > 0){
    wide.form2.T[which(wide.form2.T[,i]>1),i] <- 1
  }
}

## Identify the block of the study here
wide.form1.T$Block <- "CLED"
wide.form1.T$Block[grep(pattern = "PLED", x = wide.form1.T$Observation)] <- "PLED"
wide.form1.T$Block[grep(pattern = "CLUP", x = wide.form1.T$Observation)] <- "CLUP"
wide.form2.T$Block <- "CLED"
wide.form2.T$Block[grep(pattern = "PLED", x = wide.form2.T$Observation)] <- "PLED"
wide.form2.T$Block[grep(pattern = "CLUP", x = wide.form2.T$Observation)] <- "CLUP"
wide.form3.T$Block <- "CLED"
wide.form3.T$Block[grep(pattern = "PLED", x = wide.form3.T$Observation)] <- "PLED"
wide.form3.T$Block[grep(pattern = "CLUP", x = wide.form3.T$Observation)] <- "CLUP"

## Now sum each observation within each timeGroup
## Make a function which will identify all of the blocks and return counts for each action
## taken by the parent in each block
retCellCounts <- function(x=NULL,blockLength = 10, allActions=c("Indirect_Command", "Neutral_Talk", "Question", "Reflection",
                                                                "Unlabeled_Praise", "Behavior_Description", "Direct_Command",
                                                                "Negative_Talk", "Labeled_Praise","Direct_Command_NO","Indirect_Command_NO"),
                          allOutputColumns = c("IC", "NT", "QU", "Re", "UP", "BD", "DC", "NeT", "LP","DCn","ICn"),
                          collapseVars = NULL){
  ## First make the blocks on the in data file
  ## This will assume that the variable indiciating the time of the event
  ## is called: "Date_Time_Absolute_dmy_hmsf"
  ## Also it assumes everything is 5 min long -- 300 seconds
  timeFromStart <- x$Time_Relative_sf
  timeFromStartN <- as.numeric(timeFromStart)
  breakVals <- seq(0, 300, blockLength)
  labelLengths <- length(breakVals)-1
  timeGroup <- cut(timeFromStartN, breaks = breakVals, include.lowest = TRUE, labels = 1:labelLengths)
  ## Now prepare a dataframe with all of the output values
  output.vals <- data.frame(blockCount = 1:labelLengths)
  for(vals in allOutputColumns){
    output.vals[vals] <- 0
  }
  if(!is.null(collapseVars)){
    output.vals$Other <- 0
  }
  
  ## Now go through each Action
  ## and add the amount of these actions of this action into each block
  for(action1 in 1:length(allActions)){
    ## First check if this action occurs in the dataframe
    action <- allActions[action1]
    output.col <- allOutputColumns[action1]
    presentCheck <- action %in% names(x)
    if(presentCheck){
      all.vals <- table(timeGroup, x[,action])
      if(dim(all.vals)[2]>1){
        iso.vals <- all.vals[,"1"]
        iso.vals <- iso.vals[iso.vals>0]
        output.vals[names(iso.vals),output.col] <- iso.vals
      }
    }
  }
  if(!is.null(collapseVars)){
    ## Identify values to sum across to create the other column
    output.vals$Other <- rowSums(output.vals[,collapseVars])
    output.vals <- output.vals[,!(names(output.vals) %in% collapseVars)]
  }
  ## Now figure out what to do with the other values
  output.vals$totalInt <- rowSums(output.vals[,-1])
  return(output.vals)
}

## Now do this for all participants
## Create a loop which will be used to create a list for
## all block lengths that we can explore for the
## HMM
iter.vals <- which(300 %% 1:150 ==0)
all.block.list <- list()
iter.count <- 1
iter.vals <- 10
for(d in iter.vals){
  all.out.tmp1 <- NULL
  for(i in unique(wide.form1.T$Observation)){
    counts.one.tmp <- wide.form1.T[which(wide.form1.T$Observation==i),]
    counts.one.tmp <- retCellCounts(counts.one.tmp, blockLength = d)
    counts.one.tmp$Observation <- i
    counts.one.tmp$Block <- unique(wide.form1.T[wide.form1.T$Observation==i,"Block"])
    all.out.tmp1 <- bind_rows(all.out.tmp1, counts.one.tmp)
  }
  all.out.tmp2 <- NULL
  for(i in unique(wide.form2.T$Observation)){
    counts.one.tmp <- wide.form2.T[which(wide.form2.T$Observation==i),]
    counts.one.tmp <- retCellCounts(counts.one.tmp, blockLength = d)
    counts.one.tmp$Observation <- i
    counts.one.tmp$Block <- unique(wide.form2.T[wide.form2.T$Observation==i,"Block"])
    all.out.tmp2 <- bind_rows(all.out.tmp2, counts.one.tmp)
  }
  all.out.tmp3 <- NULL
  for(i in unique(wide.form3.T$Observation)){
    counts.one.tmp <- wide.form1.T[which(wide.form3.T$Observation==i),]
    counts.one.tmp <- retCellCounts(counts.one.tmp, blockLength = d)
    counts.one.tmp$Observation <- i
    counts.one.tmp$Block <- unique(wide.form3.T[wide.form3.T$Observation==i,"Block"])
    all.out.tmp3 <- bind_rows(all.out.tmp3, counts.one.tmp)
  }
  all.counts <- bind_rows(all.out.tmp1, all.out.tmp2, all.out.tmp3)
  ntimes.vec <- as.numeric(table(all.counts$Observation))
  ## Now create our 3 class summary metrics
  ## First make the pride, neutral, and no actions for the Child led session
  all.counts$PrideCL <- rowSums(all.counts[,c("LP", "Re", "BD")])
  all.counts$PrideCL <- rowSums(all.counts[,c("LP", "Re", "BD","UP")])
  all.counts$NeutralCL <- all.counts[,c("NT")]
  all.counts$DontCL <- rowSums(all.counts[,c("DC", "DCn", "IC","ICn","NeT", "QU")])
  all.counts$PridePL <- rowSums(all.counts[,c("LP", "UP","Re", "BD", "DC", "IC")])
  all.counts$NeutralPL <- rowSums(all.counts[,c("NT", "QU")])
  all.counts$DontPL <- rowSums(all.counts[,c("NeT", "DCn","ICn")])
  ## Now grab the ones to use based on the task
  all.counts$Pride <- all.counts$PridePL
  all.counts$Pride[which(all.counts$Block=="CL")] <- all.counts$PrideCL[which(all.counts$Block=="CL")]
  all.counts$Neutral <- all.counts$NeutralPL
  all.counts$Neutral[which(all.counts$Block=="CL")] <- all.counts$NeutralCL[which(all.counts$Block=="CL")]
  all.counts$Dont <- all.counts$DontPL
  all.counts$Dont[which(all.counts$Block=="CL")] <- all.counts$DontCL[which(all.counts$Block=="CL")]
  all.block.list[[iter.count]] <- all.counts
  iter.count <- iter.count + 1
}
dim(all.counts)
head(all.counts)
corrplot::corrplot(cor(all.counts[,c("Pride", "Neutral", "Dont")]), method = "number")
corrplot::corrplot(cor(all.counts[,c("IC", "NT", "QU", "Re", "UP", "BD", "DC", "NeT", "LP", "DCn", "ICn")]), method = "number")
mod_dat <- list(
  individual = as.numeric(as.factor(all.counts$Observation)),
  time = all.counts$blockCount,
  T = max(all.counts$blockCount),
  I = max(as.numeric(as.factor(all.counts$Observation))),
  N = nrow(all.counts),
  Y = all.counts[,c("Pride", "Neutral", "Dont")],
  K = 3
)
# Compile model
p_var = stan_model("~/Documents/oregonDPICS/scripts/hierarchical_var.stan")
estimated_model <- sampling(p_var, 
                            data = mod_dat, 
                            iter = 100)
