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

## Load data
cled.dat <- read_xlsx("~/Documents/oregonDPICS/data/101CLEDW1.xlsx")
pled.dat <- read_xlsx("~/Documents/oregonDPICS/data/101PLEDW1.xlsx")
clup.dat <- read_xlsx("~/Documents/oregonDPICS/data/101CLUPW1.xlsx") ## starting with clean up because that will be the easiest to work with
more.dat1 <- foreign::read.spss("~/Documents/oregonDPICS/data/W1_rawDataEpochNums.sav", to.data.frame = TRUE)
more.dat2 <- foreign::read.spss("~/Documents/oregonDPICS/data/W2_rawDataEpochNums.sav", to.data.frame = TRUE)
more.dat3 <- foreign::read.spss("~/Documents/oregonDPICS/data/W3_rawDataEpochNums.sav", to.data.frame = TRUE)

## Make functions
retStartEM <- function(x = NULL, exitVelocity = .05, nState=2){
  ## This function will be used to create the start_EM object required for the mHMM
  ## function for the hierarchical hidden markov model
  ## It will take the input data, determine the number of levels for each dependent variable
  ## and then return a transmission matrix using the exitVelocity for each level outside of the anchor
  ## The anchor will be == 1 - q_emiss*exitVelocity
  ## X will be the counts for each action for the depdenent variable
  ##nState will be the number of hidden states that are being estimated
  
  ## Initialize everything
  n.level <- length(unique(x))
  out.mat <- matrix(NA, nrow=nState, ncol=n.level)
  outAnchor <- 1 - (n.level-1)*exitVelocity
  ## Identify the modal value
  modeVal <- Mode(x)
  ## Now create the matrix
  vals = rep(exitVelocity, n.level)
  for(i in 1:nState){
    if(i == 1){
      vals[which(unique(x)==modeVal)] <-outAnchor
      out.mat[i,] <- vals
    }
    else{
      vals = rep(exitVelocity, n.level)
      new.index <- which(out.mat[i-1,]==outAnchor) + 1
      if(new.index > n.level){
        new.index <- 1
      }
      vals[new.index] <- outAnchor
      out.mat[i,] <- vals
    }
  }
  return(out.mat)
}


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
wide.form1 <- reshape2::dcast(cled.dat, Date_Time_Absolute_dmy_hmsf ~ Behavior, fun.aggregate = length)
wide.form1.T <- reshape2::dcast(more.dat1, Observation + Time_Relative_sf ~ Behavior, fun.aggregate = length)
## Some values == 2?
## Observation CAPS181-CLUP-H2 ## Time: 292.08600
more.dat1[more.dat1$Observation=="CAPS181-CLUP-H2" & more.dat1$Time_Relative_sf =="292.086",]
#       Observation Time_Relative_sf Time_Relative_hmsf       Behavior     Modifier_1  FAMILY DPICS_Task EPOCH
# 31874 CAPS181-CLUP-H2          292.086              292.1 Direct_Command No Opportunity CAPS181       CLUP    10
# 31875 CAPS181-CLUP-H2          292.086              292.1 Direct_Command No Opportunity CAPS181       CLUP    10
## Looks like I do have identical rows -- I am just going to take all unique rows
wide.form1.T <- reshape2::dcast(unique(more.dat1), Observation + Time_Relative_sf ~ Behavior, fun.aggregate = length)
wide.form2 <- reshape2::dcast(pled.dat, Date_Time_Absolute_dmy_hmsf ~ Behavior, fun.aggregate = length)
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

wide.form3 <- reshape2::dcast(clup.dat, Date_Time_Absolute_dmy_hmsf ~ Behavior, fun.aggregate = length)
wide.form3.T <- reshape2::dcast(unique(more.dat3), Observation + Time_Relative_sf ~ Behavior, fun.aggregate = length)

## go through and correct the 2 values
iter.vals <- colnames(wide.form2.T)[3:15]
for(i in iter.vals){
  length.fix <- sum(wide.form2.T[,i] > 1)
  if(length.fix > 0){
    wide.form2.T[which(wide.form2.T[,i]>1),i] <- 1
  }
}

## Create the time from variables
wide.form1$timeFromPrev <- c(NA, wide.form1$Date_Time_Absolute_dmy_hmsf %>% diff())
wide.form1$timeFromStart <- wide.form1$Date_Time_Absolute_dmy_hmsf - wide.form1$Date_Time_Absolute_dmy_hmsf[1]
wide.form1$timeGroup <- cut(as.numeric(wide.form1$timeFromStart), breaks = seq(0, 300, 10), include.lowest = TRUE, labels = 1:30)
wide.form2$timeFromPrev <- c(NA, wide.form2$Date_Time_Absolute_dmy_hmsf %>% diff())
wide.form2$timeFromStart <- wide.form2$Date_Time_Absolute_dmy_hmsf - wide.form2$Date_Time_Absolute_dmy_hmsf[1]
wide.form2$timeGroup <- cut(as.numeric(wide.form2$timeFromStart), breaks = seq(0, 300, 10), include.lowest = TRUE, labels = 1:30)
wide.form3$timeFromPrev <- c(NA, wide.form3$Date_Time_Absolute_dmy_hmsf %>% diff())
wide.form3$timeFromStart <- wide.form3$Date_Time_Absolute_dmy_hmsf - wide.form3$Date_Time_Absolute_dmy_hmsf[1]
wide.form3$timeGroup <- cut(as.numeric(wide.form3$timeFromStart), breaks = seq(0, 300, 10), include.lowest = TRUE, labels = 1:30)

## Identify the block of the study here
wide.form1.T$Block <- NA
wide.form1.T$Block[grep(pattern = "CLED", x = wide.form1.T$Observation)] <- "CLED"
wide.form1.T$Block[grep(pattern = "PLED", x = wide.form1.T$Observation)] <- "PLED"
wide.form1.T$Block[grep(pattern = "PELD", x = wide.form1.T$Observation)] <- "PLED"
wide.form1.T$Block[grep(pattern = "CLUP", x = wide.form1.T$Observation)] <- "CLUP"
table(wide.form1.T$Block, useNA = "ifany")
wide.form2.T$Block <- NA
wide.form2.T$Block[grep(pattern = "CLED", x = wide.form2.T$Observation)] <- "CLED"
wide.form2.T$Block[grep(pattern = "PLED", x = wide.form2.T$Observation)] <- "PLED"
wide.form2.T$Block[grep(pattern = "CLUP", x = wide.form2.T$Observation)] <- "CLUP"
table(wide.form2.T$Block, useNA = "ifany")
wide.form3.T$Block <- NA
wide.form3.T$Block[grep(pattern = "CLED", x = wide.form3.T$Observation)] <- "CLED"
wide.form3.T$Block[grep(pattern = "CLEAD", x = wide.form3.T$Observation)] <- "CLED"
wide.form3.T$Block[grep(pattern = "PLED", x = wide.form3.T$Observation)] <- "PLED"
wide.form3.T$Block[grep(pattern = "CLUP", x = wide.form3.T$Observation)] <- "CLUP"
table(wide.form3.T$Block, useNA = "ifany")

## Now see if I can do it by participant
wide.form1.T$participant <- str_extract(wide.form1.T$Observation, "CAPS[0-9][0-9][0-9]")
wide.form2.T$participant <- str_extract(wide.form2.T$Observation, "CAPS[0-9][0-9][0-9]")
wide.form3.T$participant <- str_extract(wide.form3.T$Observation, "CAPS[0-9][0-9][0-9]")

## Now identify session code
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
#First remove all blank characters in the strings
wide.form1.T$Observation <- str_trim(wide.form1.T$Observation)
## Now remove the Block and the participant from the Observation string
rm.string <- function(x, rmVals = c("CAPS[0-9][0-9][0-9]", "CLED", "PLED", "PELD", "CLUP", "CLEAD", "--", "-")){
  out.vals <- x
  for(i in rmVals){
    out.vals <- gsub(pattern = i, x = out.vals, replacement = "")
  }
  return(out.vals)
}
wide.form1.T$sessionCode <- rm.string(wide.form1.T$Observation)
wide.form2.T$sessionCode <- rm.string(wide.form2.T$Observation)
wide.form3.T$sessionCode <- rm.string(wide.form3.T$Observation)

## Now melt and plot
plot.dat1 <- melt(wide.form1.T, id.vars = c("Time_Relative_sf", "Block", "Observation"))
plot.dat2 <- melt(wide.form2.T, id.vars = c("Time_Relative_sf", "Block", "Observation"))
plot.dat3 <- melt(wide.form3.T, id.vars = c("Time_Relative_sf", "Block", "Observation"))
plot.dat <- bind_rows(plot.dat1, plot.dat2, plot.dat3)
# ggplot(plot.dat, aes(x=Time_Relative_sf, y=value)) +
#   geom_point() +
#   facet_grid(Block ~ variable)

## Now sum each observation within each timeGroup
## Make a function which will identify all of the blocks and return counts for each action
## taken by the parent in each block
retCellCounts <- function(x=NULL,blockLength = 10, allActions=c("Indirect_Command", "Neutral_Talk", "Question", "Reflection",
                                                                "Unlabeled_Praise", "Behavior_Description", "Direct_Command",
                                                                "Negative_Talk", "Labeled_Praise","Direct_Command_NO","Indirect_Command_NO"),
                          allOutputColumns = c("IC", "NT", "QU", "Re", "UP", "BD", "DC", "NeT", "LP","DCn","ICn"),
                          collapseVars = NULL, maxCount = NULL){
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
  ## Check if we are only allowing a specific amount of actions per block
  if(!is.null(maxCount)){
    for(a in allOutputColumns){
      output.vals[which(output.vals[,a]>maxCount),a] <- maxCount
    }
  }
  ## Check if we are removing columns here
  if(!is.null(collapseVars)){
    ## Identify values to sum across to create the other column
    output.vals$Other <- rowSums(output.vals[,collapseVars])
    output.vals <- output.vals[,!(names(output.vals) %in% collapseVars)]
  }
  ## Now figure out what to do with the other values
  output.vals$totalInt <- rowSums(output.vals[,-1])
  return(output.vals)
}


# counts.one <- retCellCounts(wide.form1, blockLength = 5)
# counts.one$Block <- "CL"
# counts.two <- retCellCounts(wide.form2, blockLength = 5)
# counts.two$Block <- "PL"
# counts.thr <- retCellCounts(wide.form3, blockLength = 5)
# counts.thr$Block <- "CU"
# all.counts <- bind_rows(counts.one, counts.two, counts.thr)
# all.counts.plot <- reshape2::melt(all.counts, id.vars=c("blockCount", "Block"))
# ggplot(all.counts.plot, aes(x=blockCount, y=value)) +
#   geom_line() + 
#   facet_grid(variable ~ Block)
## Now do this for all participants
## Create a loop which will be used to create a list for
## all block lengths that we can explore for the
## HMM
iter.vals <-   seq(1,50,1)[which(300 %% 1:50 ==0)]
iter.vals <- 30
all.block.list <- list()
iter.count <- 1
blockVals <- c("CLED", "PLED", "CLUP")
for(d in iter.vals){
  all.out.tmp1 <- NULL
  for(i in unique(wide.form1.T$participant)){
    ind.subj <- NULL
    iso.ind.vals <- wide.form1.T[which(wide.form1.T$participant==i),]
    for(b in blockVals){
      ## Check length of session codes
      counts.one.tmp <- iso.ind.vals[which(iso.ind.vals$Block==b),]
      if(length(unique(counts.one.tmp$sessionCode))>1){
        ## Now grab the best guess session code
        counts.one.tmp <- counts.one.tmp[counts.one.tmp$sessionCode==names(which.max(table(iso.ind.vals$sessionCode))),]
      }
      counts.one.tmp <- retCellCounts(counts.one.tmp, blockLength = d, maxCount = NULL)
      counts.one.tmp$Subj <- i
      counts.one.tmp$Block <- b
      counts.one.tmp$Session <- 1
      ind.subj <- bind_rows(ind.subj, counts.one.tmp)
    }
    ind.subj$acrossCount <- 1:dim(ind.subj)[1]
    all.out.tmp1 <- bind_rows(all.out.tmp1, ind.subj)
  }
  all.out.tmp2 <- NULL
  for(i in unique(wide.form2.T$participant)){
    ind.subj <- NULL
    iso.ind.vals <- wide.form2.T[which(wide.form2.T$participant==i),]
    for(b in blockVals){
      ## Check length of session codes
      counts.one.tmp <- iso.ind.vals[which(iso.ind.vals$Block==b),]
      if(length(unique(counts.one.tmp$sessionCode))>1){
        ## Now grab the best guess session code
        counts.one.tmp <- counts.one.tmp[counts.one.tmp$sessionCode==names(which.max(table(iso.ind.vals$sessionCode))),]
      }
      counts.one.tmp <- retCellCounts(counts.one.tmp, blockLength = d, maxCount = NULL)
      counts.one.tmp$Subj <- i
      counts.one.tmp$Block <- b
      counts.one.tmp$Session <- 2
      ind.subj <- bind_rows(ind.subj, counts.one.tmp)
    }
    ind.subj$acrossCount <- 1:dim(ind.subj)[1]
    all.out.tmp2 <- bind_rows(all.out.tmp2, ind.subj)
  }
  all.out.tmp3 <- NULL
  for(i in unique(wide.form3.T$participant)){
    ind.subj <- NULL
    iso.ind.vals <- wide.form3.T[which(wide.form3.T$participant==i),]
    for(b in blockVals){
      ## Check length of session codes
      counts.one.tmp <- iso.ind.vals[which(iso.ind.vals$Block==b),]
      if(length(unique(counts.one.tmp$sessionCode))>1){
        ## Now grab the best guess session code
        counts.one.tmp <- counts.one.tmp[counts.one.tmp$sessionCode==names(which.max(table(iso.ind.vals$sessionCode))),]
      }
      counts.one.tmp <- retCellCounts(counts.one.tmp, blockLength = d, maxCount = NULL)
      counts.one.tmp$Subj <- i
      counts.one.tmp$Block <- b
      counts.one.tmp$Session <- 3
      ind.subj <- bind_rows(ind.subj, counts.one.tmp)
    }
    ind.subj$acrossCount <- 1:dim(ind.subj)[1]
    all.out.tmp3 <- bind_rows(all.out.tmp3, ind.subj)
  }
  all.counts <- bind_rows(all.out.tmp1, all.out.tmp2, all.out.tmp3)
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

## Fix data so the column values agree
all.counts <- all.block.list[[1]]
all.counts$Block[all.counts$Block=="CLED"] <- 1
all.counts$Block[all.counts$Block=="PLED"] <- 2
all.counts$Block[all.counts$Block=="CLUP"] <- 3
all.counts <- all.counts %>% group_by(Subj, Block) %>% 
  arrange(Subj, Block)
all.counts$id <- paste(all.counts$Subj, all.counts$Session)



## Now try the mulitlevel HMM here
# Run a model without covariate(s) and default priors:
set.seed(14532)
all.counts.mod <- all.counts[,c("id", "Pride", "Neutral", "Dont")]
all.counts.mod <- data.frame(all.counts.mod)
all.counts.mod[,2:ncol(all.counts.mod)] <- apply(all.counts.mod[,2:ncol(all.counts.mod)], c(1,2), function(x) x+1)
all.counts.mod$Pride <- as.numeric(cut(all.counts.mod$Pride, breaks = quantile(all.counts.mod$Pride, c(0, .66,.9, 1)), include.lowest = TRUE, labels = 1:3))
all.counts.mod$Neutral <- as.numeric(cut(all.counts.mod$Neutral, breaks = quantile(all.counts.mod$Neutral, c(0, .66,.9, 1)), include.lowest = TRUE, labels = 1:3))
all.counts.mod$Dont <- as.numeric(cut(all.counts.mod$Dont, breaks = quantile(all.counts.mod$Dont, c(0, .7,.9, 1)), include.lowest = TRUE, labels = 1:3))

## 2 state
m <- 2
n_dep <- 3
q_emiss <- c(length(unique(all.counts.mod[,2])), length(unique(all.counts.mod[,3])), length(unique(all.counts.mod[,4])))
#q_emiss <- c(3, 3)
# specifying starting values
start_TM <- diag(.8, m)
start_TM[lower.tri(start_TM) | upper.tri(start_TM)] <- .2
start_EM <- list(retStartEM(x = all.counts.mod[,2], nState = m, exitVelocity = .1),
                 retStartEM(x = all.counts.mod[,3], nState = m, exitVelocity = .1),
                 retStartEM(x = all.counts.mod[,4], nState = m, exitVelocity = .1))
out_2st <- mHMM(s_data = all.counts.mod,
                gen = list(m = m, n_dep = n_dep, q_emiss = q_emiss),
                start_val = c(list(start_TM), start_EM),
                mcmc = list(J = 2000, burn_in = 500))




## 3 state
m <- 3
n_dep <- 3
q_emiss <- c(length(unique(all.counts.mod[,2])), length(unique(all.counts.mod[,3])), length(unique(all.counts.mod[,4])))
#q_emiss <- c(3, 3)
# specifying starting values
start_TM <- diag(.8, m)
start_TM[lower.tri(start_TM) | upper.tri(start_TM)] <- .1
start_EM <- list(retStartEM(x = all.counts.mod[,2], nState = m, exitVelocity = .1),
                 retStartEM(x = all.counts.mod[,3], nState = m, exitVelocity = .1),
                 retStartEM(x = all.counts.mod[,4], nState = m, exitVelocity = .1))
out_3st <- mHMM(s_data = all.counts.mod,
                gen = list(m = m, n_dep = n_dep, q_emiss = q_emiss),
                start_val = c(list(start_TM), start_EM),
                mcmc = list(J = 2000, burn_in = 500))

## 4 state
m <- 4
n_dep <- 3
q_emiss <- c(length(unique(all.counts.mod[,2])), length(unique(all.counts.mod[,3])), length(unique(all.counts.mod[,4])))
# specifying starting values
start_TM <- diag(.7, m)
start_TM[lower.tri(start_TM) | upper.tri(start_TM)] <- .1
start_EM <- list(retStartEM(x = all.counts.mod[,2], nState = m, exitVelocity = .1),
                 retStartEM(x = all.counts.mod[,3], nState = m, exitVelocity = .1),
                 retStartEM(x = all.counts.mod[,4], nState = m, exitVelocity = .1))
out_4st <- mHMM(s_data = all.counts.mod, 
                gen = list(m = m, n_dep = n_dep, q_emiss = q_emiss), 
                start_val = c(list(start_TM), start_EM),
                mcmc = list(J = 2000, burn_in = 500))

## 5 state
m <- 5
n_dep <- 3
q_emiss <- c(length(unique(all.counts.mod[,2])), length(unique(all.counts.mod[,3])), length(unique(all.counts.mod[,4])))
# specifying starting values
start_TM <- diag(.54, m)
start_TM[lower.tri(start_TM) | upper.tri(start_TM)] <- .115
start_EM <- list(retStartEM(x = all.counts.mod[,2], nState = m, exitVelocity = .1),
                 retStartEM(x = all.counts.mod[,3], nState = m, exitVelocity = .1),
                 retStartEM(x = all.counts.mod[,4], nState = m, exitVelocity = .1))
out_5st <- mHMM(s_data = all.counts.mod, 
                gen = list(m = m, n_dep = n_dep, q_emiss = q_emiss), 
                start_val = c(list(start_TM), start_EM),
                mcmc = list(J = 2000, burn_in = 500))
