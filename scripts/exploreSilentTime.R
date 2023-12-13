## This script will be used to examine all of the different levels for time unitl a silent state is introduced

## Clean slate
rm(list = ls())

## Load library(s)
library("ggplot2")
library("tidyverse")
source("~/Documents/Github/adroseHelperScripts/R/afgrHelpFunc.R")
library("reshape2")
library("visreg")
library("survival")
library("coxme")
library("survminer")
library("lme4")
library(brms)

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


## Now try to organize data, so the columns are the actions that can be taken from the parent (indirect command, neutral talk.. et cetera), the rows are still the time, but it is a binary indiciator for when an event occurs
wide.form1 <- reshape2::dcast(unique(more.dat1), Observation + Time_Relative_sf ~ Behavior, fun.aggregate = length)
wide.form3 <- reshape2::dcast(unique(more.dat3), Observation + Time_Relative_sf ~ Behavior, fun.aggregate = length)
## Now do the same for child behavior
wide.form1.C <- reshape2::dcast(unique(more.dat1), Observation + Time_Relative_sf ~ Modifier_1, fun.aggregate = length)
wide.form3.C <- reshape2::dcast(unique(more.dat3), Observation + Time_Relative_sf ~ Modifier_1, fun.aggregate = length)
## Now see if we can merge these into the states dataset?
wide.form1.C <- wide.form1.C[,c(1,2,4,6,7)]
names(wide.form1.C) <- gsub(patter=" ", replacement = "", x = names(wide.form1.C))
wide.form3.C <- wide.form3.C[,c(1,2,4,6,7)]
names(wide.form3.C) <- gsub(patter=" ", replacement = "", x = names(wide.form3.C))
## Now merge these onto the state dataset?
wide.form1.C$Block <- NA
wide.form1.C$Block[grep(pattern = "CLED", x = wide.form1.C$Observation)] <- "CLED"
wide.form1.C$Block[grep(pattern = "PLED", x = wide.form1.C$Observation)] <- "PLED"
wide.form1.C$Block[grep(pattern = "PELD", x = wide.form1.C$Observation)] <- "PLED"
wide.form1.C$Block[grep(pattern = "CLUP", x = wide.form1.C$Observation)] <- "CLUP"
wide.form3.C$Block <- NA
wide.form3.C$Block[grep(pattern = "CLED", x = wide.form3.C$Observation)] <- "CLED"
wide.form3.C$Block[grep(pattern = "CLEAD", x =wide.form3.C$Observation)] <- "CLED"
wide.form3.C$Block[grep(pattern = "PLED", x = wide.form3.C$Observation)] <- "PLED"
wide.form3.C$Block[grep(pattern = "CLUP", x = wide.form3.C$Observation)] <- "CLUP"

## Now see if I can do it by participant
wide.form1.C$participant <- str_extract(wide.form1.C$Observation, "CAPS[0-9][0-9][0-9]")
wide.form3.C$participant <- str_extract(wide.form3.C$Observation, "CAPS[0-9][0-9][0-9]")

## Now merge these?
wide.form1.C$wave <- 1
wide.form3.C$wave <- 3

## Identify the block of the study here
wide.form1$Block <- NA
wide.form1$Block[grep(pattern = "CLED", x = wide.form1$Observation)] <- "CLED"
wide.form1$Block[grep(pattern = "PLED", x = wide.form1$Observation)] <- "PLED"
wide.form1$Block[grep(pattern = "PELD", x = wide.form1$Observation)] <- "PLED"
wide.form1$Block[grep(pattern = "CLUP", x = wide.form1$Observation)] <- "CLUP"
wide.form3$Block <- NA
wide.form3$Block[grep(pattern = "CLED", x = wide.form3$Observation)] <- "CLED"
wide.form3$Block[grep(pattern = "CLEAD", x = wide.form3$Observation)] <- "CLED"
wide.form3$Block[grep(pattern = "PLED", x = wide.form3$Observation)] <- "PLED"
wide.form3$Block[grep(pattern = "CLUP", x = wide.form3$Observation)] <- "CLUP"
wide.form1$wave <- 1
wide.form3$wave <- 3

## Now combine these into one wide format data frame across blocks
all.wide <- merge(wide.form1, wide.form3,all=TRUE)
## Now add child's data
all.child.wide <- merge(wide.form1.C, wide.form3.C, all=TRUE)
all.wide <- merge(all.wide, all.child.wide)

## Now see if I can do it by participant
all.wide$participant <- str_extract(all.wide$Observation, "CAPS[0-9][0-9][0-9]")

## Now identify session code
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
#First remove all blank characters in the strings
all.wide$Observation <- str_trim(all.wide$Observation)
## Now remove the Block and the participant from the Observation string
rm.string <- function(x, rmVals = c("CAPS[0-9][0-9][0-9]", "CLED", "PLED", "PELD", "CLUP", "CLEAD", "--", "-")){
  out.vals <- x
  for(i in rmVals){
    out.vals <- gsub(pattern = i, x = out.vals, replacement = "")
  }
  return(out.vals)
}
all.wide$sessionCode <- rm.string(all.wide$Observation)
## Now isolate CLUP data just for the survivial analysis
all.wide.clup <- all.wide[which(all.wide$Block=="CLUP"),]
## Now remove any observations that ocur at t==0.00
all.wide.clup <- all.wide.clup[-which(all.wide.clup$Time_Relative_sf==0),]

## Now collapse variables into the 3 class system: PRIDE, Dont, Neutral
collpaseVars <- list()
collpaseVars[[1]] <- c("Behavior_Description", "Labeled_Praise", 
                       "Reflection", "Unlabeled_Praise",
                       "Direct_Command","Indirect_Command","Command") # rowSums(all.counts[,c("LP", "UP","Re", "BD", "DC", "IC")])
collpaseVars[[2]] <- c("Neutral_Talk", "Question")
collpaseVars[[3]] <- c("Direct_Command_NO", "Indirect_Command_NO","Negative_Talk") # rowSums(all.counts[,c("NeT", "DCn","ICn")])
collpaseVars[[4]] <- c("Direct_Command","Indirect_Command","Command") ## command
# Now go through every action and add the state based on these assignments to the action being performed at each timepoint across all data
## First remove "Var.3" column from the data

## No go through all of the collapse variable columns
state1index <- which(rowSums(all.wide.clup[,collpaseVars[[1]]], na.rm=TRUE)>0)
state2index <- which(rowSums(all.wide.clup[,collpaseVars[[2]]], na.rm=TRUE)>0)
state3index <- which(rowSums(all.wide.clup[,collpaseVars[[3]]], na.rm=TRUE)>0)
state4index <- which(rowSums(all.wide.clup[,collpaseVars[[4]]], na.rm=TRUE)>0)
all.wide.clup$state <- NA
all.wide.clup$state[state1index] <- 1
all.wide.clup$state[state2index] <- 2
all.wide.clup$state[state3index] <- 3
all.wide.clup$state[state4index] <- 4
## Now remove NA state values
all.wide.clup <- all.wide.clup[!is.na(all.wide.clup$state),]
## Now go through and add the disengaged state 5 variable
id.rep <- unique(all.wide.clup$Observation)
unengage.times <- c(1, seq(2.5, 20, 2.5))
all.mods <- list()
all.mods.count <- 1
all.dat <- list()
insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}
## Organize the clup data here
all.wide.clup <- all.wide.clup %>% group_by(Observation) %>% 
  arrange(Observation, Time_Relative_sf)

for(ue in unengage.times){
  unengage.time <- ue
  out.states.UN <- NULL
  for(d in id.rep){
    print(d)
    tmp.data <- all.wide.clup[which(all.wide.clup$Observation == d),]
    ## Add a disengaged column at the start
    new.row <- tmp.data[1,]
    new.row$Time_Relative_sf <- 0
    new.row$state <- 5
    tmp.data <- insertRow(tmp.data, new.row, 1)
    diff.times <- c(0,diff(tmp.data$Time_Relative_sf))
    runDiff <- FALSE
    if(length(which(diff.times>unengage.time))>0){
      runDiff <- TRUE
    }
    while(runDiff){
      index.one <- which(diff.times>unengage.time)[1]
      tmp.row <- index.one - 1
      new.row <- tmp.data[tmp.row,]
      new.row$Time_Relative_sf <- new.row$Time_Relative_sf + (unengage.time-.01)
      new.row$state <- 5
      tmp.data <- insertRow(tmp.data, new.row, index.one)
      diff.times <- c(0, diff(tmp.data$Time_Relative_sf))
      if(length(which(diff.times>unengage.time))>0){
        runDiff <- TRUE
      }else{
        runDiff <- FALSE
      }
    }
    out.states.UN <- bind_rows(out.states.UN, tmp.data)  
  }
  out.states.UN$state <- factor(out.states.UN$state, levels=c(1,2,3,4,5))
  
  ## Now go through and add the following variables
  ## comply count indicator == running tally for the number of complies achieved
  out.states.UN <- bind_cols(out.states.UN, model.matrix(~-1 +state, out.states.UN))
  ## Organize the data
  out.states.UN %>% group_by(Observation) %>% 
    arrange(Observation, Time_Relative_sf)
  
  ## Now organize the data for the survivial analysis
  out.surv <- NULL
  for(i in unique(out.states.UN$Observation)){
    print(i)
    ## Isolate data
    tmp.dat <- out.states.UN[which(out.states.UN$Observation==i),]
    ## First identify all comply values
    complyIndex <- which(tmp.dat$Comply==1)
    ## Now identify the time from previous comply
    vals <- tmp.dat$Time_Relative_sf[complyIndex]
    complyVals <- diff(vals)
    complyVals <- c(vals[1], complyVals)
    ## Now add a count for each comply count
    tmp.dat$complyCount <- 1
    row.index <- 1:nrow(tmp.dat)
    for(j in 1:length(complyIndex)){
      tmp.index <- which(row.index>complyIndex[j])
      tmp.dat$complyCount[tmp.index] <- tmp.dat$complyCount[tmp.index] + 1 
    }
    ## Now go through the comply Count and identify the time from the previous comply
    tmp.dat$timePrevComply <- tmp.dat$Time_Relative_sf
    for(z in 2:length(unique(tmp.dat$complyCount))){
      lastComply <- which(tmp.dat$complyCount==z-1)
      subVal <- max(tmp.dat$Time_Relative_sf[lastComply])
      tmp.dat$timePrevComply[which(tmp.dat$complyCount==z)] <- tmp.dat$timePrevComply[which(tmp.dat$complyCount==z)]-subVal
    }
    if(length(unique(tmp.dat$complyCount))==1){
      print("NoComply")
      tmp.dat$timePrevComply <- tmp.dat$Time_Relative_sf
    }
    ## Now create the cumulative summation of the state variables within each state within each comply count
    out.cs.vals <- matrix(NA, nrow = nrow(tmp.dat), ncol = 6)
    out.cs.vals[,1] <- tmp.dat$complyCount
    colnames(out.cs.vals) <- c("complyCount2", paste("state", 1:5, "CumSum", sep=''))
    for(z in 1:length(unique(tmp.dat$complyCount))){
      ## Isolate the rows
      iso.rows <- which(tmp.dat$complyCount==z)
      ## Now apply cumsum opperator over the state binary indiciators
      if(length(iso.rows)>1){
        out.cols <- apply(tmp.dat[iso.rows,c("state1", "state2", "state3", "state4", "state5")], 2,cumsum)
        out.cs.vals[iso.rows,paste("state", 1:5, "CumSum", sep='')] <- out.cols
      }else{
        out.cols <- tmp.dat[iso.rows,c("state1", "state2", "state3", "state4", "state5")]
        for(r in 2:6){
          col.count <- r-1
          out.cs.vals[iso.rows,r] <- as.numeric(out.cols[col.count])
        }
      }
    }
    tmp.dat <- cbind(tmp.dat, out.cs.vals)
    out.surv <- rbind(out.surv, tmp.dat)
  }  
  
  ## Now I need to add the time from previous state column for each observation
  out.surv.int <- NULL
  for(i in unique(out.surv$Observation)){
    print(i)
    ## Isolate data
    tmp.dat <- out.surv[which(out.surv$Observation==i),]
    ## Ok, now I need to make a time start, time end variable
    ## Make sure the first row is initialized with a start time at 0
    ## and I will need to have state during the time and the indicator variable
    ## describing if a compliance was achieved in each interval
    #out.dat <- NULL
    ## Now add the number of rows we need for the total number of comply counts
    out.dat.cor <- NULL
    for(z in 1:length(unique(tmp.dat$complyCount))){
      iso.dat <- tmp.dat[which(tmp.dat$complyCount==z),]
      if(z == 1){
        iso.dat$currentSesh <- iso.dat$Time_Relative_sf
        if(dim(iso.dat)[1]>1){
          for(J in 2:nrow(iso.dat)){
            prev.row <- J-1
            iso.dat$currentSesh[J] <- iso.dat$Time_Relative_sf[prev.row]
          }
        }
        else{
          iso.dat$currentSesh <- iso.dat$Time_Relative_sf+.05
        }
      }else{
        ## Now identify the time of the previous comply
        prev.comply.count <- z - 1
        prev.comply.time <- tmp.dat[which(tmp.dat$complyCount==prev.comply.count & tmp.dat$Comply==1),"Time_Relative_sf"]
        iso.dat$currentSesh <- iso.dat$Time_Relative_sf - min(iso.dat$Time_Relative_sf)
      }
      out.dat.cor <- bind_rows(out.dat.cor, iso.dat)
    }
    out.surv.int <- bind_rows(out.surv.int, out.dat.cor)
  }
  ## Now attach caps data and do some wave 1 organization
  out.surv.caps <- merge(out.surv.int, caps.data, by.x=c("participant"), by.y=c("FAMILY"))
  ## Now fix the wave issues
  out.surv.caps$CDINUM[out.surv.caps$wave==1] <- 0
  out.surv.caps$PDINUM[out.surv.caps$wave==1] <- 0
  ## Remove issues where currentSesh is == Time_Relative_sf
  out.surv.caps <- out.surv.caps[-which(out.surv.caps$Time_Relative_sf==0),]

  
  all.behaviors <- c("Behavior_Description", "Direct_Command", "Direct_Command_NO",
                     "Indirect_Command", "Indirect_Command_NO", "Labeled_Praise", 
                     "Negative_Talk", "Neutral_Talk", "Question", "Reflection","Unlabeled_Praise")
  ## Now run logistic regressions predicting if compliance is achieved for any command down here
  ## More data prep will have to be performed down here
  out.logit <- NULL
  for(i in unique(out.states.UN$Observation)){
    ## Isolate data
    tmp.dat <- out.states.UN[which(out.states.UN$Observation==i),]
    ## First identify all comply values
    commandIndex <- which(tmp.dat$state==4)
    ## Now identify the time from previous comply
    vals <- tmp.dat$Time_Relative_sf[commandIndex]
    commandVals <- diff(vals)
    commandVals <- c(vals[1], commandVals)
    ## Now add a count for each comply count
    tmp.dat$commandCount <- 1
    row.index <- 1:nrow(tmp.dat)
    for(j in 1:length(commandIndex)){
      tmp.index <- which(row.index>commandIndex[j])
      tmp.dat$commandCount[tmp.index] <- tmp.dat$commandCount[tmp.index] + 1 
    }
    ## Now go through the comply Count and identify the time from the previous comply
    tmp.dat$timePrevCommand <- tmp.dat$Time_Relative_sf
    for(z in 2:length(unique(tmp.dat$commandCount))){
      lastCommand <- which(tmp.dat$commandCount==z-1)
      subVal <- max(tmp.dat$Time_Relative_sf[lastCommand])
      tmp.dat$timePrevCommand[which(tmp.dat$commandCount==z)] <- tmp.dat$timePrevCommand[which(tmp.dat$commandCount==z)]-subVal
    }
    ## Now do the same for the other actions?
    ## In order to do this I am going to index all discrete states
    ## And just find the lag of the times across these states and then difference the lagged with the current
    ## state time
    tmp.mat.time.diff <- matrix(NA, nrow = nrow(tmp.dat), ncol = 5)
    tmp.mat.time.diff2 <- matrix(NA, nrow = nrow(tmp.dat), ncol = 5)
    tmp.mat.time.diff[1,] <- 0
    for(z in 1:5){
      ## Isolate the state times
      state.index <- unique(c(1, which(tmp.dat$state==z)))
      ## Now put these values into the matrix for all values within
      ## a range
      for(r in 1:length(state.index)){
        tmp.mat.time.diff[state.index[r]:nrow(tmp.mat.time.diff),as.numeric(z)] <- tmp.dat$Time_Relative_sf[state.index[r]]
      }
    }
    ## Now find the difference in time for each behavior
    for(z in 1:5){
      tmp.mat.time.diff2[,z] <- tmp.dat$Time_Relative_sf - tmp.mat.time.diff[,z]
    }
    ## Now prep the column names
    colnames(tmp.mat.time.diff2) <- paste("timeFromLastState", 1:5, sep="_")
    if(length(unique(tmp.dat$commandCount))==1){
      print("NoCommand")
      tmp.dat$timePrevCommand <- tmp.dat$Time_Relative_sf
    }
    ## Now perform this same procedure for all of the distinct behaviors
    tmp.mat.time.diffB <- matrix(NA, nrow = nrow(tmp.dat), ncol = length(all.behaviors))
    tmp.mat.time.diff2B <- matrix(NA, nrow = nrow(tmp.dat), ncol = length(all.behaviors))
    tmp.mat.time.diffB[1,] <- 0
    count.val <- 1
    for(z in all.behaviors){
      ## Isolate the state times
      state.index <- unique(c(1, which(tmp.dat[,z]==1)))
      ## Now put these values into the matrix for all values within
      ## a range
      if(length(state.index)>0){
        for(r in 1:length(state.index)){
          tmp.mat.time.diffB[state.index[r]:nrow(tmp.mat.time.diffB),count.val] <- tmp.dat$Time_Relative_sf[state.index[r]]
        }
      }
      count.val <- count.val+1
    }
    for(z in 1:length(all.behaviors)){
      tmp.mat.time.diff2B[,z] <- tmp.dat$Time_Relative_sf - tmp.mat.time.diffB[,z]
    }
    colnames(tmp.mat.time.diff2B) <- paste("timeFromLast", all.behaviors, sep="_")
    ## Now create the cumulative summation of the state variables within each state within each comply count
    out.cs.vals <- matrix(NA, nrow = nrow(tmp.dat), ncol = 6)
    out.cs.vals[,1] <- tmp.dat$commandCount
    colnames(out.cs.vals) <- c("commandCount2", paste("state", 1:5, "CumSum", sep=''))
    for(z in 1:length(unique(tmp.dat$commandCount))){
      ## Isolate the rows
      iso.rows <- which(tmp.dat$commandCount==z)
      ## Now apply cumsum opperator over the state binary indiciators
      if(length(iso.rows)>1){
        out.cols <- apply(tmp.dat[iso.rows,c("state1", "state2", "state3", "state4", "state5")], 2,cumsum)
        out.cs.vals[iso.rows,paste("state", 1:5, "CumSum", sep='')] <- out.cols
      }else{
        out.cols <- tmp.dat[iso.rows,c("state1", "state2", "state3", "state4", "state5")]
        for(r in 2:6){
          col.count <- r-1
          out.cs.vals[iso.rows,r] <- as.numeric(out.cols[col.count])
        }
      }
    }
    ## Now do the same for all of the specific behavior actions
    out.com.vals <- matrix(NA, nrow=nrow(tmp.dat), ncol = length(all.behaviors)*2)
    ## Now prepare the column names here
    colnames(out.com.vals) <- paste(1:ncol(out.com.vals))
    colnames(out.com.vals)[1:length(all.behaviors)] <- paste("comSpecific_", all.behaviors, sep='')
    part.two.index <- length(all.behaviors) +1
    colnames(out.com.vals)[part.two.index:ncol(out.com.vals)] <- paste("nonSpecific_", all.behaviors, sep='')
    for(b in all.behaviors){
      col.one <- paste("comSpecific_", b, sep='')
      col.two <- paste("nonSpecific_", b, sep='')
      ## Calc the non specific value here
      out.com.vals[,col.two] <- as.numeric(unlist(cumsum(tmp.dat[,b])))
      ## Now cycle through all of the commands
      for(z in 1:length(unique(tmp.dat$commandCount))){
        iso.rows <- which(tmp.dat$commandCount==z)
        ## Now calc the cum sum for each of these values
        out.com.vals[iso.rows,col.one] <- as.numeric(unlist(cumsum(tmp.dat[iso.rows,b])))
      }
    }
    ## Now do total cum sum counts
    out.cols2 <- apply(tmp.dat[,c("state1", "state2", "state3", "state4", "state5")], 2,cumsum)
    colnames(out.cols2) <- paste("state", 1:5, "CumSumTotal", sep='')
    tmp.dat <- cbind(tmp.dat, out.cs.vals)
    tmp.dat <- cbind(tmp.dat, out.cols2)
    tmp.dat <- cbind(tmp.dat, tmp.mat.time.diff2)
    tmp.dat <- cbind(tmp.dat, tmp.mat.time.diff2B)
    tmp.dat <- cbind(tmp.dat, out.com.vals)
    out.logit <- rbind(out.logit, tmp.dat)
  } 
  
  ## Now go through and create a running tally for time spent in a silent state
  ## I will have to identify any states == 5
  ## This is going to be expensive, I think
  ## How I am going to do this is create a tally for every state
  ## and I will increase this count for every state based on the time the action is performed, until the next count
  ## need to make sure I figure out a good method for the final action taken
  out.logit2 <- NULL
  for(i in unique(out.logit$Observation)){
    ## Isolate data
    tmp.dat <- out.logit[which(out.logit$Observation==i),]
    ## Prep the new column titles
    new.data <- data.frame(timeState1=rep(0, nrow(tmp.dat)), timeState2=rep(0, nrow(tmp.dat)), timeState3=rep(0, nrow(tmp.dat)), 
                           timeState4=rep(0, nrow(tmp.dat)), timeState5=rep(0, nrow(tmp.dat)))
    ## Now identify the time spent in each state up across every row within an individual
    for(l in 1:nrow(tmp.dat)){
      enter.val <- tmp.dat[l,"Time_Relative_sf"]
      enter.state <- tmp.dat[l,"state"]
      if(l<nrow(tmp.dat)){
        exit.row <- l + 1
        exit.val <- tmp.dat[exit.row,"Time_Relative_sf"]
      }
      if(l==nrow(tmp.dat)){
        exit.val <- 300
      }
      ## Now find the correct state column name and add this to the new.data df
      add.col <- paste("timeState", enter.state, sep="")
      add.val <- exit.val - enter.val
      ## now put this number into the corresponding row in the new.data variable
      new.data[l,add.col] <- add.val
    }
    for(z in 1:ncol(new.data)){
      new.data[,z] <- cumsum(new.data[,z])
    }
    ## Add this to out.logit
    tmp.dat <- bind_cols(tmp.dat, new.data)
    out.logit2 <- bind_rows(out.logit2, tmp.dat)
  }
  
  ## Now repeat this process but run the timer within each command
  out.logit3 <- NULL
  for(i in unique(out.logit$Observation)){
    ## Isolate data
    tmp.dat2 <- out.logit2[which(out.logit2$Observation==i),]
    ## Now idenitfy all coammand counts within these data
    total.command <- max(tmp.dat2$commandCount)
    new.data.out <- NULL
    for(j in 1:total.command){
      ## Isolate the command specific counts
      tmp.dat <- tmp.dat2[which(tmp.dat2$commandCount==j),]
      ## Prep the new column titles
      new.data <- data.frame(timeState1=rep(0, nrow(tmp.dat)), timeState2=rep(0, nrow(tmp.dat)), timeState3=rep(0, nrow(tmp.dat)), 
                             timeState4=rep(0, nrow(tmp.dat)), timeState5=rep(0, nrow(tmp.dat)))
      ## Now identify the time spent in each state up across every row within an individual
      for(l in 1:nrow(tmp.dat)){
        enter.val <- tmp.dat[l,"Time_Relative_sf"]
        enter.state <- tmp.dat[l,"state"]
        if(l<nrow(tmp.dat)){
          exit.row <- l + 1
          exit.val <- tmp.dat[exit.row,"Time_Relative_sf"]
        }
        if(l==nrow(tmp.dat2)){
          exit.val <- 300
        }
        ## Now find the correct state column name and add this to the new.data df
        add.col <- paste("timeState", enter.state, sep="")
        add.val <- exit.val - enter.val
        ## now put this number into the corresponding row in the new.data variable
        new.data[l,add.col] <- add.val
        ## Now calc the cumsum for each column, this details the amount of time spent in each state
        ## across the entire task
      }
      for(z in 1:ncol(new.data)){
        new.data[,z] <- cumsum(new.data[,z])
      }
      new.data.out <- bind_rows(new.data.out, new.data)
      ## Add this to out.logit
    }
    ## Fix the column names
    colnames(new.data.out) <- paste(colnames(new.data.out), "_ComSpec", sep='')
    tmp.dat2 <- bind_cols(tmp.dat2, new.data.out)
    out.logit3 <- bind_rows(out.logit3, tmp.dat2)
  }
  
  ## Now isolate all command rows
  logit.dat <- out.logit3[which(out.logit3$state==4),]

  ## Now add a direct versus indirect command indicator
  logit.dat$directCommand <- TRUE
  logit.dat$directCommand[logit.dat$Indirect_Command==1] <- FALSE

  ## Try a brms model here
  comply.glmer.brmZ2 <- brm(Comply ~  state1CumSumTotal + state2CumSumTotal + state3CumSumTotal + state5CumSumTotal+ state1CumSum + state2CumSum + state3CumSum + state5CumSum + 
                              state1CumSumTotal:timeFromLastState_1 + state2CumSumTotal:timeFromLastState_2 + state3CumSumTotal:timeFromLastState_3 + state5CumSumTotal:timeFromLastState_5 + timePrevCommand +
                              directCommand + (state1CumSumTotal +state2CumSumTotal +state3CumSumTotal +state5CumSumTotal +directCommand|participant), 
                            data=logit.dat,family = bernoulli(link = "logit"),
                            warmup = 2000,
                            thin=5,
                            iter = 6000,
                            chains = 5, 
                            cores = 5,
                            seed = 123)

  ## Now toy around with the time spent in silence using the new count variables
  ## This builds off of the comply.glmer.brmZ2 model syntax
  comply.glmer.brmZ3 <- brm(Comply ~  state1CumSumTotal + state2CumSumTotal + state3CumSumTotal+timeState5+ state1CumSum + state2CumSum + state3CumSum + timeState5_ComSpec + 
                              state1CumSumTotal:timeFromLastState_1 + state2CumSumTotal:timeFromLastState_2 + state3CumSumTotal:timeFromLastState_3 + timeState5:timeFromLastState_5 + timePrevCommand +
                              directCommand + (state1CumSumTotal +state2CumSumTotal +state3CumSumTotal +timeState5 +directCommand|participant), 
                            data=logit.dat,family = bernoulli(link = "logit"),
                            warmup = 2000,
                            thin=5,
                            iter = 6000,
                            chains = 5, 
                            cores = 5,
                            seed = 123)
  tmp.mod.list <- list(comply.glmer.brmZ2, comply.glmer.brmZ3)
  all.mods[[all.mods.count]] <- tmp.mod.list
  all.dat[[all.mods.count]] <- out.logit3
  all.mods.count <- all.mods.count + 1
}
saveRDS(all.mods, file = "~/Documents/oregonDPICS/data/allSilentComplyBRMSMods.RDS")
saveRDS(all.dat, file = "~/Documents/oregonDPICS/data/allSilentIterVals.RDS")

all.mods <- readRDS("~/Documents/oregonDPICS/data/allSilentComplyBRMSMods.RDS")


## Now compare the main effects across each of these models
## grab all of the summary stats here
all.sum.stats <- list()
all.sum.stats2 <- list()
for(i in 1:length(all.mods)){
  all.sum.stats[[i]] <- summary(all.mods[[i]][[2]])
  all.sum.stats2[[i]] <- summary(all.mods[[i]][[1]])
}
## Examine fixed effects here
all.sum.stats[[1]]$fixed[c(5,9,15),]
all.sum.stats[[2]]$fixed[c(5,9,15),]
all.sum.stats[[3]]$fixed[c(5,9,15),] ## Sig overall effect
all.sum.stats[[4]]$fixed[c(5,9,15),] ## Sig overall effect
all.sum.stats[[5]]$fixed[c(5,9,15),] ## Sig overall effect
all.sum.stats[[6]]$fixed[c(5,9,15),] ## Sig overall effect
all.sum.stats[[7]]$fixed[c(5,9,15),] ## Sig overall effect; interaction
all.sum.stats[[8]]$fixed[c(5,9,15),] ## Sig overall effect; interaction


all.sum.stats2[[1]]$fixed[c(5,9,15),]
all.sum.stats2[[2]]$fixed[c(5,9,15),]
all.sum.stats2[[3]]$fixed[c(5,9,15),] ## Sig command effect
all.sum.stats2[[4]]$fixed[c(5,9,15),] ## Sig command effect
all.sum.stats2[[5]]$fixed[c(5,9,15),] ## Sig overall efffect; command effect
all.sum.stats2[[6]]$fixed[c(5,9,15),] ## all sig
all.sum.stats2[[7]]$fixed[c(5,9,15),] ## all sig
all.sum.stats2[[8]]$fixed[c(5,9,15),] ## all sig
