## Clean slate
rm(list = ls())

## Load library(s)
library("ggplot2")
library("depmixS4")
library("mHMMbayes")
library("tidyverse")
source("~/Documents/Github/adroseHelperScripts/R/afgrHelpFunc.R")
library("reshape2")
library("msm")
library("visreg")

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
plot.dat1 <- melt(wide.form1.T, id.vars = c("Time_Relative_sf", "Block", "Observation", "participant", "sessionCode", "Var.3"))
plot.dat2 <- melt(wide.form2.T, id.vars = c("Time_Relative_sf", "Block", "Observation", "participant", "sessionCode", "Var.3"))
plot.dat3 <- melt(wide.form3.T, id.vars = c("Time_Relative_sf", "Block", "Observation", "participant", "sessionCode", "Var.3"))
plot.dat <- bind_rows(plot.dat1, plot.dat2, plot.dat3)
## Now collapse variables into the 3 class system: PRIDE, Dont, Neutral
collpaseVars <- list()
collpaseVars[[1]] <- c("Behavior_Description", "Labeled_Praise", 
                       "Reflection", "Unlabeled_Praise") ## Pride
collpaseVars[[2]] <- c("Neutral_Talk") ## Neutral
collpaseVars[[3]] <- c("Direct_Command", "Direct_Command_NO", 
                       "Indirect_Command", "Indirect_Command_NO",
                       "Negative_Talk", "Question", "Command") ## Don't
collpaseVars[[4]] <- c("Behavior_Description", "Labeled_Praise", 
                       "Reflection", "Unlabeled_Praise",
                       "Direct_Command","Indirect_Command","Command") # rowSums(all.counts[,c("LP", "UP","Re", "BD", "DC", "IC")])
collpaseVars[[5]] <- c("Neutral_Talk", "Question")
collpaseVars[[6]] <- c("Direct_Command_NO", "Indirect_Command_NO","Negative_Talk") # rowSums(all.counts[,c("NeT", "DCn","ICn")])
## Now create a matrix with the time of events, and the category 
plot.dat$Pride <- 0
plot.dat$Neutral <- 0
plot.dat$Dont <- 0
plot.dat$variable <- as.character(plot.dat$variable)
plot.dat$variable[which(plot.dat$Block=="CLED" & plot.dat$variable %in% collpaseVars[[1]])] <- "Pride"
plot.dat$variable[which(plot.dat$Block=="CLED" & plot.dat$variable %in% collpaseVars[[2]])] <- "Neutral"
plot.dat$variable[which(plot.dat$Block=="CLED" & plot.dat$variable %in% collpaseVars[[3]])] <- "Dont"
plot.dat$variable[which(plot.dat$Block!="CLED" & plot.dat$variable %in% collpaseVars[[4]])] <- "Pride"
plot.dat$variable[which(plot.dat$Block!="CLED" & plot.dat$variable %in% collpaseVars[[5]])] <- "Neutral"
plot.dat$variable[which(plot.dat$Block!="CLED" & plot.dat$variable %in% collpaseVars[[6]])] <- "Dont"
## Now organize the data so we have an inactive parental state
## First experiment with this for one participant: CAPS111
test.data <- wide.form1.T[which(wide.form1.T$participant=="CAPS111"),]
## See if we cannot combine these 
test.data$Pride <- 0
test.data$Neutral <- 0
test.data$Dont <- 0
test.data$Pride[which(test.data$Block=="CLED")] <- apply(test.data[which(test.data$Block=="CLED"),collpaseVars[[1]]], 1, max)
test.data$Neutral[which(test.data$Block=="CLED")] <- test.data[which(test.data$Block=="CLED"),collpaseVars[[2]]]
test.data$Dont[which(test.data$Block=="CLED")] <- apply(test.data[which(test.data$Block=="CLED"),collpaseVars[[3]]], 1, max)
test.data$Pride[which(test.data$Block!="CLED")] <- apply(test.data[which(test.data$Block!="CLED"),collpaseVars[[4]]], 1, max)
test.data$Neutral[which(test.data$Block!="CLED")] <- apply(test.data[which(test.data$Block!="CLED"),collpaseVars[[5]]], 1, max)
test.data$Dont[which(test.data$Block!="CLED")] <- apply(test.data[which(test.data$Block!="CLED"),collpaseVars[[6]]], 1, max)
## Now do this for all participants
wide.form1.T$Wave <- 1
wide.form3.T$Wave <- 3
for.msm <- merge(wide.form1.T, wide.form3.T, all=TRUE)
for.msm$state <- NA
for.msm$diffTime <- NA
for.msm$participant <- paste(for.msm$participant, for.msm$Wave, sep="_")
## Trun all "Commands" into a 0 value
for.msm$Command[is.na(for.msm$Command)] <- 0
for(t in unique(for.msm$participant)){
  for(i in 1:6){
    if(i %in% c(1:3)){
      use.vars <- collpaseVars[[i]]
      index <- which(for.msm$Block=="CLED" & for.msm$participant==t)
      ## Now calculate the diff within participant
      for.msm$diffTime[index] <- c(NA, diff(for.msm$Time_Relative_sf[index]))
      for(z in index){
        for(u in use.vars){
          if(for.msm[z,u]==1){for.msm$state[z] <- i}
        }
      }
    }
    if(i %in% c(4:6)){
      use.vars <- collpaseVars[[i]]
      index <- which(for.msm$Block!="CLED" & for.msm$participant==t)
      for.msm$diffTime[index] <- c(NA, diff(for.msm$Time_Relative_sf[index]))
      for(z in index){
        for(u in use.vars){
          if(for.msm[z,u]==1){for.msm$state[z] <- i-3}
        }
      }
    }
  }
}
## Now grab only the CLUP task here
for.msm.clup <- for.msm[which(for.msm$Block=="CLUP"),]
## Now prep the merge column for the msm data here
for.msm.clup$Part <- strSplitMatrixReturn(for.msm.clup$participant, "_")[,1]
for.msm.clup$Wave <- strSplitMatrixReturn(for.msm.clup$participant, "_")[,2]
for.msm.clup$mergeTime <- round(for.msm.clup$Time_Relative_sf, 2)
for.msm.clup$MergeCol <- paste(for.msm.clup$Part, for.msm.clup$Block, for.msm.clup$mergeTime, for.msm.clup$Wave)

## Now create a matrix with all of the times we need and an ability to include state
out.states <- NULL
check.val <- 1000
for(i in unique(for.msm$participant)){
  for(b in unique(for.msm$Block)){
    index <- which(for.msm$participant==i & for.msm$Block==b)
    MX <- NULL
    count.val <- length(index) + 1
    for(z in 1:count.val){
      print(z)
      if(z == 1){
        tmp.row <- c(i,b,0,4,0)
        MX <- rbind(MX, tmp.row)
      }else{
        back.row <- z -1
        tmp.time <- as.numeric(for.msm[index[back.row],c("Time_Relative_sf")])
        tmp.time.diff <- tmp.time - as.numeric(MX[back.row,3])
        ## Now identify tmp.state
        if(b=="CLED"){
          for(p in 1:3){
            sum.val <- sum(for.msm[index[back.row],collpaseVars[[p]]])
            if(sum.val>0){tmp.state <- p}
          }
        }else{
          for(p in 4:6){
            sum.val <- sum(for.msm[index[back.row],collpaseVars[[p]]])
            if(sum.val>0){tmp.state <- p-3}
          }
        }
        tmp.row <- NULL
        if(tmp.time.diff>check.val & MX[back.row,4] !=4){
          insert.val <- as.numeric(MX[back.row,3]) + 2
          tmp.out <- rbind(c(i,b,insert.val,4), c(i,b,tmp.time,tmp.state))
        }else{
          tmp.out <- c(i,b,tmp.time,tmp.state,tmp.time.diff)
        }
        MX <- rbind(MX, tmp.out)
      }
    }
    out.states <- rbind(out.states, MX)
  }
}

## Clean up outstates
out.states <- data.frame(out.states)
out.states$X3 <- as.numeric(out.states$X3)
out.states$X4 <- as.numeric(out.states$X4)
out.states$X5 <- as.numeric(out.states$X5)
out.states$Part <- paste(out.states$X1, out.states$X2)
out.states$Block <- as.numeric(factor(out.states$X2, levels=c("CLED", "PLED", "CLUP")))

## Now add the disengaged state here
id.rep <- unique(out.states$X1)
block.rep <- 1:3
unengage.time <- 4
insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}
out.states.UN <- NULL
for(d in id.rep){
  print(d)
  for(b in block.rep){
    print(b)
    tmp.data <- out.states[which(out.states$X1 == d & out.states$Block == b),]
    diff.times <- c(0,diff(tmp.data$X3))
    runDiff <- FALSE
    if(length(which(diff.times>unengage.time))>0){
      runDiff <- TRUE
    }
    while(runDiff){
      index.one <- which(diff.times>unengage.time)[1]
      tmp.row <- index.one - 1
      new.row <- tmp.data[tmp.row,]
      new.row$X3 <- new.row$X3 + (unengage.time-.01)
      new.row$X4 <- 4
      tmp.data <- insertRow(tmp.data, new.row, index.one)
      diff.times <- c(0, diff(tmp.data$X3))
      if(length(which(diff.times>unengage.time))>0){
        runDiff <- TRUE
      }else{
        runDiff <- FALSE
      }
    }
    out.states.UN <- bind_rows(out.states.UN, tmp.data)  
  }
}

## Now organize the counts
out.states2 <- out.states.UN %>% group_by(X1, Block) %>% 
  arrange(X1, Block,X3)
out.states2$Wave <- strSplitMatrixReturn(out.states2$X1, "_")[,2]
out.states2$Part <- strSplitMatrixReturn(out.states2$X1, "_")[,1]
out.states2$time <- round(out.states2$X3, 2)
out.states2$MergeCol <- paste(out.states2$Part, out.states2$X2, out.states2$time, out.states2$Wave)

## Next we need to grab the compliance values and merge these onto the for.msm.clup data
wide.form1.C <- reshape2::dcast(more.dat1, Observation + Time_Relative_sf ~ Modifier_1, fun.aggregate = length)
wide.form3.C <- reshape2::dcast(more.dat3, Observation + Time_Relative_sf ~ Modifier_1, fun.aggregate = length)
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
wide.form1.C$Time_Relative_sf <- round(wide.form1.C$Time_Relative_sf, 2)
wide.form1.C$Wave <- 1
wide.form3.C$Time_Relative_sf <- round(wide.form3.C$Time_Relative_sf, 2)
wide.form3.C$Wave <- 3
wide.form1.C2 <- merge(wide.form1.C, wide.form3.C, all=TRUE)
wide.form1.C2$MergeCol <- paste(wide.form1.C2$participant, wide.form1.C2$Block, wide.form1.C2$Time_Relative_sf, wide.form1.C2$Wave)

## Now go through and identify families without and comply counts
## within each block of the task
totalComply <- wide.form1.C2 %>% group_by(participant, Block, Wave) %>% 
  mutate(complyCount = sum(Comply)) %>% 
  mutate(noncomplyCount = sum(Noncomply)) %>% 
  distinct(participant, Block, Wave, complyCount, noncomplyCount)
## Now do the same for the number of direct and indirect commands
for.msm.tmp <- for.msm
for.msm.tmp$participant <- strSplitMatrixReturn(for.msm.tmp$participant, "_")[,1]
totalCommands <- for.msm.tmp %>% group_by(participant, Block, Wave) %>% 
  mutate(totalCommands = sum(Direct_Command, Direct_Command_NO, Indirect_Command, Indirect_Command_NO, Command)) %>% 
  mutate(totalGoodCommands = sum(Direct_Command, Indirect_Command)) %>% 
    distinct(participant, Block, Wave, totalCommands, totalGoodCommands)
## Merge these
forXiaolan <- merge(totalComply, totalCommands, by=c("participant", "Block", "Wave"))
## Now create an additional factor which identifies individuals w/ no compliance & no commands
forXiaolan$noComplynoCommand <- FALSE
forXiaolan$noComplynoCommand[which(forXiaolan$complyCount==0 & forXiaolan$totalCommands==0)] <- TRUE
forXiaolan$noComplyWithCommand <- FALSE
forXiaolan$noComplyWithCommand[which(forXiaolan$complyCount==0 & forXiaolan$totalCommands>0)] <- TRUE
## Now isolate instances that are one of the two previous listed
forXiaolan <- forXiaolan[which(forXiaolan$noComplynoCommand == TRUE | forXiaolan$noComplyWithCommand == TRUE),]

## Now try merging these
for.surv <- merge(for.msm.clup, wide.form1.C2, by.x=c("MergeCol"), by.y = c("MergeCol"))
for.survUE <- merge(out.states2, wide.form1.C2, by.x=c("MergeCol"), by.y = c("MergeCol"), all.x = TRUE)

## Now arrange these values
for.surv <- for.surv %>% group_by(participant.x, Block.x) %>% 
  arrange(participant.x, Block.x,Time_Relative_sf.x)
for.surv <- for.survUE %>% group_by(X1, X2) %>% 
  arrange(X1,X2,X3) %>% 
  filter(X2=="CLUP")

## Now make sure all comply values are coded as 1's
for.surv$Comply[for.surv$Comply==2] <- 1
for.surv$Comply[is.na(for.surv$Comply)] <- 0
## Now fix the levels of the state factor
for.surv$state <- factor(for.surv$X4, levels=c(1,2,3,4))
## Now do the expanded factor levels
for.surv <- bind_cols(for.surv, model.matrix(~-1 +state, for.surv))

## Now try to organize this for the survival analysis
## I am going to have to go through ever subjects compliance counts and organize the data so death occurs when the compliance is achieved
## Lets start with a single subject here
out.surv <- NULL
for(i in unique(for.surv$X1)){
  print(i)
  ## Isolate data
  tmp.dat <- for.surv[which(for.surv$X1==i),]
  ## First identify all comply values
  complyIndex <- which(tmp.dat$Comply==1)
  ## Now identify the time from previous comply
  vals <- tmp.dat$X3[complyIndex]
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
  tmp.dat$timePrevComply <- tmp.dat$X3
  for(z in 2:length(unique(tmp.dat$complyCount))){
    lastComply <- which(tmp.dat$complyCount==z-1)
    subVal <- max(tmp.dat$X3[lastComply])
    tmp.dat$timePrevComply[which(tmp.dat$complyCount==z)] <- tmp.dat$timePrevComply[which(tmp.dat$complyCount==z)]-subVal
  }
  if(length(unique(tmp.dat$complyCount))==1){
    print("NoComply")
    tmp.dat$timePrevComply <- tmp.dat$X3
  }
  ## Now create the cumulative summation of the state variables within each state within each comply count
  out.cs.vals <- matrix(NA, nrow = nrow(tmp.dat), ncol = 5)
  out.cs.vals[,1] <- tmp.dat$complyCount
  colnames(out.cs.vals) <- c("complyCount2", paste("state", 1:4, "CumSum", sep=''))
  for(z in 1:length(unique(tmp.dat$complyCount))){
    ## Isolate the rows
    iso.rows <- which(tmp.dat$complyCount==z)
    ## Now apply cumsum opperator over the state binary indiciators
    if(length(iso.rows)>1){
      out.cols <- apply(tmp.dat[iso.rows,c("state1", "state2", "state3", "state4")], 2,cumsum)
      out.cs.vals[iso.rows,paste("state", 1:4, "CumSum", sep='')] <- out.cols
    }else{
      out.cols <- tmp.dat[iso.rows,c("state1", "state2", "state3", "state4")]
      for(r in 2:5){
        col.count <- r-1
        out.cs.vals[iso.rows,r] <- as.numeric(out.cols[col.count])
      }
    }
  }
  tmp.dat <- cbind(tmp.dat, out.cs.vals)
  out.surv <- rbind(out.surv, tmp.dat)
}  
mod <- survival::survfit(survival::Surv(time = out.surv$timePrevComply, 
                                      event = out.surv$Comply, type="right") ~ 1, data=out.surv)
plot(mod)
## Now run through every participant and try to find pl;aces where surivial curves get flagged
library(survival)
library(coxme)
library(survminer)
# for(i in unique(out.surv$participant.x)){
#   tmp.dat <- out.surv[which(out.surv$participant.x==i),]
#   surv.dat <- Surv(time=tmp.dat$timePrevComply, event = tmp.dat$Comply, type="right")
#   surv.mod <- survfit(surv.dat ~ 1)
#   print(plot(surv.mod))
# }

## Try a frailty model here
#m3 <- coxph(Surv(timePrevComply, Comply) ~ 1 + frailty(Part,distribution = "gaussian", sparse = FALSE, method = "reml"), data=out.surv)
## Now add some explanatory variables
out.surv.m <- merge(out.surv, caps.data, by.x = "Part", by.y="FAMILY")
m4 <- coxme(Surv(timePrevComply, Comply) ~ (Wave.x + Group)^2 + (1|Part), data = out.surv.m)
m4 <- coxme(Surv(timePrevComply, Comply) ~ (Wave.x + Group + state)^2 + (1|Part), data = out.surv.m)
m4 <- coxme(Surv(timePrevComply, Comply) ~ (state1CumSum + state2CumSum + state3CumSum + state4CumSum)^2 + (1|Part), data = out.surv.m)
## Now do these all in a coxph model
tmp.dat <- out.surv[which(out.surv$Comply==1),]
m4 <- coxph(Surv(timePrevComply, Comply) ~ 1, data=tmp.dat)
m5 <- survival::survfit(m4)
m4 <- coxph(Surv(timePrevComply, Comply) ~ (state1CumSum), data=tmp.dat)
m5 <- survival::survfit(m4)
plot(m5)

## Now try to add the time dependent covariate via the interval strategy for the kaplan meir analyses
## use the tmerge function here
## first idenitfy only the columns we need
## This si going to take a lot of heavy lifting so I will go ahead and work through that process in a seris of for loops
## as per usual
out.surv.int <- NULL
for(i in unique(out.surv$X1)[-c(28,185,344)]){
  print(i)
  ## Isolate data
  tmp.dat <- out.surv[which(out.surv$X1==i),]
  ## Ok, now I need to make a time start, time end variable
  ## Make sure the first row is initialized with a start time at 0
  ## and I will need to have state during the time and the indicator variable
  ## describing if a compliance was achieved in each interval
  #out.dat <- NULL
  iso.vars <- c("X1", "X2", "time", "state", "Comply", "complyCount", "timePrevComply", 
                "state1CumSum","state2CumSum","state3CumSum","state4CumSum")
  # for(z in 2:nrow(tmp.dat)){
  #   tmp.row <- tmp.dat[z,iso.vars]
  #   ## isolate variable of interest
  #   ## Now add a time start for the current event
  #   prev.row <- z - 1
  #   timeStart <- tmp.dat[prev.row,"time"]
  #   tmp.row$timeStart <- as.numeric(timeStart)
  #   out.dat <- bind_rows(out.dat, tmp.row)
  # }
  ## Now do it the correct way -- adding time from previous comply and a 0 time from each comply
  ## Now add the number of rows we need for the total number of comply counts
  out.dat.cor <- NULL
  for(z in 1:length(unique(tmp.dat$complyCount))){
    iso.dat <- tmp.dat[which(tmp.dat$complyCount==z),iso.vars]
    if(z == 1){
      iso.dat$currentSesh <- iso.dat$time
      for(J in 2:nrow(iso.dat)){
        prev.row <- J-1
        iso.dat$currentSesh[J] <- iso.dat$time[prev.row]
      }
    }else{
      ## Now identify the time of the previous comply
      prev.comply.count <- z - 1
      prev.comply.time <- tmp.dat[which(tmp.dat$complyCount==prev.comply.count & tmp.dat$Comply==1),"time"]
      iso.dat$currentSesh <- iso.dat$time - min(iso.dat$time)
    }
    out.dat.cor <- bind_rows(out.dat.cor, iso.dat)
  }
  out.surv.int <- bind_rows(out.surv.int, out.dat.cor)
}
## Now add the wave and subject indicator
out.surv.int$Part <- strSplitMatrixReturn(out.surv.int$X1, "_")[,1]
out.surv.int$wave <- strSplitMatrixReturn(out.surv.int$X1, "_")[,2]

## Now see if I can make this surv object
Surv(time = out.surv.int$currentSesh, time2 = out.surv.int$timePrevComply, event = out.surv.int$Comply)
## Remove times where start time is not greater than end time
out.surv.int <- out.surv.int[-which(out.surv.int$currentSesh >= out.surv.int$timePrevComply),]
Surv(time = out.surv.int$currentSesh, time2 = out.surv.int$timePrevComply, event = out.surv.int$Comply)
## Plot these survival curves
plot(survfit(Surv(time = out.surv.int$currentSesh, time2 = out.surv.int$timePrevComply, event = out.surv.int$Comply)~1))
## Now do this with some of the basic covariates
plot(survfit(Surv(time = out.surv.int$currentSesh, time2 = out.surv.int$timePrevComply, event = out.surv.int$Comply)~out.surv.int$wave))


## Now fit models
## Now fit all of the data together
coxphmod <- coxphmod <- coxph(Surv(currentSesh, time2 = timePrevComply, event = Comply) ~ 1, 
                  ties=c("efron"),data = out.surv.int)
modelfit <- survfit(coxphmod)
plot(modelfit)
coxphmod <- coxphmod <- coxph(Surv(currentSesh, time2 = timePrevComply, event = Comply) ~ wave, 
                              ties=c("efron"),data = out.surv.int)
modelfit <- survfit(coxphmod)
plot(modelfit)
coxphmod <- coxph(Surv(currentSesh, time2 = timePrevComply, event = Comply) ~ (state1CumSum+state2CumSum+state3CumSum+state4CumSum)^2, 
                              ties=c("efron"),data = out.surv.int)
modelfit <- survfit(coxphmod)
plot(modelfit)
## Now plot the model for the interaction terms for all variables
## but only do it for the state1 and the other cumulative summs


coxphmod <-coxph(Surv(currentSesh, time2 = timePrevComply, event = Comply) ~ (state1CumSum+state2CumSum+state3CumSum+state4CumSum)^2 + frailty(Part), 
                ties=c("efron"),data = out.surv.int)
coxphmod <-coxph(Surv(currentSesh, time2 = timePrevComply, event = Comply) ~ state1CumSum + frailty(Part), 
                ties=c("efron"),data = out.surv.int)
coxphmod <-coxph(Surv(currentSesh, time2 = timePrevComply, event = Comply) ~ state2CumSum + frailty(Part), 
                ties=c("efron"),data = out.surv.int)
coxphmod <-coxph(Surv(currentSesh, time2 = timePrevComply, event = Comply) ~ state3CumSum + frailty(Part), 
                ties=c("efron"),data = out.surv.int)
coxphmod <-coxph(Surv(currentSesh, time2 = timePrevComply, event = Comply) ~ state4CumSum + frailty(Part), 
                              ties=c("efron"),data = out.surv.int)
## Now add time invariant predictors too
out.surv.int.caps <- merge(out.surv.int, caps.data, by.x="Part", by.y = "FAMILY")
coxphmod <- coxph(Surv(currentSesh, time2 = timePrevComply, event = Comply) ~ (state1CumSum+state2CumSum+state3CumSum+state4CumSum)^2+CDINUM*wave+complyCount+frailty(Part), 
                              ties=c("efron"),data = out.surv.int.caps)
summary(coxphmod)
#coxphmod <- Surv(currentSesh, time2 = timePrevComply, event = Comply) ~ wave,data = out.surv.int.caps)
modelfit <- survfit(Surv(currentSesh, time2 = timePrevComply, event = Comply) ~ wave,data = out.surv.int.caps)
plot(modelfit)
modelfit <- survfit(Surv(currentSesh, time2 = timePrevComply, event = Comply) ~ Group+wave,data = out.surv.int.caps)
plot(modelfit)

## Now plot the expected values for these data?
## First create our new data
## FIx the CDI num effect
out.surv.int.caps$CDINUM[out.surv.int.caps$wave==1] <- 0
coxphmod <- coxph(Surv(currentSesh, time2 = timePrevComply, event = Comply) ~ (state1CumSum+state2CumSum+state3CumSum+state4CumSum)^2+CDINUM+complyCount, 
                  ties=c("efron"),data = out.surv.int.caps)
summary(coxphmod)

plot.vals <- survfit(coxphmod)
## Now make a new data frame for all of these values
#new.data = data.frame(state1CumSum = c(10,0), state2CumSum=c(10,0),state3CumSum=c(0),state4CumSum=c(0), CDINUM=c(0), wave=factor(3, levels=c(1,3)), complyCount=c(rep(1, 1)))
new.data <- expand.grid(c(1,10),c(1,10),c(1),c(1),c(10), c(3), c(1))
colnames(new.data) <- c("state1CumSum", "state2CumSum", "state3CumSum", "state4CumSum", "CDINUM", "wave", "complyCount")
new.data <- data.frame(new.data)
new.data$wave <- factor(new.data$wave, levels=c(1,3))
plot.vals <- survfit(coxphmod, newdata = new.data)
plot(plot.vals, lty = 1:3)
plot1 <- ggsurvplot(plot.vals, data=out.surv.int.caps, conf.int = TRUE, xlim=c(0,70)) + ggtitle("AllVals")# + coord_cartesian(xlim=c(0,100))
coxmemod <- coxme(Surv(currentSesh, time2 = timePrevComply, event = Comply) ~ (state1CumSum+state2CumSum+state3CumSum+state4CumSum)^2+CDINUM+complyCount + (1|Part), 
                  ties=c("efron"),data = out.surv.int.caps)
## Now go through the main effects for each of the parent variables
new.dataME1 <- data.frame(state1CumSum = c(0,10), state2CumSum=c(0),state3CumSum=c(0),state4CumSum=c(0), CDINUM=c(0), wave=factor(3, levels=c(1,3)), complyCount=c(1))
plot.vals <- survfit(coxphmod, newdata = new.dataME1)
plot1 <- ggsurvplot(plot.vals, data=out.surv.int.caps, conf.int = FALSE, xlim=c(0,100)) + ggtitle("MEPositive")# + coord_cartesian(xlim=c(0,100))
new.dataME2 <- data.frame(state1CumSum = c(0), state2CumSum=c(0,10),state3CumSum=c(0),state4CumSum=c(0), CDINUM=c(0), wave=factor(3, levels=c(1,3)), complyCount=c(1))
plot.vals <- survfit(coxphmod, newdata = new.dataME2)
plot2 <- ggsurvplot(plot.vals, data=out.surv.int.caps, conf.int = FALSE, xlim=c(0,100)) + ggtitle("MENeutral")# + coord_cartesian(xlim=c(0,100))
new.dataME3 <- data.frame(state1CumSum = c(0), state2CumSum=c(0),state3CumSum=c(0,10),state4CumSum=c(0), CDINUM=c(0), wave=factor(3, levels=c(1,3)), complyCount=c(1))
plot.vals <- survfit(coxphmod, newdata = new.dataME3)
plot3 <- ggsurvplot(plot.vals, data=out.surv.int.caps, conf.int = FALSE, xlim=c(0,100)) + ggtitle("MEDont")# + coord_cartesian(xlim=c(0,100))
new.dataME4 <- data.frame(state1CumSum = c(0), state2CumSum=c(0),state3CumSum=c(0),state4CumSum=c(0,10), CDINUM=c(0), wave=factor(3, levels=c(1,3)), complyCount=c(1))
plot.vals4 <- survfit(coxphmod, newdata = new.dataME4)
plot4 <- ggsurvplot(plot.vals4, data=out.surv.int.caps, conf.int = FALSE, xlim=c(0,100)) + ggtitle("MEDisengage")# + coord_cartesian(xlim=c(0,100))
new.dataME5 <- data.frame(state1CumSum = c(0), state2CumSum=c(0),state3CumSum=c(0),state4CumSum=c(0), CDINUM=c(0), wave=factor(c(1,3), levels=c(1,3)), complyCount=c(1))
plot.vals5 <- survfit(coxphmod, newdata = new.dataME5)
plot5 <- ggsurvplot(plot.vals5, data=out.surv.int.caps, conf.int = FALSE, xlim=c(0,100)) + ggtitle("MEwave")# + coord_cartesian(xlim=c(0,100))
new.dataME6 <- data.frame(state1CumSum = c(0), state2CumSum=c(0),state3CumSum=c(0),state4CumSum=c(0), CDINUM=c(0), wave=factor(c(3), levels=c(1,3)), complyCount=c(1,30))
plot.vals6 <- survfit(coxphmod, newdata = new.dataME6)
plot6 <- ggsurvplot(plot.vals6, data=out.surv.int.caps, conf.int = FALSE, xlim=c(0,100)) + ggtitle("MEcomplyCount")# + coord_cartesian(xlim=c(0,100))
new.dataME7 <- data.frame(state1CumSum = c(0), state2CumSum=c(0),state3CumSum=c(0),state4CumSum=c(0), CDINUM=c(0,10), wave=factor(c(3), levels=c(1,3)), complyCount=c(1))
plot.vals7 <- survfit(coxphmod, newdata = new.dataME7)
plot7 <- ggsurvplot(plot.vals7, data=out.surv.int.caps, conf.int = FALSE, xlim=c(0,100)) + ggtitle("ME_CDI")# + coord_cartesian(xlim=c(0,100))

## Interactions down here
new.dataME7 <- data.frame(state1CumSum = c(0,10,10,0), state2CumSum=c(0,0,10,10),state3CumSum=c(0),state4CumSum=c(0), CDINUM=c(0,10), wave=factor(c(3), levels=c(1,3)), complyCount=c(1))
plot.vals7 <- survfit(coxphmod, newdata = new.dataME7)
plot7 <- ggsurvplot(plot.vals7, data=out.surv.int.caps, conf.int = FALSE, xlim=c(0,100)) + ggtitle("ME_CDI")# + coord_cartesian(xlim=c(0,100))
