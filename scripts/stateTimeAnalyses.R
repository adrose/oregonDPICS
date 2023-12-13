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
                       "Direct_Command", "Indirect_Command", "Command") # rowSums(all.counts[,c("LP", "UP","Re", "BD", "DC", "IC")])
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

## Now look into the state table
statetable.msm(state=state, subject = participant, data=for.msm)
## Now calculate the msm model
q <- 1
Q <- rbind(c(q,q,q), c(q,q,q),c(q,q,q))
qnames <- c("q12","q13","q23")
obstype <- 2
center <- FALSE
deathexact <- FALSE
method <- "BFGS"
control <- list(trace = 0, REPORT = 1)

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
## Drop subjects where count is only 1
mod.dat <- out.states2 %>% group_by(Part) %>% 
  dplyr::mutate(countVal = n()) %>% 
  filter(countVal > 12) %>% 
  filter(X3 < 300)
## Now drop observations with identical time
mod.dat <- mod.dat[-which(duplicated(mod.dat[,c("X1", "X2", "X3", "Part")])),]
## Now make it one continous time series wsithin each individual
mod.dat$OrigTime <- mod.dat$X3
mod.dat$X3[mod.dat$X2=="PLED"] <- mod.dat$X3[mod.dat$X2=="PLED"] + 300
mod.dat$X3[mod.dat$X2=="CLUP"] <- mod.dat$X3[mod.dat$X2=="CLUP"] + 600

## Now calculate MSM model
statetable.msm(state=X4, subject = Part, data=mod.dat)

## Plot a single partiicpant's time series
mod.dat[which(mod.dat$X1=="CAPS111_1"),] %>% ggplot(., aes(x=X3, y=X4)) +
  #geom_line() +
  geom_point() + 
  facet_grid(. ~ Block, scales = "free")

## Now estimate time spent in each state
state.time <- NULL
for(d in id.rep){
  tmp.out <- data.frame(id=d, block=1:3, time_1=NA, time_2 = NA, time_3 = NA, time_4 = NA)
  for(b in 1:3){
    tmp.dat <- mod.dat[which(mod.dat$X1==d & mod.dat$Block==b),]
    ## Identify all values
    all.trans.vals <- cbind(tmp.dat$X4, c(0,diff(tmp.dat$X3)), cumsum(c(0, diff(tmp.dat$X3))))
    ## Go through all values and add the time within each state
    for(s in 1:4){
      index.vals <- which(all.trans.vals[,1]==s)
      if(length(index.vals)>0){
        tot.count <- 0
        for(i in index.vals){
          if(i+1 <= nrow(all.trans.vals)){
            tot.count <- tot.count + diff(c(all.trans.vals[i,3],all.trans.vals[i+1,3]))
          }else{
            tot.count <- tot.count + diff(c(all.trans.vals[i,3],300))
          }
        }
        tmp.col <- paste("time_", s, sep='')
        tmp.out[b,tmp.col] <- tot.count
      }
    }
  }
  state.time <- bind_rows(state.time, tmp.out)
}
## Add a wave effect for the state time
state.time$wave <- strSplitMatrixReturn(state.time$id, "_")[,2]
state.time$part <- strSplitMatrixReturn(state.time$id, "_")[,1]

## Now run some regression models here -- with BRMS for consistency
## First reshape the data
mod.dat <- reshape2::melt(state.time, id.vars = c("id", "block", "wave", "part"))
mod.dat$block <- factor(mod.dat$block)
## Turn values == NA into 0
mod.dat$value[is.na(mod.dat$value)] <- 0
mod.dat <- merge(mod.dat, caps.data, by.x="part", by.y="FAMILY")

## Now train the model
mod.out <- brms::brm(value ~ (block + wave + variable+Group)^4 + (1|part), data=mod.dat, cores = 5)


## Now plot these predictions
plot.dat <- mod.dat %>% filter(!is.na(Group))
plot.dat$predVals <- predict(mod.out)[,1]
plot.vals <- summarySE(plot.dat, measurevar = "predVals", groupvars = c("block", "wave", "variable", "Group"))
plot.vals %>% ggplot(., aes(x=variable, y=predVals, group=block, color=block, fill=block)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = predVals-sd, ymax = predVals+sd), position = "dodge", color="black") +
  facet_grid(wave ~ Group) +
  theme_bw()

## Now look and see if we can estimate the number of compliances and non compliances form the kids with these data
## First organize the compliance data
## Now attach the child behavior to each of these and use this as a time variant covars
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
## Now merge these into foo
wide.form1.C2 <- merge(wide.form1.C, wide.form3.C, all=TRUE)

## Now turn any twos into 1 values
wide.form1.C2[wide.form1.C2[,3]==2,3] <- 1
wide.form1.C2[wide.form1.C2[,4]==2,4] <- 1
wide.form1.C2[wide.form1.C2[,5]==2,5] <- 1

## Now go through each family and identify number of comply
output.data <- NULL
for(i in unique(wide.form1.C2$participant)){
  for(w in c(1,3)){
  for(z in c("CLED", "PLED", "CLUP")){
    tmp.dat <- wide.form1.C2[which(wide.form1.C2$participant==i & wide.form1.C2$Block==z & wide.form1.C2$Wave==w),]
    ## Now sum the total number of comply's total number of non comply's and total number of no opps
    if(dim(tmp.dat)[1]>0){
      numComply <- sum(tmp.dat$Comply)
      numNoOp <- sum(tmp.dat$NoOpportunity)
      numNoncomp <- sum(tmp.dat$Noncomply)
      ## Now prep the output row here
      if(z=="CLED"){block <- 1}
      if(z=="PLED"){block <- 2}
      if(z=="CLUP"){block <- 3}
      out.row <- cbind(i, block, w,numComply, numNoOp, numNoncomp)
      output.data <- rbind(output.data, out.row)
    }
  }
  }
}
output.data <- data.frame(output.data)
output.data$block <- factor(output.data$block)
## Now attached these and make them the output
## Attach these to the mod.dat data
complyMod <- merge(state.time, output.data, by.x=c("part", "wave", "block"), by.y=c("i", "w", "block"))
complyMod <- complyMod[complyMod$block==3,]
## Now melt these
complyModDat <- reshape2::melt(complyMod, id.vars = c("part", "wave", "block", "id","time_1", "time_2", "time_3", "time_4"))
str(complyModDat)
complyModDat$value <- as.numeric(complyModDat$value)
complyModDatScale <- complyModDat
complyModDatScale[,5:8] <- scale(complyModDatScale[,5:8])[,]

## Now model these data0
mod.comply <- brms::brm(value ~ (time_1+time_2+time_3+time_4+variable)^2 + (1|part), data=complyModDatScale, cores = 1, chains=1, family = "poisson", control = list(max_treedepth=15, adapt_delta=.99))
mod.comply <- lme4::lmer(value ~ (time_1+time_2+time_3+time_4+variable) + (1|part), data=complyModDatScale)
