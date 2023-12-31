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
## Now plot these time series for the 3 class categories
plot.dat[plot.dat$participant=="CAPS125",] %>% ggplot(., aes(x=Time_Relative_sf, y = value)) +
  geom_point() +
  facet_grid(variable ~ Block)

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
## Now plot this?
plot.test <- reshape2::melt(test.data[,c("Time_Relative_sf","participant", "Block", "Pride", "Neutral", "Dont")], id.vars=c("Time_Relative_sf","participant", "Block"))
plot.test %>% ggplot(., aes(x=Time_Relative_sf,y=value)) +
  geom_point() +
  facet_grid(Block ~ variable)

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
model_1 <- msm(state~Time_Relative_sf, subject = Observation, data = for.msm, center= center, qmatrix=Q,exacttimes = TRUE,
               gen.inits = TRUE, obstype = obstype, deathexact = deathexact, method = method, control = control)


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
p1 <- mod.dat[which(mod.dat$X1=="CAPS111_1" & mod.dat$Block=="3"),] %>% ggplot(., aes(x=X3, y=X4)) +
  #geom_line() +
  geom_point() + 
  #facet_grid(Block ~ ., scales = "free") +
  theme_bw() +
  theme(text=element_text(size=20), axis.text.x = element_text(angle=35)) +
  ylab("State") +
  xlab("Time (s)")
rm(p1)

## Now estimate time spent in each state
state.time <- NULL
for(d in id.rep){
  tmp.out <- data.frame(id=d, block=1:3, time_1=NA, time_2 = NA, time_3 = NA, time_4 = NA)
  for(b in 1:3){
    tmp.dat <- mod.dat[which(mod.dat$X1==d & mod.dat$Block==b),]
    ## Identify all values
    all.trans.vals <- cbind(tmp.dat$X4, c(0,diff(tmp.dat$X3)), cumsum(c(0, diff(tmp.dat$X3))))
    ## Go through all values and add the time wihtin each state
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
findoutlier <- function(x) {
  return(x < quantile(x, .25) - 1.5*IQR(x) | x > quantile(x, .75) + 1.5*IQR(x))
}
plot.dat <- melt(state.time, id.vars = c("id","block","wave", "part")) %>% 
  group_by(variable) %>% 
  filter(!is.na(value)) %>% 
  mutate(value = as.numeric(value)) %>% 
  mutate(outlier = ifelse(findoutlier(value), part, NA))

## Now save these times to disk
state.time$wave <- paste("Wave_", state.time$wave, sep='')
state.time$block <- paste("Block_", state.time$block, sep='')
out.vals <- tidyr::pivot_wider(state.time[,-1], id_cols = c("part"), names_from = c("block","wave"), values_from = c("time_1", "time_2", "time_3", "time_4"))
write.csv(out.vals, "./Documents/oregonDPICS/data/stateTime.csv", quote=F, row.names=F)

## Now calculate the msm model
q <- .25
Q <- rbind(c(q,q,q,q), c(q,q,q,q),c(q,q,q,q),c(q,q,q,q))
covariates = list(
  "1-2" = ~ Block,
  "1-3" = ~ Block,
  "1-4" = ~ Block,
  "2-1" = ~ Block,
  "2-3" = ~ Block,
  "2-4" = ~ Block,
  "3-1" = ~ Block,
  "3-2" = ~ Block,
  "3-4" = ~ Block,
  "4-1" = ~ Block,
  "4-2" = ~ Block,
  "4-3" = ~ Block)
obstype <- 2
center <- FALSE
deathexact <- FALSE
method <- "BFGS"
control <- list(trace = 0, REPORT = 1)
model_all <- msm(X4~X3, subject = X1, data = mod.dat, center= FALSE, qmatrix=Q,exacttimes = TRUE,covariates = ~ Block,
                 gen.inits = FALSE, obstype = obstype,deathexact = deathexact, method = method, control = control)
pmatrix.msm(model_all, covariates=list(1))
pmatrix.msm(model_all, covariates=list(2))
pmatrix.msm(model_all, covariates=list(3))

## Now fit within every individual
all.mods <- list()
for(i in unique(mod.dat$X1)){
  tmp.dat <- mod.dat[which(mod.dat$X1==i),]
  model_1 <- msm(X4~X3, subject = X1,data = tmp.dat, center= FALSE, qmatrix=Q,exacttimes = TRUE,covariates = ~ Block,
                 gen.inits = TRUE, obstype = obstype,deathexact = deathexact, method = method, control = control)
  all.mods[[i]] <- model_1
}

all.mods2 <- list()
for(i in unique(mod.dat$Part)){
  tmp.dat <- mod.dat[which(mod.dat$Part==i),]
  model_1 <- msm(X4~OrigTime, subject = X1,data = tmp.dat, center= FALSE, qmatrix=Q,exacttimes = TRUE,
                 gen.inits = TRUE, obstype = obstype,deathexact = deathexact, method = method, control = control)
  all.mods2[[i]] <- model_1
}

## Now go through every on one these matrices and see if we can't organize these data for some sort of analysis
all.trans <- NULL
for(q in 1:length(all.mods)){
  subj.vals <- NULL
  subj.ind <- unique(mod.dat$X1)[q]
  for(i in 1:3){
    vals.tmp <- data.frame(pmatrix.msm(all.mods[[q]], covariates=list(i)))
    vals.tmp <- vals.tmp[complete.cases(vals.tmp),]
    vals.tmp <- data.frame(vals.tmp)
    vals.tmp$To <- rownames(vals.tmp)
    vals.tmp <- reshape2::melt(vals.tmp, id.vars = "To")
    vals.tmp$Subj <- subj.ind
    vals.tmp$Block <- i
    subj.vals <- rbind(subj.vals, vals.tmp)
  }
  all.trans <- bind_rows(all.trans, subj.vals)
}
all.trans1 <- all.trans

all.trans <- NULL
for(q in 1:length(all.mods2)){
  subj.vals <- NULL
  subj.ind <- unique(mod.dat$X1)[q]
  for(i in 1){
    vals.tmp <- data.frame(pmatrix.msm(all.mods2[[q]]))
    vals.tmp <- vals.tmp[complete.cases(vals.tmp),]
    vals.tmp <- data.frame(vals.tmp)
    vals.tmp$To <- rownames(vals.tmp)
    vals.tmp <- reshape2::melt(vals.tmp, id.vars = "To")
    vals.tmp$Subj <- subj.ind
    vals.tmp$Block <- i
    subj.vals <- rbind(subj.vals, vals.tmp)
  }
  all.trans <- bind_rows(all.trans, subj.vals)
}
all.trans2 <- all.trans
all.trans <- all.trans1
all.trans$Pattern <- paste(all.trans$To, all.trans$variable)
all.trans$Block <- as.factor(all.trans$Block)
## Now model this for shits and gigs
vals <- lmerTest::lmer(value ~Pattern*Block+ (1|Subj), data=all.trans)
## LMER doesn't work, prob because everything sums to 1
visreg::visreg(vals, "Pattern", "Block", overlay=TRUE)

# ## See if we can z score these values
all.trans.wide <- pivot_wider(all.trans, id_cols=c("Subj","Block"), names_from="Pattern",values_from = "value")
names(all.trans.wide) <- gsub(pattern = " ", replacement = "_", x = names(all.trans.wide))
# ## Now plot these
all.trans.wide[,3:18] <- scale(all.trans.wide[,3:18])[,]
# ## Now run a model within each of the 




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
mod.dat <- data.frame(mod.dat)
mod.dat$timeMerge <- as.numeric(round(mod.dat$OrigTime, 2))
mod.dat$Wave <- strSplitMatrixReturn(mod.dat$X1, "_")[,2]
## Now merge these into foo
wide.form1.C2 <- merge(wide.form1.C, wide.form3.C, all=TRUE)
mod.dat$Part <- strSplitMatrixReturn(mod.dat$X1, "_")[,1]
mod.dat$MergeCol <- paste(mod.dat$Part, mod.dat$X2, mod.dat$timeMerge, mod.dat$Wave)
wide.form1.C2$MergeCol <- paste(wide.form1.C2$participant, wide.form1.C2$Block, wide.form1.C2$Time_Relative_sf, wide.form1.C2$Wave)
mod.dat2 <- merge(mod.dat, wide.form1.C2, by.x=c("Part", "X2", "timeMerge", "Wave"), by.y=c("participant", "Block", "Time_Relative_sf", "Wave"), all.x = TRUE)

## Now convert NA values --> 0
mod.dat2$Comply[is.na(mod.dat2$Comply)] <- 0
mod.dat2$Comply[mod.dat2$Comply==2] <- 1
mod.dat2$NoOpportunity[is.na(mod.dat2$NoOpportunity)] <- 0
mod.dat2$NoOpportunity[mod.dat2$NoOpportunity==2] <- 1
mod.dat2$Noncomply[is.na(mod.dat2$Noncomply)] <- 0
mod.dat2$Noncomply[mod.dat2$Noncomply==2] <- 1

mod.dat2 <- mod.dat2 %>% group_by(Part, Block) %>% 
  arrange(X1, Block,X3)

mod.dat2$Wave[mod.dat2$Wave==1] <- 1
mod.dat2$Wave[mod.dat2$Wave==3] <- 2


## Now estimate the model including the Comply, NoOppurtunity, and Noncomply
# mod.dat2 <- mod.dat2 %>% group_by(Part) %>% 
#   dplyr::mutate(countVal = n()) %>% 
#   filter(countVal > 12)
covariates = list(
  "1-2" = ~ Wave+ Comply+ Noncomply,
  "1-3" = ~ Wave+ Comply+ Noncomply,
  "1-4" = ~ Wave+ Comply+ Noncomply,
  "2-1" = ~ Wave ,
  "2-3" = ~ Wave ,
  "2-4" = ~ Wave ,
  "3-1" = ~ Wave + NoOpportunity ,
  "3-2" = ~ Wave + NoOpportunity ,
  "3-4" = ~ Wave + NoOpportunity ,
  "4-1" = ~ Wave ,
  "4-2" = ~ Wave ,
  "4-3" = ~ Wave )
mod.dat23 <- mod.dat2[mod.dat2$Block==3,]
model_allC <- msm(X4~X3, subject = X1, data = mod.dat23, center= FALSE, qmatrix=Q,exacttimes = TRUE,covariates = covariates,
                  gen.inits = FALSE, obstype = obstype,deathexact = deathexact, method = method, control = control)

round(pmatrix.msm(model_allC, covariates = data.frame( Wave=3, Comply=0, NoOpportunity=0, Noncomply=0)),3)
round(pmatrix.msm(model_allC, covariates = data.frame( Wave=3, Comply=0, NoOpportunity=5, Noncomply=0)),3)
round(pmatrix.msm(model_allC, covariates = data.frame( Wave=3, Comply=0, NoOpportunity=0, Noncomply=5)),3)
round(pmatrix.msm(model_allC, covariates = data.frame( Wave=3, Comply=5, NoOpportunity=0, Noncomply=0)),3)


## Now do individual specific
all.modsC <- list()
for(i in unique(mod.dat2$X1)){
  tmp.dat <- mod.dat2[which(mod.dat2$X1==i),]
  model_1 <- msm(X4~X3, subject = X1,data = tmp.dat, center= FALSE, qmatrix=Q,exacttimes = TRUE,covariates = ~ Block + Comply + Noncomply,
                 gen.inits = TRUE, obstype = obstype,deathexact = deathexact, method = method, control = control)
  all.modsC[[i]] <- model_1
}

## Now go through and grab all of the actually observed transitions form the raw data
all.trans.counts <- NULL
for(i in unique(mod.dat2$X1)){
  for(b in 1:3){
    tmp.dat <- mod.dat2[which(mod.dat2$X1==i & mod.dat2$Block==b),]
    if(dim(tmp.dat)[1]>0){
      est.mat <- statetable.msm(state = X4, subject = X1, data = tmp.dat)
      out.mat <- melt(est.mat)
      out.mat$Part <- i
      out.mat$Block <- b
      all.trans.counts <- rbind(all.trans.counts, out.mat)
    }
  }
}

## Now see if we can do some regression on these data
all.trans.counts$Wave <- factor(strSplitMatrixReturn(all.trans.counts$Part, "_")[,2])
all.trans.counts$Subj <- factor(strSplitMatrixReturn(all.trans.counts$Part, "_")[,1])
all.trans.counts$trans.pat <- paste(all.trans.counts$from, all.trans.counts$to, sep="_")
all.trans.counts$Block <- factor(all.trans.counts$Block)
mod <- lmerTest::lmer(value ~ (trans.pat + Block + Wave)^3 + (1|Part), data = all.trans.counts)
p1 <- visreg(mod, "trans.pat", "Block", cond = list(Wave="1"), gg=TRUE) + ggtitle("Wave 1") + coord_cartesian(ylim=c(-10, 90))
p2 <- visreg(mod, "trans.pat", "Block", cond = list(Wave="3"), gg=TRUE) + ggtitle("Wave 3") + coord_cartesian(ylim=c(-10, 90))
multiplot(p1, p2, cols = 1)
## Looks like GLMER is not working so I will transition to BRMS here
## Now do a poisson
#mod <- lme4::glmer(value ~ (trans.pat + Block + Wave)^3 + (1|Subj), data = all.trans.counts, family = "poisson")
  
## Now attach the data 
all.trans.countsCAPS <- merge(all.trans.counts, caps.data, by.x=c("Subj"), by.y="FAMILY")
#mod <- lmerTest::lmer(value ~ (trans.pat + Block + Wave + Group)^4 + (1|Part), data = all.trans.countsCAPS)
#mod <- lme4::glmer(value ~ (trans.pat + Block + Wave + Group)^4 + (1|Subj), data = all.trans.counts, family = "poisson")
modP <- brms::brm(value ~ (trans.pat + Block + Wave + Group)^4 + (1|Subj), data = all.trans.countsCAPS, family = "poisson",iter = 9000, warmup = 3000, thin = 50,cores = 5, chains = 5,seed=16, control = list(max_treedepth=15, adapt_delta=.9))
saveRDS(modP, file="~/Documents/oregonDPICS/data/possionfourWay.RDS")
modP2 <- brms::brm(value ~ (trans.pat + Block + Wave + PDINUM)^4 + (1|Subj), data = all.trans.countsCAPS, family = "poisson",iter = 3000, warmup = 1000, thin = 25,cores = 5, chains = 5,seed=16, control = list(max_treedepth=15, adapt_delta=.9))
saveRDS(modP2, file="~/Documents/oregonDPICS/data/possionfourWayPDI.RDS")
modP <- readRDS("~/Documents/oregonDPICS/data/possionfourWay.RDS")
plot.dat <- all.trans.countsCAPS %>% 
  filter(!is.na(Group))
plot.dat$fitted <- predict(modP)[,1]
## Now plot these effects'
plot.vals <- summarySE(data = plot.dat, measurevar = "fitted", groupvars = c("trans.pat", "Block", "Wave", "Group"))
plot.vals %>% ggplot(., aes(x=trans.pat, y=fitted, group=Block, color=Block, fill=Block)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(Wave ~ Group)

## Now look into block differences
p11 <- visreg(mod, "trans.pat","Wave",overlay=TRUE, cond = list(Group="Control", Block=1), gg=TRUE) + ggtitle("Control:1") + coord_cartesian(ylim=c(-1, 30))
p11$layers[[17]] <- NULL
p31 <- visreg(mod, "trans.pat","Wave",overlay=TRUE, cond = list(Group="Intervention", Block=1), gg=TRUE) + ggtitle("Intervention:1") + coord_cartesian(ylim=c(-1, 30))
p31$layers[[17]] <- NULL
p12 <- visreg(mod, "trans.pat","Wave",overlay=TRUE, cond = list(Group="Control", Block=2), gg=TRUE) + ggtitle("Control:2") + coord_cartesian(ylim=c(-1, 30))
p12$layers[[17]] <- NULL
p32 <- visreg(mod, "trans.pat","Wave",overlay=TRUE, cond = list(Group="Intervention", Block=2), gg=TRUE) + ggtitle("Intervention:2") + coord_cartesian(ylim=c(-1, 30))
p32$layers[[17]] <- NULL
p13 <- visreg(mod, "trans.pat","Wave",overlay=TRUE, cond = list(Group="Control", Block=3), gg=TRUE) + ggtitle("Control:3") + coord_cartesian(ylim=c(-1, 30))
p13$layers[[17]] <- NULL
p33 <- visreg(mod, "trans.pat","Wave",overlay=TRUE, cond = list(Group="Intervention", Block=3), gg=TRUE) + ggtitle("Intervention:3") + coord_cartesian(ylim=c(-1, 30))
p33$layers[[17]] <- NULL
multiplot(p11,p31,p12,p32,p13,p33, cols = 3)

## Now do the visit counts
mod <- lmerTest::lmer(value ~ (trans.pat + Block + Wave + PDINUM + CDINUM)^5 + (Wave|Part), data = all.trans.countsCAPS)

## Now run these same analyses using the estimated transition probabilities
all.trans <- NULL
for(q in 1:length(all.modsC)){
  subj.vals <- NULL
  subj.ind <- unique(mod.dat$X1)[q]
  for(i in 1:3){
    vals.tmp <- data.frame(pmatrix.msm(all.modsC[[q]], covariates=list(i, 0,0)))
    vals.tmp <- vals.tmp[complete.cases(vals.tmp),]
    vals.tmp <- data.frame(vals.tmp)
    vals.tmp$To <- rownames(vals.tmp)
    vals.tmp <- reshape2::melt(vals.tmp, id.vars = "To")
    vals.tmp$Subj <- subj.ind
    vals.tmp$Block <- i
    subj.vals <- rbind(subj.vals, vals.tmp)
  }
  all.trans <- bind_rows(all.trans, subj.vals)
}
all.trans$Wave <- factor(strSplitMatrixReturn(all.trans$Subj, "_")[,2])
all.trans$Subj <- factor(strSplitMatrixReturn(all.trans$Subj, "_")[,1])
all.trans$trans.pat <- paste(all.trans$To, all.trans$variable, sep="_")
all.trans$Block <- factor(all.trans$Block)
## Now add caps data
all.trans.CAPS <- merge(all.trans, caps.data, by.x=c("Subj"), by.y="FAMILY")
full.mod <- lmerTest::lmer(value ~ (trans.pat + Wave + Block)^3 + (1|Subj), data=all.trans)
p1 <- visreg::visreg(full.mod, "trans.pat", cond = list(Wave=1, Block = 3), overlay=TRUE, gg=TRUE) + coord_cartesian(ylim=c(0,1)) + theme(axis.text.x = element_text(angle = 35)) ++ ggtitle("Wave 1")
p2 <- visreg::visreg(full.mod, "trans.pat", cond = list(Wave=3, Block = 3), overlay=TRUE, gg=TRUE) + coord_cartesian(ylim=c(0,1)) + theme(axis.text.x = element_text(angle = 35)) ++ ggtitle("Wave 3")
p1$layers[[17]] <- NULL
p2$layers[[17]] <- NULL
multiplot(p1, p2, cols=2)
full.mod.2 <- lmerTest::lmer(value ~ (trans.pat + Wave + Block + Group)^4 + (1|Subj), data=all.trans.CAPS)
## Now try a MANCOVA?


for(z in unique(all.trans$trans.pat)){
  mod <- lmerTest::lmer(value ~ (Wave + Block)^3 + (1|Subj), data=all.trans[which(all.trans$trans.pat==z),])
  print(car::Anova(mod))
}

save.image(file="Documents/oregonDPICS/data/rImage.RDS")