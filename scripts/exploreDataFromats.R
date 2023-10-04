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
library("msm")

## Load data
cled.dat <- read_xlsx("~/Documents/oregonDPICS/data/101CLEDW1.xlsx")
pled.dat <- read_xlsx("~/Documents/oregonDPICS/data/101PLEDW1.xlsx")
clup.dat <- read_xlsx("~/Documents/oregonDPICS/data/101CLUPW1.xlsx") ## starting with clean up because that will be the easiest to work with
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
## Now change the data so we can run an msm analysis on it?
test.data <- wide.form1.T[which(wide.form1.T$participant=="CAPS111"),]
test.data$state <- NA
for(i in 1:6){
  if(i %in% c(1:3)){
    use.vars <- collpaseVars[[i]]
    index <- which(test.data$Block=="CLED")
    for(z in index){
      for(u in use.vars){
        if(test.data[z,u]==1){test.data$state[z] <- i}
      }
    }
  }
  if(i %in% c(4:6)){
    use.vars <- collpaseVars[[i]]
    index <- which(test.data$Block!="CLED")
    for(z in index){
      for(u in use.vars){
        if(test.data[z,u]==1){test.data$state[z] <- i-3}
      }
    }
  }
}
## Now look into the state transition matrix
statetable.msm(state=test.data$state, subject = test.data$Observation)

## Now do this for all participants
for.msm <- wide.form1.T
for.msm$state <- NA
for.msm$diffTime <- NA
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
for.msm <- wide.form1.T
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

## Now add unengaged state == 4
unengage.time <- 10
id.row <- which(diff(out.states$X3)>10)
while(length(id.row)>0){
  ## Print output
  print(id.row[1])
  ## Identify row to use
  tmp.row <- out.states[id.row[1],]
  ## Now modify row and append to end of dataframe
  out.row <- tmp.row
  out.row$X3 <- out.row$X3 + 4
  out.row$X4 <- 4
  id.row <- id.row[-1]
  if(tmp.row$X4 != 4){
    out.states <- bind_rows(out.states, out.row)
  }
}

## Now organize the counts
out.states2 <- out.states %>% group_by(X1, Block) %>% 
  arrange(X1, Block,X3)
## Drop subjects where count is only 1
foo <- out.states2 %>% group_by(Part) %>% 
  dplyr::mutate(countVal = n()) %>% 
  filter(countVal > 12) %>% 
  filter(X3 < 300)
## Now drop observations with identiical time
foo <- foo[-which(duplicated(foo[,c("X1", "X2", "X3", "Part")])),]
## Now make it one continous time series wsithin each individual
foo$X3[foo$X2=="PLED"] <- foo$X3[foo$X2=="PLED"] + 300
foo$X3[foo$X2=="CLUP"] <- foo$X3[foo$X2=="CLUP"] + 600

## Now calculate MSM model
statetable.msm(state=X4, subject = Part, data=foo)
## Go through every one of these and find where we have a 4 into a 4
for(i in unique(foo$Part)){
  vals <- statetable.msm(state=X4, subject = Part, data=foo[which(foo$Part==i),])
  if(vals[nrow(vals),ncol(vals)]>0){print(i)}
}
## For some reason CAPS266 CLED didnt get caught any of my logic so I will remove it
foo <- foo[-which(foo$Part=="CAPS266 CLED" & foo$X4==4 & foo$X3 == 4),]
statetable.msm(state=X4, subject = Part, data=foo[foo$X2=="CLED",])
statetable.msm(state=X4, subject = Part, data=foo[foo$X2=="PLED",])
statetable.msm(state=X4, subject = Part, data=foo[foo$X2=="CLUP",])


## Now calculate the msm model
q <- .25
Q <- rbind(c(q,q,q,q), c(q,q,q,q),c(q,q,q,q),c(.33,.33,.34,0))
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
model_all <- msm(X4~X3, subject = X1, data = foo, center= FALSE, qmatrix=Q,exacttimes = TRUE,covariates = ~ Block,
               gen.inits = FALSE, obstype = obstype,deathexact = deathexact, method = method, control = control)
pmatrix.msm(model_all, covariates=list(1))
pmatrix.msm(model_all, covariates=list(2))
pmatrix.msm(model_all, covariates=list(3))

## Now fit within every individual
all.mods <- list()
for(i in unique(foo$X1)){
  tmp.dat <- foo[which(foo$X1==i),]
  model_1 <- msm(X4~X3, subject = X1,data = tmp.dat, center= FALSE, qmatrix=Q,exacttimes = TRUE,covariates = ~ Block,
                 gen.inits = TRUE, obstype = obstype,deathexact = deathexact, method = method, control = control)
  all.mods[[i]] <- model_1
}

## Now go through every on one these matrices and see if we can't organize these data for some sort of analysis
all.trans <- NULL
for(q in 1:length(all.mods)){
  subj.vals <- NULL
  subj.ind <- unique(foo$X1)[q]
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

all.trans$Pattern <- paste(all.trans$To, all.trans$variable)
## Now model this for shits and gigs
vals <- lmerTest::lmer(value ~Pattern*Block+ (1|Subj), data=all.trans)
## LMER doesn't work, prob because everything sums to 1
visreg::visreg(vals, "Pattern", "Block", overlay=TRUE)

## See if we can z score these values
all.trans.wide <- pivot_wider(all.trans, id_cols=c("Subj","Block"), names_from="Pattern",values_from = "value")
names(all.trans.wide) <- gsub(pattern = " ", replacement = "_", x = names(all.trans.wide))
## Now plot these
GGally::ggpairs(all.trans.wide[,-1], mapping = aes(group=factor(Block), color=factor(Block)))
all.trans.wide[,3:18] <- scale(all.trans.wide[,3:18])[,]

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
wide.form1.C$Time_Relative_sf <- round(wide.form1.C$Time_Relative_sf, 1)
wide.form3.C$Time_Relative_sf <- round(wide.form3.C$Time_Relative_sf, 1)
foo$timeMerge <- round(foo$X3, 1)
## Now merge these into foo
foo <- merge(foo, wide.form1.C, by.x=c("X1", "X2", "timeMerge"), by.y=c("participant", "Block", "Time_Relative_sf"), all.x=TRUE)
## Now convert NA values --> 0
foo$Comply[is.na(foo$Comply)] <- 0
foo$NoOpportunity[is.na(foo$NoOpportunity)] <- 0
foo$Noncomply[is.na(foo$Noncomply)] <- 0

## Now estimate the model including the Comply, NoOppurtunity, and Noncomply
foo <- foo %>% group_by(Part) %>% 
  dplyr::mutate(countVal = n()) %>% 
  filter(countVal > 12) %>% 
  filter(X3 < 300)
model_allC <- msm(X4~X3, subject = X1, data = foo, center= FALSE, qmatrix=Q,exacttimes = TRUE,covariates = ~ Block + Comply + NoOpportunity + Noncomply,
                 gen.inits = FALSE, obstype = obstype,deathexact = deathexact, method = method, control = control)


## Now run the LMER on these data
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
iter.vals <-  seq(1,15,1)[which(300 %% 1:15 ==0)]
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

## Now try some HMM with the three class definition
library(doParallel)
library(foreach)

## Fit a mod for each participant?
all.counts <- all.block.list[[1]]
all.counts$Block[all.counts$Block=="CLED"] <- 1
all.counts$Block[all.counts$Block=="PLED"] <- 2
all.counts$Block[all.counts$Block=="CLUP"] <- 3
all.counts <- all.counts %>% group_by(Subj, Block) %>% 
  arrange(Subj, Block)
all.counts$id <- paste(all.counts$Subj, all.counts$Session)
mod <- depmix(list(Pride ~1, Dont ~ 1, Neutral ~ 1),
              data = all.counts, nstates = 4,
              family = list(poisson(),poisson(),poisson()))
# cl <- makeCluster(10)
# registerDoParallel(cl)
# all.part.mods <- foreach(i=1:10, .packages = c("depmixS4")) %dopar% {
#   i <- unique(all.counts$id)[i]
#   all.counts.tmp <- all.counts[which(all.counts$id==i),]
#   fm <- depmixS4::multistart(mod)
#   fm
# }
# stopCluster(cl)

# Run through each block count
all.mods.out.list <- list()
bin.flag <- FALSE
for(q in 1:length(all.block.list)){
  ## Identify our data
  all.counts <- all.block.list[[q]]
  ## Now organize data by participant and by block
  all.counts$Block[all.counts$Block=="CLED"] <- 1
  all.counts$Block[all.counts$Block=="PLED"] <- 2
  all.counts$Block[all.counts$Block=="CLUP"] <- 3
  all.counts <- all.counts %>% group_by(Subj, Block) %>%
    arrange(Subj, Block)
  ntimes.vec <- as.numeric(table(all.counts$Subj))
  if(bin.flag){
    ## Binarzie the Pride, Neutral, and Dont columns
    all.counts$Pride[all.counts$Pride>1] <- 1
    all.counts$Neutral[all.counts$Neutral>1] <- 1
    all.counts$Dont[all.counts$Dont>1] <- 1
  }
  cl <- makeCluster(7)
  registerDoParallel(cl)
  all.mods <- foreach(i=2:8, .packages = c("depmixS4"), .noexport = c("all.block.list")) %dopar% {
    mod <- depmix(list(Pride ~1, Dont ~ 1, Neutral ~ 1),
                  data = all.counts, nstates = i,
                  family = list(poisson(),poisson(),poisson()), ntimes = ntimes.vec)
                  #family = list(binomial(link="logit"), binomial(link="logit"), binomial(link="logit")), ntimes=ntimes.vec)
    #fm <- fit(mod, emc=em.control(rand=TRUE))
    fm <- depmixS4::multistart(mod)
  }
  stopCluster(cl)
  all.mods.out.list[[q]] <- all.mods
}
## Save the models
saveRDS(all.mods.out.list, file = "~/Documents/oregonDPICS/data/allHMMOut.RDS")
q()
all.mods.out.list <- readRDS("~/Documents/oregonDPICS/data/allHMMOut.RDS")
## obtain BIC values across all block lengths here
# Example within a list lapply(all.mods.out.list[[1]], function(x) BIC(x))
all.bic <- lapply(all.mods.out.list, function(x) lapply(x, function(x) BIC(x)))
all.aic <- lapply(all.mods.out.list, function(x) lapply(x, function(x) AIC(x)))
#all.bic.min <- lapply(all.mods.out.list, function(x) which.min(lapply(x, function(x) BIC(x))))
all.converge <- lapply(all.mods.out.list, function(x) lapply(x, function(x) x@message))
## Now try to plot these values
aic.vals <- unlist(all.aic)
bic.vals <- unlist(all.bic)
cluster.vals <- rep(2:8, length(all.mods.out.list))
block.vals <- rep(iter.vals, each = 7)
plot.dat <- data.frame(cbind(bic.vals, cluster.vals, block.vals))
plot.dat$minVal <- 0
for(b in iter.vals){
  index <- which(plot.dat$block.vals==b)
  min.block <- which.min(plot.dat$bic.vals[index])
  plot.dat$minVal[index[min.block]] <- 1
}
plot.dat$minVal2 <- 0
for(b in iter.vals){
  index <- which(plot.dat$block.vals==b)
  min.block <- which.min(plot.dat$aic.vals[index])
  plot.dat$minVal2[index[min.block]] <- 1
}
## Add a binary indicator if lowest BIC in 
plot.dat %>% ggplot(., aes(x=cluster.vals, y=bic.vals, color=minVal)) +
  geom_point() + facet_grid(block.vals ~., scales = "free")

## Now plot these data
## fin model is the length 5; 5 cluster solution?
fin.mod <- all.mods.out.list[[5]][[3]]
all.counts <- all.block.list[[5]]
all.counts$predClass <- apply(fin.mod@posterior[,-c(1)], 1, which.max)
to.plot <- melt(all.counts, id.vars = c("blockCount", "totalInt", "Block", "predClass", "Session", "Subj"))
to.plot <- to.plot[which(to.plot$variable %in% c("Pride", "Dont", "Neutral")),]
ggplot(to.plot[which(to.plot$Subj=="CAPS186"),], aes(x=blockCount, y=value, group = Session, color=factor(predClass))) + 
  geom_point() +
  facet_grid(variable~Block)
## Now try to organize this a little better
all.counts <- all.block.list[[5]]
all.counts$predClass <- apply(fin.mod@posterior[,-c(1)], 1, which.max)
to.plot <- melt(all.counts, id.vars = c("blockCount", "Block", "predClass", "Session", "Subj"))
to.plot$Uniq <- paste(to.plot$Subj, to.plot$Block, to.plot$Session)
to.plot <- to.plot[which(to.plot$variable %in% c("Pride", "Neutral", "Dont")),]
to.plot <- summarySE(data = to.plot, measurevar = "value", groupvars = c("predClass", "variable"))
ggplot(to.plot, aes(x=predClass, y=value, fill=factor(predClass))) + geom_bar(stat = "identity") +
  facet_grid(.~variable)

ggplot(to.plot, aes(x=blockCount, y=value, color=factor(predClass))) + geom_line() +
  facet_grid(Block~variable)

## Plot the mixtures of the poisson distributions here



## estimate the maodel factoring in session
all.counts <- all.block.list[[1]]
all.counts$Block[all.counts$Block=="CLED"] <- 1
all.counts$Block[all.counts$Block=="PLED"] <- 2
all.counts$Block[all.counts$Block=="CLUP"] <- 3
all.counts <- all.counts %>% group_by(Subj, Block, Session) %>% 
  arrange(Subj, Block,Session)
all.counts$Uniq <- paste(all.counts$Subj, all.counts$Session)
ntimes.vec <- as.numeric(table(all.counts$Uniq))
mod <- depmix(list(Pride ~1, Dont ~ 1),
              data = all.counts, nstates = 4,
              family = list(poisson(),poisson()), 
              ntimes = ntimes.vec)
fm <- fit(mod, emc=em.control(rand=TRUE))
fm <- depmixS4::multistart(mod)
