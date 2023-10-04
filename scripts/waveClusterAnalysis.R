## This R file will be used to run a cluster analysis exploring the number of behaviors endorsed within each of the DPICs tasks
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
library("poLCA")
library("mclust")
library("LMest")

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
## Now explore the three factor EFA to see how these counts might load onto one another
all.counts <- merge(wide.form1.T, wide.form3.T, all=TRUE)
psych::fa.parallel(all.counts[,c(5:38)])
fa.model <- psych::fa(all.counts[,c(5:38)], nfactors = 3)

## Now plot some growth across the waves?
tmp.plot.1 <- wide.form1.T[,c(1, 39:47)]
tmp.plot.1$wave <- 1
tmp.plot.3 <- wide.form3.T[,c(1, 38:46)]
tmp.plot.3$wave <- 3
plot.dat <- bind_rows(tmp.plot.1, tmp.plot.3)
plot.dat <- reshape2::melt(plot.dat, id.vars = c("participant", "wave"))
## Now plot
plot.dat %>% ggplot(., aes(x=wave, y=value)) +
  geom_line(aes(group=participant)) +
  geom_smooth(method="lm") +
  facet_wrap(variable~.)


## Cluster data here
# Model to explore
# EII: Spherical, equal volume
# VII: spherical, unequal volume
# EEI: diagonal, equal volume and shape
# EEE Ellipsoidal, equal volume, shape, and orientation
# EVV ellipsoidal, equal volume
# XII
modName <- c("EII", "EEI", "EEE", "EVV", "XII") ## Equal volumes
modName <- c("VII", "VEI", "VEI")
mod1 <- Mclust(wide.form1.T[,c(39:47)], G=2:12)#, modelNames = modName)
mod1 <- Mclust(wide.form1.T[,c(39,41:42,44:45,47)], G=2:12, modelNames=c("EVV"))#, modelNames = modName)
mod1A <- Mclust(wide.form1.T[,c(39:47)], G=2:12, modelNames = modName)
summary(mod1, parameters = TRUE)
plot(mod1, what = "classification")
plot(mod1A, what = "classification")
wide.form1.T$Group <- factor(predict(mod1)$classification)
mod1_2Group <- GGally::ggpairs(wide.form1.T[,c(39:48)], mapping = aes(group=Group, color=Group))

# wide.form1.T$Group <- factor(predict(mod1A)$classification)
# GGally::ggpairs(wide.form1.T[,c(39:48)], mapping = aes(group=Group, color=Group))

## Now force a 4 class solution
mod1 <- Mclust(wide.form1.T[,c(39:47)], G=4)#, modelNames = modName)
summary(mod1, parameters = TRUE)
plot(mod1, what = "classification")
wide.form1.T$Group <- factor(predict(mod1)$classification)
mod1_4Group <- GGally::ggpairs(wide.form1.T[,c(39:48)], mapping = aes(group=Group, color=Group))

## Now plot these data a little better?
plot.dat <- reshape2::melt(wide.form1.T[,c(1, 39:48)], id.vars = c("participant", "Group"))
ggplot(plot.dat, aes(x=variable, y=value, group=participant, color=Group)) +
  geom_point() +
  geom_line() + 
  facet_grid(Group ~ .)

## Do wave 2 data?
mod2 <- Mclust(wide.form2.T[,c(39:47)], G=2:12)#, modelNames = modName)
#plot(mod2, what = "classification")

# Onto wave 3
mod3 <- Mclust(wide.form3.T[,c(38:46)], G=2:5)#, modelNames = modName)
summary(mod3, parameters = TRUE)
plot(mod3, what = "classification")
wide.form3.T$Group <- factor(predict(mod3)$classification)
GGally::ggpairs(wide.form3.T[,c(39:47)], mapping = aes(group=Group, color=Group))
mod3_2Group <- GGally::ggpairs(wide.form3.T[,c(38:47)], mapping = aes(group=Group, color=Group))
plot(mod3, what="BIC")

## Now force a four factor 
mod3 <- Mclust(wide.form3.T[,c(38:46)], G=4)#, modelNames = modName)
wide.form3.T$Group <- factor(predict(mod3)$classification)
mod3_4Group <- GGally::ggpairs(wide.form3.T[,c(38:47)], mapping = aes(group=Group, color=Group))


## Now do a mixed model
## First use all data
modA <- Mclust(bind_rows(wide.form1.T[,c(39:47)], wide.form2.T[,c(39:47)],wide.form3.T[,c(38:46)]), modelNames = c("VVE"), G=2)
summary(modA, parameters = TRUE)
plot(modA, what = "classification")
tmp.dat <- bind_rows(wide.form1.T[,c(39:47)], wide.form2.T[,c(39:47)],wide.form3.T[,c(38:46)])
tmp.dat$Group <- factor(predict(modA)$classification)
GGally::ggpairs(tmp.dat, mapping = aes(group=Group, color=Group))

## Now use just the W1 and W3 data -- with the model that was consistent across the two wave specific models
modA <- Mclust(bind_rows(wide.form1.T[,c(39:47)],wide.form3.T[,c(38:46)]), modelNames = c("VVE"), G=2)
summary(modA, parameters = TRUE)
plot(modA, what = "classification")
tmp.dat <- bind_rows(wide.form1.T[,c(39:47)],wide.form3.T[,c(38:46)])
tmp.dat$Group <- factor(predict(modA)$classification)
GGally::ggpairs(tmp.dat, mapping = aes(group=Group, color=Group))

## Now explore all fits
modA <- Mclust(bind_rows(wide.form1.T[,c(39:47)],wide.form3.T[,c(38:46)]), G=2:12)
summary(modA, parameters = TRUE)
#plot(modA, what = "classification")
tmp.dat <- bind_rows(wide.form1.T[,c(1, 39:47)],wide.form3.T[,c(1, 38:46)])
tmp.dat$Group <- factor(predict(modA)$classification)
modA_4Group <- GGally::ggpairs(tmp.dat[,-1], mapping = aes(group=Group, color=Group))
plot(modA, what="BIC")

## Now do transition analysis
tmp.dat.1 <- wide.form1.T[,c(1, 39:47)]
tmp.dat.1$wave <- 1
tmp.dat.3 <- wide.form3.T[,c(1, 38:46)]
tmp.dat.3$wave <- 2

## First just start by a lmer model looking for differences from t1 and t3
all.long <- bind_rows(tmp.dat.1, tmp.dat.3)
modA <- Mclust(all.long[,2:10], G=4)
all.long$Group <- factor(predict(modA)$classification)
all.long <- reshape2::melt(all.long, id.vars=c("participant", "wave", "Group"))
lmer.model <- lmerTest::lmer(value ~ wave * variable + (1|participant), data=all.long)
visreg::visreg(lmer.model, "wave", "variable", overlay=FALSE)
all.long$wave2 <- factor(all.long$wave)
lmer.model <- lmerTest::lmer(value ~ wave2 * variable + (1|participant), data=all.long)
visreg::visreg(lmer.model, "wave2", "variable", overlay=FALSE)

## Now attach the Caps data and look for any interesting main effects
all.long <- merge(all.long, caps.data, by.x = "participant", by.y = "FAMILY", all.x = TRUE)
all.long <- all.long[!is.na(all.long$Group.y),]
all.long <- all.long[!is.na(all.long$variable),]
lmer.mod.2 <- lmerTest::lmer(value ~ (wave + variable + Group.y)^3 + (1|participant), data=all.long)
car::Anova(lmer.mod.2)
## Looks like we have a three-way interaction here
## Remove NA groups and NA varaibles
p1 <- visreg::visreg(lmer.mod.2, "wave", "variable", cond = list("Group.y"="Intervention"),overlay=FALSE, gg=TRUE) + coord_cartesian(ylim=c(0, 90)) + ggtitle("Int")
p2 <- visreg::visreg(lmer.mod.2, "wave", "variable", cond = list("Group.y"="Control"),overlay=FALSE, gg=TRUE) + coord_cartesian(ylim=c(0, 90)) + ggtitle("Con")
multiplot(p1, p2, cols=1)

## Now do PLED count
lmer.mod.3 <- lmerTest::lmer(value ~ (wave + variable + CDINUM)^4 + (1|participant), data=all.long)
car::Anova(lmer.mod.3)
p1 <- visreg::visreg(lmer.mod.3, "CDINUM", "variable", cond = list("wave"=1),overlay=FALSE, gg=TRUE) + coord_cartesian(ylim=c(0, 90)) + ggtitle("PLED count")
p2 <- visreg::visreg(lmer.mod.3, "CDINUM", "variable", cond = list("wave"=2),overlay=FALSE, gg=TRUE) + coord_cartesian(ylim=c(0, 90)) + ggtitle("PLED count")
multiplot(p1, p2, cols=1)

## Now see if we can do this in ggplot
all.long %>% ggplot(., aes(x=PDINUM, y=value, group=wave, color=wave)) +
  geom_point() +
  geom_smooth(method="lm") +
  facet_wrap(variable ~.)

## Plot the growth artifacts?
all.long  %>% ggplot(., aes(x=wave, y=value)) +
  geom_violin(mapping = aes(group=wave)) +
  geom_smooth(aes(group=participant), method="lm", color="grey", alpha=.05) +
  geom_point() +
  #geom_smooth(method = "lm", color="red", formula = y ~ x) +
  facet_wrap(variable ~ ., scales = "free")

all.long  %>% ggplot(., aes(x=wave, y=value,group=Group.y, color=Group.y)) +
  #geom_violin() +
  #geom_smooth(aes(group=participant), method="lm", color="grey", alpha=.05) +
  geom_jitter() +
  geom_smooth(method="lm") +
  #geom_smooth(method = "lm", color="red", formula = y ~ x) +
  facet_wrap(. ~ variable) +
  coord_cartesian(ylim=c(0, 75))

all.long %>% ggplot(., aes(x=wave, y=value, color=Group.y)) +
  geom_smooth(method="lm", color="grey", alpha=.05) +
  geom_point() +
  #geom_smooth(method = "lm", color="red", formula = y ~ x) +
  facet_grid(variable ~ Group.x, scales = "free")

## now combine and melt
all.long <- bind_rows(tmp.dat.1, tmp.dat.3)
#all.long <- reshape2::melt(all.long, id.vars=c("participant", "wave"))
## Lose neutral vars
all.long <- all.long[,-c(3,6,9)]
all.long$participant <- as.numeric(as.factor(all.long$participant))
include.part <- as.numeric(names(which(table(all.long$participant)==2)))
all.long <- all.long[all.long$participant %in% include.part,]
#fmLatent <- lmestFormula(data=all.long, response = names(all.long)[2:10])
fmLatent <- lmestFormula(data=all.long, response = names(all.long)[2:7])

## Now have a dataset available where we zscore the data within each wave
model <- lmestCont(index = c("participant","wave"),
               responsesFormula = fmLatent$responsesFormula,
               k = 2:8,
               data = all.long,
               modBasic = 1,
               start = 1,
               maxit = 5000,
               seed = 16,
               ntry=3)

model2 <- lmestCont(index = c("participant","wave"),
                   responsesFormula = fmLatent$responsesFormula,
                   k = 2,
                   data = all.long,
                   modBasic = 1,
                   start = 1,
                   maxit = 5000,
                   seed = 16,
                   ntry=10)
model3 <- lmestCont(index = c("participant","wave"),
                    responsesFormula = fmLatent$responsesFormula,
                    k = 3,
                    data = all.long,
                    modBasic = 1,
                    start = 1,
                    maxit = 5000,
                    seed = 16,
                    ntry=10)

model4 <- lmestCont(index = c("participant","wave"),
                   responsesFormula = fmLatent$responsesFormula,
                   k = 4,
                   data = all.long,
                   modBasic = 0,
                   start = 1,
                   maxit = 5000,
                   seed = 16,
                   ntry=10)

model5 <- lmestCont(index = c("participant","wave"),
                    responsesFormula = fmLatent$responsesFormula,
                    k = 5,
                    data = all.long,
                    modBasic = 1,
                    start = 1,
                    maxit = 5000,
                    seed = 16,
                    ntry=10)


tmp <- lmestSearch(responsesFormula = fmLatent$responsesFormula, seed=16,
            data=all.long, index = c("participant","wave"), k = 2:8, version="continous")

bestSel <- lmestCont(index = c("participant","wave"),
                               responsesFormula = fmLatent$responsesFormula,
                               k = 4,
                               data = all.long,
                               modBasic = 0,
                               start = 1,
                               maxit = 5000,
                               seed = 16,
                               ntry=10)

## Try the s4 methods here
library("depmixS4")
all.long <- all.long %>% group_by(participant, wave) %>%
  arrange(participant, wave)
ntimes.vec <- as.numeric(table(all.long$participant))
mod <- depmixS4::depmix(list(PRIDE_CLED ~ 1, Dont_CLED ~ 1, PRIDE_PLED ~ 1, Dont_PLED ~ 1, PRIDE_CLUP ~ 1, Dont_CLUP ~ 1),
                 data=all.long, nstates=4, family=list(gaussian(),gaussian(),gaussian(),gaussian(),gaussian(),gaussian()),
                 ntimes=ntimes.vec)
fm <- fit(mod, emc=em.control(rand=TRUE))
pars <- c(unlist(getpars(fm)))
pars[1] <- .25
pars[2] <- .25
pars[3] <- .25
pars[4] <- .25

fm1 <- setpars(mod, pars)
conpat <- c(0,0,0,0,rep(1, 64))
fm2 <- fit(fm1, equal=conpat)
fm <- depmixS4::multistart(mod)


# ## Now try latent growch model GMM here
# #install.packages("lcmm")
# library(lcmm)
all.long.2 <- melt(all.long, id.vars = c("participant", "wave"))

##first visualize growth within each variable
tmp.growth <- merge(tmp.dat.1, tmp.dat.3, by="participant", suffixes = c("_1", "_3"))
tmp.diff.vals <- tmp.growth[,c(12:20)] - tmp.growth[,c(2:10)]
tmp.diff.vals$participant <- tmp.growth$participant
## Now cluster the difference vals?
## Attach time 1 vals
clustChange <- Mclust(cbind(tmp.diff.vals[,1:9]), G = 4)
tmp.plot <- cbind(tmp.diff.vals[,1:9])
tmp.plot$Group <- factor(predict(clustChange)$classification)
GGally::ggpairs(tmp.plot, mapping = aes(group=Group, color=Group))

## Now melt and regress
tmp.diff.vals <- reshape2::melt(tmp.diff.vals, id.vars = c("participant"))
## Attach the demo vars
tmp.diff.vals <- merge(tmp.diff.vals, caps.data, by.x = "participant", by.y = "FAMILY")
## Now regress
tmp.mod <- lmerTest::lmer(value ~ Group * variable + (1|participant), data=tmp.diff.vals)
car::Anova(tmp.mod)
## Now see which variables this effect is found 
for(q in unique(tmp.diff.vals$variable)){
  tmp.mod <- lm(value ~ Group, data=tmp.diff.vals[which(tmp.diff.vals$variable==q),])
  if(summary(tmp.mod)$coefficients[2,4]<.05){
    print(paste(q))
  }
}
tmp.mod <- lmerTest::lmer(value ~ PDINUM * variable +(1|participant), data=tmp.diff.vals)
car::Anova(tmp.mod)
for(q in unique(tmp.diff.vals$variable)){
  tmp.mod <- lm(value ~ PDINUM, data=tmp.diff.vals[which(tmp.diff.vals$variable==q),])
  if(summary(tmp.mod)$coefficients[2,4]<.05){
    print(paste(q))
  }
}
tmp.mod <- lmerTest::lmer(value ~ CDINUM * variable + (1|participant), data=tmp.diff.vals)
car::Anova(tmp.mod)
for(q in unique(tmp.diff.vals$variable)){
  tmp.mod <- lm(value ~ CDINUM, data=tmp.diff.vals[which(tmp.diff.vals$variable==q),])
  if(summary(tmp.mod)$coefficients[2,4]<.05){
    print(paste(q))
  }
}
## Regress change of command counts onto the cognitive data
## But also cluster change counts?
library(lcmm)
all.long.2$participant <- as.numeric(factor(all.long.2$participant))
gmm1 <- hlme(value ~ variable * wave, subject="participant", random=~1, ng=1, data=all.long.2)
gmm2 <- gridsearch(rep = 100, maxiter = 10, minit = gmm1,
                   hlme(value ~ variable * wave, subject = "participant", random=~1,
                        ng = 2, data = all.long.2, mixture = ~ variable * wave,
                        nwg=T))
gmm3 <- gridsearch(rep = 100, maxiter = 10, minit = gmm1,
                   hlme(value ~ variable * wave, subject = "participant", random=~1,
                        ng = 3, data = all.long.2, mixture = ~ variable * wave,
                        nwg=T))
gmm4 <- gridsearch(rep = 100, maxiter = 10, minit = gmm1,
                   hlme(value ~ variable * wave, subject = "participant", random=~1,
                        ng = 4, data = all.long.2, mixture = ~ variable * wave,
                        nwg=T))
gmm5 <- gridsearch(rep = 100, maxiter = 10, minit = gmm1,
                   hlme(value ~ variable * wave, subject = "participant", random=~1,
                        ng = 5, data = all.long.2, mixture = ~ variable * wave,
                        nwg=T))
gmm6 <- gridsearch(rep = 100, maxiter = 10, minit = gmm1,
                   hlme(value ~ variable * wave, subject = "participant", random=~1,
                        ng = 6, data = all.long.2, mixture = ~ variable * wave,
                        nwg=T))
gmm7 <- gridsearch(rep = 100, maxiter = 10, minit = gmm1,
                   hlme(value ~ variable * wave, subject = "participant", random=~1,
                        ng = 7, data = all.long.2, mixture = ~ variable * wave,
                        nwg=T))
gmm8 <- gridsearch(rep = 100, maxiter = 10, minit = gmm1,
                   hlme(value ~ variable * wave, subject = "participant", random=~1,
                        ng = 8, data = all.long.2, mixture = ~ variable * wave,
                        nwg=T))
summarytable(gmm1, gmm2, gmm3, gmm4, gmm5, gmm6, gmm7, gmm8)
## Now plot these results
## Looks like the gmm5 is the best solution?
## Still getting really low % for some clusters
## table(gmm5$pprob[,2])
# 1   2   3   4   5 
# 13 109   9   3  19 
