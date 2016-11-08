#######################################################
## Comparing Methods of Assessing Migratory Behavior ##
########  NSERP - Kristin Barker - June 2016  #########
#######################################################

## WD
wd_workcomp <- "C:\\Users\\kristin.barker\\Documents\\GitHub\\MigStatus"
wd_laptop <- "C:\\Users\\kjbark3r\\Documents\\GitHub\\MigStatus"
if (file.exists(wd_workcomp)) {
  setwd(wd_workcomp)
} else {
  if(file.exists(wd_laptop)) {
    setwd(wd_laptop)
  } else {
      cat("Are you SURE you got that file path right?\n")
  }
}
rm(wd_workcomp, wd_laptop)

## PACKAGES
library(dplyr)

## "RAW" DATA ####
look <- read.csv("migstatus-prelimlook.csv") %>%
  select(c(AnimalID, Status, Notes)) %>%
  rename(Look = Status)
## USING FULL NSD ANALYSIS RATHER THAN AVG GEOG DISPLACEMENT
# nsd is from run 5 (see Migration QUickNotes in Thesis folder)
nsd <- read.csv("AICtable.csv") %>%
  select(c(animal, bestmodel)) %>%
  rename(AnimalID = animal) %>%
  mutate(NSD = ifelse(bestmodel == 1, "Migrant",
                      ifelse(bestmodel == 4, "Resident",
                             "Intermediate")))
mig <- read.csv("migration-analysis.csv") %>%
  full_join(nsd, by = "AnimalID") %>%
  select(-c(SprNSD, FallNSD, bestmodel)) %>%
  mutate(Year = ifelse(grepl("-14", IndivYr), 2014, 2015)) %>%
  left_join(look, by = "AnimalID") 

## BINNED ####
third.ao.s <- max(mig$SprAO, na.rm = T)/3
third.vi.s <- max(mig$SprVI, na.rm=T)/3
third.ao.f <- max(mig$FallAO, na.rm=T)/3
third.vi.f <- max(mig$FallVI, na.rm=T)/3

mig <- mig %>%
  transform(MigAO.S = ifelse(SprAO < third.ao.s, "Migrant", 
                           ifelse(SprAO > 2*third.ao.s, "Resident",
                                  "Intermediate"))) %>%
  transform(MigVI.S = ifelse(SprVI < third.vi.s, "Migrant", 
                           ifelse(SprVI > 2*third.vi.s, "Resident",
                                  "Intermediate"))) 
mig <- mig %>%
  transform(MigAO.F = ifelse(FallAO < third.ao.f, "Migrant", 
                           ifelse(SprAO > 2*third.ao.f, "Resident",
                                  "Intermediate"))) %>%
  transform(MigVI.F = ifelse(SprVI < third.vi.f, "Migrant", 
                           ifelse(SprVI > 2*third.vi.f, "Resident",
                                  "Intermediate"))) 


migbin <- select(mig, c(IndivYr, MigAO.S, MigAO.F, MigVI.S, MigVI.F, NSD, Look, Year)) %>%
  rename(AOspr = MigAO.S, AOfall = MigAO.F, VIspr = MigVI.S, VIfall = MigVI.F)

table(migbin$AOspr,migbin$VIspr, migbin$NSD) 
# tells how many were classified the same bt all 3 methods

## VISUALIZATIONS ####

hist(mig$SprAO)



## RANKED ####
mig <- mig %>% 
  transform(SprAORank = ave(SprAO, Year, FUN = function(x) rank(-x, ties.method = "average")),
            FallAORank = ave(FallAO, Year, FUN = function(x) rank(-x, ties.method = "average")),
            SprVIRank = ave(SprVI, Year, FUN = function(x) rank(-x, ties.method = "average")),
            FallVIRank = ave(FallVI, Year, FUN = function(x) rank(-x, ties.method = "average")),
            SprNSDRank = ave(SprNSD, Year, FUN = function(x) rank(x, ties.method = "average")),
            FallNSDRank = ave(FallNSD, Year, FUN = function(x) rank(x, ties.method = "average"))) 

## STANDARDIZED ####
mig$SprAOStd <- scale(mig$SprAO)
mig$FallAOStd <- scale(mig$FallAO)
mig$SprVIStd <- scale(mig$SprVI)
mig$FallVIStd <- scale(mig$FallVI)
mig$SprNSDStd <- scale(mig$SprNSD)
mig$FallNSDStd <- scale(mig$FallNSD)


## STATS ####



## VISUALIZATIONS ####

# just by rank - this has zero mathematical validity
par(mfrow=c(3,2))
plot(mig$SprAO ~ mig$SprAORank, col = mig$Look)
plot(mig$FallAO ~ mig$FallAORank, col = mig$Look)
plot(mig$SprVI ~ mig$SprVIRank, col = mig$Look)
plot(mig$FallVI ~ mig$FallVIRank, col = mig$Look)
plot(mig$SprNSD ~ mig$SprNSDRank, col = mig$Look)
plot(mig$FallNSD ~ mig$FallNSDRank, col = mig$Look)

# distributions

#spr
hist(-1*mig$SprAOStd)
hist(-1*mig$SprVIStd)
#hist(mig$SprAOStd)
#hist(mig$SprVIStd)
hist(mig$SprNSDStd)

#fall
hist(-1*mig$FallAOStd)
hist(-1*mig$FallVIStd)
hist(mig$FallNSDStd)

# boxplots

boxplot(-1*mig$SprAOStd, main = "-SprAO")
boxplot(-1*mig$SprVIStd, main = "-SprVI")
boxplot(mig$SprNSDStd, main = "SprNSD")

# plotting each against the other
# only looking at spring for now bc this is the mign my thesis focuses on
par(mfrow=c(3,1))
plot(SprAORank ~ SprVIRank, data=mig, col=mig$Look)
plot(SprAORank ~ SprNSDRank, data=mig, col=mig$Look)
plot(SprVIRank ~ SprNSDRank, data=mig, col=mig$Look)

# just added nsd total;; checking out what that does
nsd <- read.csv("nsd-avg-total.csv", header = TRUE) 
nsd <- nsd[rowSums(is.na(nsd)) !=2,] #remove nas
plot(SprNSD ~ SprTotalNSD, data=mig, col=Look)
plot(FallNSD ~ FallTotalNSD, data=mig, col=Look)
