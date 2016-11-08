#####################
### misc code related to migstatus
### kjb 2016
######################

#########
## NA animal IDs in joined data
## because nsd has more obs than ao/vi

setdiff(nsd$AnimalID, ao$AnimalID)
#[1] 140930 141420 151500
setdiff(nsd$IndivYr, ao$IndivYr)
#[1] "140930-14" "141420-14" "151500-14" "140930-15" "141420-15" "151500-15"

setdiff(ao$AnimalID, vi$AnimalID)
#samesies
setdiff(ao$IndivYr, vi$IndivYr)

#ok all difs are from the ones that are missing all values
#like if they died in 2014 they don't have 2015 indivyr overlaps
#or if the collar malfunctioned befoer we could get any data

#########
# removing NA rows prior to join
# playing with new drop_na fcn in tidyr

test <- drop_na(nsd, c(3,4))
#nope, does 3 or 4 not 3 and 4
test <- drop_na(nsd, 3:4)
#ditto
test <- drop_na(nsd, 3, 4) 
#ditto
#eff it

test <- nsd[rowSums(is.na(nsd)) !=2,]
test2 <- ao[rowSums(is.na(ao)) !=2,]
setdiff(test2$IndivYr, test$IndivYr)
#this diff makes sense; collar malfunctioned in the middle of summer
##which threw off the nsd calculation but not the overlap

rm(test, test2)

###################
#### reshaping data by rank and plotting altogether ####
# starting over with the standardizing and plotting
# bc something's screwy and i don't know wtf i did

setwd("C:\\Users\\kristin.barker\\Documents\\GitHub\\MigStatus")

# prelim look data
look <- read.csv("migstatus-prelimlook.csv") %>%
  subset(select = c("AnimalID", "Status", "Notes")) %>%
  rename(Look = Status)

# data from ao/vi/nsd analyses
mig <- read.csv("migration-analysis.csv")
  # right now, low ao/vi vals correspond to high nsd vals

# standardize (& make AO and VI in order of inc'ing mign strength)
mig$SprAOStd <- as.numeric(scale(-1*mig$SprAO))
mig$FallAOStd <- as.numeric(scale(-1*mig$FallAO))
mig$SprVIStd <- as.numeric(scale(-1*mig$SprVI))
mig$FallVIStd <- as.numeric(scale(-1*mig$FallVI))
mig$SprNSDStd <- as.numeric(scale(mig$SprNSD))
mig$FallNSDStd <- as.numeric(scale(mig$FallNSD))
  # now, high std ao/vi vals correspond to high nsd vals
  # which is what we want
  # migrants tend to have higher ranks; res have lower

par(mfrow=c(3,1))
plot(mig$SprAO ~ mig$SprAOStd, col = mig$Look)
plot(mig$SprVI ~ mig$SprVIStd, col = mig$Look)
plot(mig$SprNSD ~ mig$SprNSDStd, col = mig$Look)
  # this looks correct

# add rank (using standardized values)
mig <- mig %>%
  left_join(look, by = "AnimalID") %>%
  mutate(Year = ifelse(grepl("-14", mig$IndivYr), 2014, 2015)) %>%
  transform(SprAORank = ave(SprAOStd, Year, FUN = function(x) rank(x, ties.method = "average")),
            FallAORank = ave(FallAOStd, Year, FUN = function(x) rank(x, ties.method = "average")),
            SprVIRank = ave(SprVIStd, Year, FUN = function(x) rank(x, ties.method = "average")),
            FallVIRank = ave(FallVIStd, Year, FUN = function(x) rank(x, ties.method = "average")),
            SprNSDRank = ave(SprNSDStd, Year, FUN = function(x) rank(x, ties.method = "average")),
            FallNSDRank = ave(FallNSDStd, Year, FUN = function(x) rank(x, ties.method = "average"))) # %>%
#  gather(Anal, Rank, 12:17)
  
# plots - varn in mign
par(mfrow=c(1,1))
plot(mig$SprVIRank, mig$SprVI, col = mig$Year)
hist(mig$SprVI)
hist(mig$SprVIStd)
boxplot(mig$SprVI)
boxplot(scale(mig$SprVI))

# plot
plot(mig$SprAO ~ mig$SprAORank, col = mig$Look)
plot(mig$SprVI ~ mig$SprVIRank, col = mig$Look)
plot(mig$SprNSD ~ mig$SprNSDRank, col = mig$Look)

# identify weirdly classified points
par(mfrow=c(1,1))
plot(mig$SprNSD ~ mig$SprNSDRank, col = mig$Look)
identify(mig$SprNSD ~ mig$SprNSDRank, n = 5, plot = TRUE)
mig[5,1]; mig[82,1]; mig[13,1]; mig[39,1]; mig[88,1]

plot(mig$SprVI ~ mig$SprVIRank, col = mig$Look)
identify(mig$SprVI ~ mig$SprVIRank, n = 5, plot = TRUE)
mig[6,1]; mig[39,1]; mig[54,1]; mig[99,1]; mig[112,1]

plot(mig$SprAO ~ mig$SprAORank, col = mig$Look)
identify(mig$SprAO ~ mig$SprAORank, n = 5, plot = TRUE)
mig[6,1]; mig[19,1]; mig[5,1]; mig[13,1]; mig[39,1]

# pull notes for weirdos
mig[mig$IndivYr == "140120-14", 16]
mig[mig$IndivYr == "141340-15", 16]
mig[mig$IndivYr == "151380-15", 16]
mig[mig$IndivYr == "141130-14", 16]
mig[mig$IndivYr == "141490-14", 16]
mig[mig$IndivYr == "140400-14", 16]
mig[mig$IndivYr == "140100-14", 16]
mig[mig$IndivYr == "140890-15", 16]
mig[mig$IndivYr == "141010-15", 16]
mig[mig$IndivYr == "140680-14", 16]
mig[mig$IndivYr == "141130-14", 16]

# box plots
par(mfrow=c(3,1))
boxplot(mig$SprAOStd, main = "SprAO")
boxplot(mig$SprVIStd, main = "SprVI")
boxplot(mig$SprNSDStd, main = "SprNSD")

