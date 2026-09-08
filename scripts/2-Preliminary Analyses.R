#### Begin - Loading Dependencies, Set Working Directory ####
library(dplyr) ## useful for transforming data
library(stringr) ## needed to find patterns in character strings
library(lme4) ## glmm package
library(vegan) ## multidimentional vegetation analysis package
library(here) ## for setting the working directory and reproducibility
setwd(here()) ## set working directory to the main GitHub folder

#### Reading in Data ####
cover25 <- read.csv("./data/comm_2025.csv") ## field data on plant cover for 2025
cover26 <- read.csv("./data/comm_2026.csv") ## 2026
rich25 <- read.csv("./data/rich_2025.csv")
rich26 <- read.csv("./data/rich_2026.csv") ## field data on species richness
env <- read.csv("./data/ENV_Data.csv") ## environmental data from script 1-Clean Environmental Data.R
env <- env[complete.cases(env),] ## removing the 1 plot that doesn't exist 
sp.info <- read.csv("./data/IntroducedStatusKey.csv") ## species information on introduced/native, functional group, etc. 
# soils25 <- read.csv("./data/CarterFR_analysis.csv") ## will have to deal with soils later

#### cleaning data ####
## removing stone canyon from the 2025 data
cover25 <- cover25[-grep("_", cover25$plot),]
unique(rich25$plot)
rich25 <- rich25[-grep("_", rich25$plot),]
unique(rich25$plot)


## cleaning the codes
`%notin%` <- Negate(`%in%`)

## 2025
unique(cover25$code[cover25$code %notin% sp.info$code]) ## finding observations not in the sp.info df
cover25 <- cover25[cover25$code != "ROCK" & 
                     cover25$code != "LITTER" & 
                     cover25$code != "LITTER " &
                     cover25$code != "LITTER`" &
                     cover25$code != "SOIL",] ## removing non-species codes
## manually fixing spelling errors
cover25$code[cover25$code == "ANDGER "] <- "ANDGER"
cover25$code[cover25$code == "CAREX"] <- "CAREX SP."
cover25$code[cover25$code == "ESCVIR"] <- "ECHVIR" 
cover25$code[cover25$code == "UNK 04" | cover25$code == "UNK 05" | cover25$code == "UNK 14"] <- "UNABLETOID"
cover25$code[cover25$code == "BOUSTR"] <- "BOESTR" 
cover25$code[cover25$code == "LATALN"] <- "LATLAN" 
cover25$code[cover25$code == "POLDEL"] <- "POLDOU" 
cover25$code[cover25$code == "CAREX "] <- "CAREX SP."
cover25$code[cover25$code == "AQUCER"] <- "AQUCOE"
cover25$code[cover25$code == "MEHRAN"] <- "MAHREP"
cover25 <- cover25[cover25$code != "HIEALB",]  # no clue, removing for now (will look at data on campus)
cover25 <- cover25[cover25$code != "LONUTA",]  # no clue, removing
unique(cover25$code[cover25$code %notin% sp.info$code]) ## now the only missing code is the UNABLETOID code, which makes sense

## 2026
unique(cover26$code[cover26$code %notin% sp.info$code]) ## finding observations not in the sp.info df
sp_vec <- unique(cover26$code[cover26$code %notin% sp.info$code]) ## finding observations not in the sp.info df
sp_vec ## need to find which plots they are on, verify, and add to sp.info data
cover26 <- cover26[cover26$code != "LITT" & 
                     cover26$code != "SOIL" & 
                     cover26$code != "ROCK" &
                     cover26$code != "WOOD",] ## removing non-species codes
cover26$code[cover26$code == "CAREX"] <- "CAREX SP."
cover26$code[cover26$code == "CREX"] <- "CAREX SP."
cover26$code[cover26$code == "UNKG1" | cover26$code == "UNKF4"] <- "UNABLETOID"
cover26$code[cover26$code == "LACCER"] <- "LACSER"
cover26$code[cover26$code == "CLAPAR"] <- "CLAPER"
cover26$code[cover26$code == "BROTEX"] <- "BROTEC"
cover26$code[cover26$code == "CARMIC"] <- "CAMMIC"
cover26$code[cover26$code == "VIOADU"] <- "VIONUT" ## pretty sure Trevor mis-ID'ed the violet
cover26$code[cover26$code == "MEISTE"] <- "MAISTE"
cover26$code[cover26$code == "GARBOR"] <- "GALBOR"
cover26$code[cover26$code == "PINVIR"] <- "PENVIR"
cover26$code[cover26$code == "PENVIT"] <- "PENVIR"
cover26$code[cover26$code == "CERTHA"] <- "VERTHA"
cover26$code[cover26$code == "SYMALAB"] <- "SYMALB"
cover26$code[cover26$code == "COLRUB"] <- "COLPAR" ## Not sure if 100% correct, but fairly confident
cover26$code[cover26$code == "XOLPAR"] <- "COLPAR"
cover26$code[cover26$code == "HEVVIL"] <- "HETVIL"
cover26$code[cover26$code == "AQUCER"] <- "AQUCOE"
cover26$code[cover26$code == "MAHREPP"] <- "MAHREP"
cover26$code[cover26$code == "PENINT"] <- "PENVIR"
cover26 <- cover26[cover26$code != "JAMAME",]  # no clue, removing for now (will look at data on campus)
unique(cover26$code[cover26$code %notin% sp.info$code]) ## now the only missing code is the UNABLETOID code, which makes sense

length(unique(cover25$code)) ## 92 species in 2025 cover data
length(unique(cover26$code)) ## 95 species in 2026 cover data

## repeating the process for the species richness df
unique(rich25$code[rich25$code %notin% sp.info$code]) ## mostly unknowns as errors
rich25$code[rich25$code == "UNK10" | 
              rich25$code == "UNK11" | 
              rich25$code == "UNK 11" | 
              rich25$code == "UNK 09" | 
              rich25$code == "UNK 04" | 
              rich25$code == "UNK 02" | 
              rich25$code == "UNK6" | 
              rich25$code == "UNK13" | 
              rich25$code == "UNK12" | 
              rich25$code == "UNK14" | 
              rich25$code == "UNK15" | 
              rich25$code == "UNK A"] <- "UNABLETOID"
rich25$code[rich25$code == "CAREX sp."] <- "CAREX SP."
rich25$code[rich25$code == "HUEPAR"] <- "HEUPAR"
rich25$code[rich25$code == "ARTDRA "] <- "ARTDRA"
rich25$code[rich25$code == "ESCVIR"] <- "ECHVIR"
rich25$code[rich25$code == "CIRSIUM SP."] ## not doing anything with the cirsium genus at the moment
rich25$code[rich25$code == "RHUARO"] <- "RHUTRI"
rich25$code[rich25$code == "OPUPOL "] <- "OPUPOL"
rich25$code[rich25$code == "BOUSTR"] <- "BOESTR"
rich25$code[rich25$code == "HETHIR"] <- "HETVIL"
rich25 <- rich25[rich25$code != "GILCUD",] ## no clue, removing for now
unique(rich25$code[rich25$code %notin% sp.info$code])

unique(rich26$code[rich26$code %notin% sp.info$code]) ## mostly unknowns as errors
rich26$code[rich26$code == "UNKF1" | 
              rich26$code == "UNKF2" | 
              rich26$code == "UNKF3" | 
              rich26$code == "UNKF5" | 
              rich26$code == "UNKG1" | 
              rich26$code == "ANTENNARIA" | 
              rich26$code == "UNKF6"] <- "UNABLETOID"
rich26$code[rich26$code == "CAREX"] <- "CAREX SP."
rich26$code[rich26$code == "LACCER"] <- "LACSER"
rich26$code[rich26$code == "CLAPAR"] <- "CLAPER"
rich26$code[rich26$code == "CARMIC"] <- "CAMMIC"
rich26$code[rich26$code == "VIOADU"] <- "VIONUT" ## pretty sure Trevor mis-ID'ed the violet
rich26$code[rich26$code == "VIOAUD"] <- "VIONUT" ## pretty sure Trevor mis-ID'ed the violet
rich26$code[rich26$code == "AQUCER"] <- "AQUCOE"
rich26$code[rich26$code == "COLRUB"] <- "COLPAR" ## Not sure if 100% correct, but fairly confident
rich26$code[rich26$code == "DPENVIR"] <- "PENVIR"
rich26$code[rich26$code == "VETHA"] <- "VERTHA"
rich26$code[rich26$code == "PENINT"] <- "PENVIR"
rich26$code[rich26$code == "RIBMON"] <- "RIBCER"
rich26$code[rich26$code == "DRAAUG"] <- "DRAAUR"
rich26$code[rich26$code == "IPOAGR"] <- "IPOAGG"
rich26$code[rich26$code == "HETCIL"] <- "HETVIL"
rich26$code[rich26$code == "PSEMIC"] <- "PSEMAC"
rich26$code[rich26$code == "BOUSTR"] <- "BOESTR"
rich26$code[rich26$code == "GRIALP"] <- "GRISUB"
rich26$code[rich26$code == "RHEHYB"] <- "CHEHYB"
rich26$code[rich26$code == "OENSUG"] <- "OENSUF"
rich26$code[rich26$code == "SYMOFF"] <- "CYNOFF"
rich26$code[rich26$code == "AIRARV"] <- "CIRARV"
rich26$code[rich26$code == "DRISUB"] <- "GRISUB"
rich26$code[rich26$code == "ERIGLA"] <- "ERIFLA"
rich26$code[rich26$code == "PENSTR"] <- "PENVIR"
rich26$code[rich26$code == "SILACA"] <- "SILANT"
rich26$code[rich26$code == "PENSTE"] <- "PENSEC"
rich26 <- rich26[rich26$code != "JAMAME",]  # no clue, removing for now (will look at data on campus)
rich26 <- rich26[rich26$code != "PLAPUR",]  # no clue, removing for now (will look at data on campus)
rich26 <- rich26[rich26$code != "CALCAN",]  # no clue, removing for now (will look at data on campus)
rich26 <- rich26[rich26$code != "DACGLO",]  # no clue, removing for now (will look at data on campus)
rich26 <- rich26[rich26$code != "PHASER",]  # no clue, removing for now (will look at data on campus)
rich26 <- rich26[rich26$code != "POLPUL",]  # no clue, removing for now (will look at data on campus)
unique(rich26$code[rich26$code %notin% sp.info$code]) ## mostly unknowns as errors


#### Making a list for future generalizability ####
rich.list <- list()
rich.list[[1]] <- rich25
rich.list[[2]] <- rich26
# rich.list[[3]] <- rich27
# rich.list[[4]] <- rich28
# rich.list[[5]] <- rich29
rm(rich25);rm(rich26)

cover.list <- list()
cover.list[[1]] <- cover25
cover.list[[2]] <- cover26
# cover.list[[3]] <- cover27
# cover.list[[4]] <- cover28
# cover.list[[5]] <- cover29
rm(cover25);rm(cover26)


#### Average Species Cover per Plot ####
nyears <- 2

for(i in 1:nyears){
  print(length(unique(cover.list[[i]]$plot)))
  print(length(unique(rich.list[[i]]$plot)))
} ## no plots missing

trans.avg <- function(x){
  transavg <- (sum(x))/3
  return(transavg)
} ## Custom function to take the average on each transect. 
## Sum is divided by 3 because there were 3 subplots and 0 values were not recorded.
plot.avg <- function(x){
  plotavg <- (sum(x))/3
  return(plotavg)
} ## Custom function to take the average on each plot
## Sum is divided by 3 because there were 3 transects and 0 values were not recorded.

cover.sum.list <- list()
for(i in 1:nyears){
  cover.sum.list[[i]] <- cover.list[[i]] %>% 
    group_by(plot, transect, code) %>% 
    summarise(transavg = trans.avg(cov)) %>%
    group_by(plot, code) %>%
    summarise(plotavg = plot.avg(transavg)) ## summarizing by transect then by plot
  cover.sum.list[[i]] <- as.data.frame(cover.sum.list[[i]])
  print(length(unique(cover.sum.list[[i]]$plot)))
  print(length(unique(cover.sum.list[[i]]$code)))
}

#### Creating Community Data (Cover Estimates + Rare Species only found as richness) ####
for(i in 1:nyears){
  rich.list[[i]] <- rbind(rich.list[[i]][,c(1,2)], cover.list[[i]][,c(1,4)])
  rich.list[[i]] <- rich.list[[i]][order(rich.list[[i]]$plot, rich.list[[i]]$code),] ## ordering the dataframe
  rich.list[[i]] <- rich.list[[i]][!duplicated(rich.list[[i]]),] ## removing duplicates (same species and same plot, but not same species different plot)
  rich.list[[i]]$plotavg <- 0
  print(length(unique(rich.list[[i]]$code)))
}

long <- list()
for(i in 1:nyears){
  long[[i]] <- rbind(cover.sum.list[[i]],rich.list[[i]]) ## creating a community dataframe that has both cover and richness
  long[[i]] <- as.data.frame(long[[i]][order(long[[i]]$plot, long[[i]]$code),]) ## ordering the community data
  large.temp <- data.frame()
  vec <- unique(long[[i]]$plot)
  for(j in 1:length(vec)){
    tmp <- long[[i]][long[[i]]$plot == vec[j],]
    tmp <- tmp[!duplicated(tmp$code),]
    large.temp <- rbind(large.temp,tmp)
    print(j/length(vec))
  } ## removing the richness (with cover of 0) for species that were present in the cover estimates
  long[[i]] <- large.temp
  long[[i]] <- long[[i]][order(long[[i]]$plot, long[[i]]$code,long[[i]]$plotavg),] ## ordering the data
}

wide <- list()
for(i in 1:nyears){
  wide[[i]] <- reshape(long[[i]], idvar = "plot", timevar = "code", direction = "wide") ## reshaping the data
  rownames(wide[[i]]) <- wide[[i]]$plot ## renaming rows to be PlotID
  wide[[i]]$plot <- NULL ## removing non species column
  colnames(wide[[i]]) <- sub("plotavg.", "", colnames(wide[[i]])) ## changing column names to be species codes
}

summary.df <- data.frame(plot = env$plot[order(env$plot)])
sp.freq.df <- data.frame(species = unique(c(long[[1]]$code, long[[2]]$code)),
                         yr2025 = NA,
                         yr2026 = NA,
                         yr2027 = NA,
                         yr2028 = NA,
                         yr2029 = NA)


vec <- c(2025,2026,2027,2028,2029)
for(i in 1:nyears){
  tmp <- as.data.frame(apply(wide[[i]], 1, sum, na.rm = TRUE)) ## getting the total cover per plot
  sp.freq <- ifelse(wide[[i]][,] >= 0,1,0)
  tmp$rich <- apply(sp.freq, 1, sum, na.rm = TRUE) ## recalculating a richness per plot
  colnames(tmp) <- c(paste0("tot.cov.", vec[i]),paste0("rich.", vec[i]))
  summary.df <- cbind(summary.df,tmp)
  
  sp.freq <- as.data.frame(apply(sp.freq, 2, sum, na.rm = TRUE)) ## getting the number of times a species occurred 
  colnames(sp.freq) <- "sp.freq" ## renaming to be be easy to identify
  sp.freq.df[match(rownames(sp.freq),sp.freq.df$species),(i+1)] <- sp.freq
}

comm.list <- c(long, wide)
rm(list = setdiff(ls(), c("comm.list", "summary.df", "sp.freq.df","env", "sp.info")))


#### Vegetation Changes  ####
## is total cover different between years
## is total richness different between years
## is the cover of non-native annuals different
## if changes are occurring, where are they happening?

t.test(summary.df$tot.cov.2025,summary.df$tot.cov.2026, paired = T) ## difference in cover
t.test(summary.df$rich.2025,summary.df$rich.2026, paired = T) ## no difference

summary.df$trt <- env$trt
summary.df$sev <- env$sev
t.test(summary.df$tot.cov.2025[summary.df$trt == "fr"],summary.df$tot.cov.2026[summary.df$trt == "fr"], paired = T) ## no difference
t.test(summary.df$tot.cov.2025[summary.df$trt == "con"],summary.df$tot.cov.2026[summary.df$trt == "con"], paired = T) ## difference in cover
## decline appears to be in control plots

t.test(summary.df$rich.2025[summary.df$trt == "fr"],summary.df$rich.2026[summary.df$trt == "fr"], paired = T) ## no difference
t.test(summary.df$rich.2025[summary.df$trt == "con"],summary.df$rich.2026[summary.df$trt == "con"], paired = T) ## difference in cover
## no difference

str(summary.df)
summary.df$trt <- factor(summary.df$trt, levels = c("con", "fr"))
summary.df$sev <- factor(summary.df$sev, levels = c("unburn", "low", "mod", "high"))
summary.df$diff.cov <- summary.df$tot.cov.2025 - summary.df$tot.cov.2026
summary(lm(diff.cov ~ trt*sev, data = summary.df))
summary(lm(diff.cov ~ trt+sev, data = summary.df))
## no real effect of trt or sev on difference in cover

plot(diff.cov ~ trt, data = summary.df)
plot(diff.cov ~ sev, data = summary.df)

sp.freq.df$duration <- sp.info$duration[match(sp.freq.df$species, sp.info$code)]
sp.freq.df$functional.group <- sp.info$functional.group[match(sp.freq.df$species, sp.info$code)]
sp.freq.df$status <- sp.info$status[match(sp.freq.df$species, sp.info$code)]

sp.freq.df$yr2025[is.na(sp.freq.df$yr2025)] <- 0
sp.freq.df$yr2026[is.na(sp.freq.df$yr2026)] <- 0
sp.freq.df <- sp.freq.df[complete.cases(sp.freq.df$duration),]
sp.freq.df$diff <- sp.freq.df$yr2025 - sp.freq.df$yr2026

str(sp.freq.df)
sp.freq.df$duration <- factor(sp.freq.df$duration, levels = c("annual", "perennial"))
sp.freq.df$functional.group <- factor(sp.freq.df$functional.group, levels = c("forb", "gram", "shrub", "tree"))
sp.freq.df$status <- factor(sp.freq.df$status, levels = c("N", "I"))

summary(lm(diff ~ duration, data = sp.freq.df))
plot(diff ~ duration, data = sp.freq.df) ## a slight increase in annuals

summary(lm(diff ~ functional.group, data = sp.freq.df))
plot(diff ~ functional.group, data = sp.freq.df) ## no landscape change among fg

summary(lm(diff ~ status, data = sp.freq.df))
plot(diff ~ status, data = sp.freq.df) ## no landscape change among native vs non-native

annuals <- sp.freq.df[sp.freq.df$duration == "annual",]
summary(lm(diff ~ status, data = annuals))
plot(diff ~ status, data = annuals) ## no landscape change among fg




## part 1.1 - richness between control and trt
PlotRichness$sev <- env$sev[match(rownames(PlotRichness),env$plot)]
PlotRichness$trt <- env$trt[match(rownames(PlotRichness),env$plot)]
PlotRichness$site <- env$site[match(rownames(PlotRichness),env$plot)]

str(PlotRichness)
t.test(PlotRichness$richness~ PlotRichness$site, na.rm = TRUE)
## difference between the two sites (quarry has fewer species on average)

## part 1.2  - cover between control and trt
PlotCov$sev <- env$sev[match(rownames(PlotCov),env$plot)]
PlotCov$trt <- env$trt[match(rownames(PlotCov),env$plot)]
PlotCov$site <- env$site[match(rownames(PlotCov),env$plot)]

str(PlotCov)
t.test(PlotCov$TotalPlotCover~ PlotCov$site, na.rm = TRUE)
## no difference in plot cover between sites

## part 2 - native and invasive cover
## separate out native vs. invasive species for comparison across burn severities (basically do part 1 but 4 times)
## plot level proportion richness Invasive /proportion cover invasive
## functional group and life history can be added if interested

status <- comm.sum
colnames(status)
match(colnames(status), sp.info$code)

native <- status[,colnames(status) %in% sp.info$code[sp.info$status == "N"]]
invasive <- status[,colnames(status) %in% sp.info$code[sp.info$status == "I"]]
annual <- status[,colnames(status) %in% sp.info$code[sp.info$duration == "annual"]]
perennial <- status[,colnames(status) %in% sp.info$code[sp.info$duration == "perennial"]]

env$X <- NULL

native.cov <- data.frame(native.cov = apply(native, 1 , sum),
                         plot = rownames(native))
invasive.cov <- data.frame(invasive.cov = apply(invasive,1,sum),
                           plot = rownames(invasive))
annual.cov <- data.frame(annual.cov = apply(annual, 1 , sum),
                         plot = rownames(annual))
perennial.cov <- data.frame(perennial.cov = apply(perennial, 1 , sum),
                         plot = rownames(perennial))
env$native.cov <- native.cov$native.cov[match(native.cov$plot, env$plot)]
env$invasive.cov <- invasive.cov$invasive.cov[match(invasive.cov$plot, env$plot)]
env$annual.cov <- annual.cov$annual.cov[match(annual.cov$plot, env$plot)]
env$perennial.cov <- perennial.cov$perennial.cov[match(perennial.cov$plot, env$plot)]

env$prop.inv.cov <- env$invasive.cov/(env$native.cov+env$invasive.cov)
env$prop.ann.cov <- env$annual.cov/(env$perennial.cov+env$annual.cov)
hist(env$prop.inv.cov)
hist(env$prop.ann.cov)

grad <- native.cov
grad$invasive.cov <- invasive.cov$invasive.cov[match(grad$plot, invasive.cov$plot)]
grad$annual.cov <- annual.cov$annual.cov[match(grad$plot, annual.cov$plot)]
grad$plot.cov <- PlotCov$TotalPlotCover[match(grad$plot, rownames(PlotCov))]
grad$plot.rich <- PlotRichness$richness[match(grad$plot, rownames(PlotRichness))]
grad$NO3 <- soils$NO3[match(grad$plot, soils$Field.ID)]
grad$NH4 <- soils$NH4[match(grad$plot, soils$Field.ID)]
grad$PO4 <- soils$PO4[match(grad$plot, soils$Field.ID)]
grad$site <- soils$Location[match(grad$plot, soils$Field.ID)]
grad$prop.inv <- grad$invasive.cov/grad$plot.cov
grad$prop.ann <- grad$annual.cov/grad$plot.cov
grad$trt <- PlotCov$trt[match(grad$plot, rownames(PlotCov))]
grad$sev <- PlotCov$sev[match(grad$plot, rownames(PlotCov))]

rm(native.cov);rm(invasive.cov)

native <- ifelse(native > 0, 1,0)
invasive <- ifelse(invasive > 0,1,0)
annual <- ifelse(annual > 0, 1,0)
perennial <- ifelse(perennial > 0,1,0)

env$native.sp <- apply(native, 1 , sum)
env$invasive.sp <- apply(invasive, 1, sum)
env$annual.sp <- apply(annual, 1 , sum)
env$perennial.sp <- apply(perennial, 1, sum)

env$prop.inv.sp <- env$invasive.sp/(env$native.sp+env$invasive.sp)
grad$prop.inv.sp <- env$prop.inv.sp[match(grad$plot, env$plot)]
env$prop.ann.sp <- env$annual.sp/(env$perennial.sp+env$annual.sp)
grad$prop.ann.sp <- env$prop.ann.sp[match(grad$plot, env$plot)]
hist(env$prop.inv.sp)
hist(env$prop.ann.sp)

hist(env$prop.inv.sp[env$trt == "con"])
hist(env$prop.inv.sp[env$trt == "fr"])

hist(env$prop.ann.sp[env$trt == "con"])
hist(env$prop.ann.sp[env$trt == "fr"])

#### Linear Models to address Q2 ####
grad$trt <- factor(grad$trt, levels = c("con", "fr"))
grad$sev <- factor(grad$sev, levels = c("unburn", "low", "mod", "high"))
str(grad)
pseudo.R.squared <- function(model){
  1 - (model$deviance/model$null.deviance)
}
q.grad <- grad[grad$site == "Quarry",]
sc.grad <- grad[grad$site == "Stone Canyon",]

## Quarry models
## response ~ soils + trt + severity
summary(lm(plot.cov ~ NO3+NH4+PO4+trt+sev,data = q.grad)) ## keeping in the trt and sev to control for it
summary(lm(prop.inv ~ NO3+NH4+PO4+trt+sev,data = q.grad))
summary(lm(prop.ann ~ NO3+NH4+PO4+trt+sev,data = q.grad))
summary(glm(plot.rich ~ NO3+NH4+PO4+trt+sev,data = q.grad, family = poisson(link = "log")))
pseudo.R.squared(glm(plot.rich ~ NO3+NH4+PO4+trt+sev,data = q.grad, family = poisson(link = "log")))
summary(lm(prop.inv.sp ~ NO3+NH4+PO4+trt+sev,data = q.grad))
summary(lm(prop.ann.sp ~ NO3+NH4+PO4+trt+sev,data = q.grad))


## Stone canyon
## response ~ soils + trt + severity
summary(lm(plot.cov ~ NO3+NH4+PO4+trt+sev,data = sc.grad)) ## keeping in the trt and sev to control for it
summary(lm(prop.inv ~ NO3+NH4+PO4+trt+sev,data = sc.grad))
summary(lm(prop.ann ~ NO3+NH4+PO4+trt+sev,data = sc.grad))
summary(glm(plot.rich ~ NO3+NH4+PO4+trt+sev,data = sc.grad, family = poisson(link = "log")))
pseudo.R.squared(glm(plot.rich ~ NO3+NH4+PO4+trt+sev,data = sc.grad, family = poisson(link = "log")))
summary(lm(prop.inv.sp ~ NO3+NH4+PO4+trt+sev,data = sc.grad))
summary(lm(prop.ann.sp ~ NO3+NH4+PO4+trt+sev,data = sc.grad))


#### Figure 4 - Richness ####
se <- function(x){sd(x)/sqrt(length(x))} ## creating a function for standard error
sc.grad$plotting <- ifelse(sc.grad$sev == "unburn" & sc.grad$tr == "con", "UCON",
                           ifelse(sc.grad$sev == "unburn" & sc.grad$tr == "fr", "UFR",
                                  ifelse(sc.grad$sev == "low" & sc.grad$tr == "con", "LCON",
                                         ifelse(sc.grad$sev == "low" & sc.grad$tr == "fr", "LFR", 
                                                ifelse(sc.grad$sev == "mod" & sc.grad$tr == "con", "MCON",
                                                       ifelse(sc.grad$sev == "mod" & sc.grad$tr == "fr", "MFR",
                                                              ifelse(sc.grad$sev == "high" & sc.grad$tr == "con", "HCON",
                                                                     ifelse(sc.grad$sev == "high" & sc.grad$tr == "fr", "HFR", NA))))))))
q.grad$plotting <- ifelse(q.grad$sev == "unburn" & q.grad$tr == "con", "UCON",
                           ifelse(q.grad$sev == "unburn" & q.grad$tr == "fr", "UFR",
                                  ifelse(q.grad$sev == "low" & q.grad$tr == "con", "LCON",
                                         ifelse(q.grad$sev == "low" & q.grad$tr == "fr", "LFR", 
                                                ifelse(q.grad$sev == "mod" & q.grad$tr == "con", "MCON",
                                                       ifelse(q.grad$sev == "mod" & q.grad$tr == "fr", "MFR",
                                                              ifelse(q.grad$sev == "high" & q.grad$tr == "con", "HCON",
                                                                     ifelse(q.grad$sev == "high" & q.grad$tr == "fr", "HFR", NA))))))))

## setting figure parameters 
Fig2order.sc <- c("UCON","LCON")
vec1.sc <- c(0.9,1.9)
Fig2paired.sc <- c("UFR","LFR")
vec2.sc <- c(1.1,2.1)

Fig2order.q <- c("UCON","LCON","MCON","HCON")
vec1.q <- c(0.9,1.9,2.9,3.9)
Fig2paired.q <- c("UFR","LFR","MFR","HFR")
vec2.q <- c(1.1,2.1,3.1,4.1)

SC <- sc.grad
Quarry <- q.grad

par(mfrow = c(3,2))

## change the nut variable (borrowed from soils code) based on the column name in grad df
nut <- 'plot.rich'
plot(x = c(0.5:4.5),
     y = c(0.5:4.5),
     ylim = c(0,max(grad[,nut])),
     las = 1,
     cex.axis = 1.5,
     ylab = "", ## density
     type = "n",
     xaxt = "n",
     xlab = "") ## disturbance history
axis(1, at = c(1:4), line = 1, tick = F, labels = c("Unburned", "Low", "Moderate", "High"), cex.axis = 1.5)
for(i in 1:length(Fig2order.q)){
  points(x = rep(vec1.q[i], length(Quarry[,nut][Quarry$plotting == Fig2order.q[i]])),
         y = Quarry[,nut][Quarry$plotting == Fig2order.q[i]],
         col = rgb(0,0,0, alpha = 0.5),
         pch = 19)
  points(x = rep(vec2.q[i], length(Quarry[,nut][Quarry$plotting == Fig2paired.q[i]])),
         y = Quarry[,nut][Quarry$plotting == Fig2paired.q[i]],
         col = rgb(1,0,0, alpha = 0.5),
         pch = 19)
  
  points(x = vec1.q[i]+0.1,
         y = mean(Quarry[,nut][Quarry$plotting == Fig2order.q[i]]),
         col = rgb(0,0,0, alpha = 0.5),
         pch = 17)
  segments(y0 = (mean(Quarry[,nut][Quarry$plotting == Fig2order.q[i]])-se(Quarry[,nut][Quarry$plotting == Fig2order.q[i]])), x0 = vec1.q[i]+0.1, 
           y1 = (mean(Quarry[,nut][Quarry$plotting == Fig2order.q[i]])+se(Quarry[,nut][Quarry$plotting == Fig2order.q[i]])), x1 = vec1.q[i]+0.1, 
           col = rgb(0,0,0),lwd = 1.5)
  
  points(x = vec2.q[i]+0.1,
         y = mean(Quarry[,nut][Quarry$plotting == Fig2paired.q[i]]),
         col = rgb(1,0,0, alpha = 0.5),
         pch = 17)
  segments(y0 = (mean(Quarry[,nut][Quarry$plotting == Fig2paired.q[i]])-se(Quarry[,nut][Quarry$plotting == Fig2paired.q[i]])), x0 = vec2.q[i]+0.1, 
           y1 = (mean(Quarry[,nut][Quarry$plotting == Fig2paired.q[i]])+se(Quarry[,nut][Quarry$plotting == Fig2paired.q[i]])), x1 = vec2.q[i]+0.1, 
           col = rgb(1,0,0),lwd = 1.5)
  
}

plot(x = c(0.5:2.5),
     y = c(0.5:2.5),
     ylim = c(0,max(grad[,nut])),
     las = 1,
     cex.axis = 1.5,
     ylab = "", ## density
     type = "n",
     xaxt = "n",
     xlab = "") ## disturbance history
axis(1, at = c(1:2), line = 1, tick = F, labels = c("Unburned", "Low"), cex.axis = 1.5)
for(i in 1:length(Fig2order.sc)){
  points(x = rep(vec1.sc[i], length(SC[,nut][SC$plotting == Fig2order.sc[i]])),
         y = SC[,nut][SC$plotting == Fig2order.sc[i]],
         col = rgb(0,0,0, alpha = 0.5),
         pch = 19)
  points(x = rep(vec2.sc[i], length(SC[,nut][SC$plotting == Fig2paired.sc[i]])),
         y = SC[,nut][SC$plotting == Fig2paired.sc[i]],
         col = rgb(1,0,0, alpha = 0.5),
         pch = 19)
  
  points(x = vec1.sc[i]+0.1,
         y = mean(SC[,nut][SC$plotting == Fig2order.sc[i]]),
         col = rgb(0,0,0, alpha = 0.5),
         pch = 17)
  segments(y0 = (mean(SC[,nut][SC$plotting == Fig2order.sc[i]])-se(SC[,nut][SC$plotting == Fig2order.sc[i]])), x0 = vec1.sc[i]+0.1, 
           y1 = (mean(SC[,nut][SC$plotting == Fig2order.sc[i]])+se(SC[,nut][SC$plotting == Fig2order.sc[i]])), x1 = vec1.sc[i]+0.1, 
           col = rgb(0,0,0),lwd = 1.5)
  
  points(x = vec2.sc[i]+0.1,
         y = mean(SC[,nut][SC$plotting == Fig2paired.sc[i]]),
         col = rgb(1,0,0, alpha = 0.5),
         pch = 17)
  segments(y0 = (mean(SC[,nut][SC$plotting == Fig2paired.sc[i]])-se(SC[,nut][SC$plotting == Fig2paired.sc[i]])), x0 = vec2.sc[i]+0.1, 
           y1 = (mean(SC[,nut][SC$plotting == Fig2paired.sc[i]])+se(SC[,nut][SC$plotting == Fig2paired.sc[i]])), x1 = vec2.sc[i]+0.1, 
           col = rgb(1,0,0),lwd = 1.5)
  
}

## change the nutrients of interest based on the column name in soils df
nut <- 'prop.inv.sp'
plot(x = c(0.5:4.5),
     y = c(0.5:4.5),
     ylim = c(0,1),
     las = 1,
     cex.axis = 1.5,
     ylab = "", ## density
     type = "n",
     xaxt = "n",
     xlab = "") ## disturbance history
abline(h = 0.5, lty = 2)
axis(1, at = c(1:4), line = 1, tick = F, labels = c("Unburned", "Low", "Moderate", "High"), cex.axis = 1.5)
for(i in 1:length(Fig2order.q)){
  points(x = rep(vec1.q[i], length(Quarry[,nut][Quarry$plotting == Fig2order.q[i]])),
         y = Quarry[,nut][Quarry$plotting == Fig2order.q[i]],
         col = rgb(0,0,0, alpha = 0.5),
         pch = 19)
  points(x = rep(vec2.q[i], length(Quarry[,nut][Quarry$plotting == Fig2paired.q[i]])),
         y = Quarry[,nut][Quarry$plotting == Fig2paired.q[i]],
         col = rgb(1,0,0, alpha = 0.5),
         pch = 19)
  
  points(x = vec1.q[i]+0.1,
         y = mean(Quarry[,nut][Quarry$plotting == Fig2order.q[i]]),
         col = rgb(0,0,0, alpha = 0.5),
         pch = 17)
  segments(y0 = (mean(Quarry[,nut][Quarry$plotting == Fig2order.q[i]])-se(Quarry[,nut][Quarry$plotting == Fig2order.q[i]])), x0 = vec1.q[i]+0.1, 
           y1 = (mean(Quarry[,nut][Quarry$plotting == Fig2order.q[i]])+se(Quarry[,nut][Quarry$plotting == Fig2order.q[i]])), x1 = vec1.q[i]+0.1, 
           col = rgb(0,0,0),lwd = 1.5)
  
  points(x = vec2.q[i]+0.1,
         y = mean(Quarry[,nut][Quarry$plotting == Fig2paired.q[i]]),
         col = rgb(1,0,0, alpha = 0.5),
         pch = 17)
  segments(y0 = (mean(Quarry[,nut][Quarry$plotting == Fig2paired.q[i]])-se(Quarry[,nut][Quarry$plotting == Fig2paired.q[i]])), x0 = vec2.q[i]+0.1, 
           y1 = (mean(Quarry[,nut][Quarry$plotting == Fig2paired.q[i]])+se(Quarry[,nut][Quarry$plotting == Fig2paired.q[i]])), x1 = vec2.q[i]+0.1, 
           col = rgb(1,0,0),lwd = 1.5)
  
}

plot(x = c(0.5:2.5),
     y = c(0.5:2.5),
     ylim = c(0,1),
     las = 1,
     cex.axis = 1.5,
     ylab = "", ## density
     type = "n",
     xaxt = "n",
     xlab = "") ## disturbance history
axis(1, at = c(1:2), line = 1, tick = F, labels = c("Unburned", "Low"), cex.axis = 1.5)
abline(h = 0.5, lty = 2)
for(i in 1:length(Fig2order.sc)){
  points(x = rep(vec1.sc[i], length(SC[,nut][SC$plotting == Fig2order.sc[i]])),
         y = SC[,nut][SC$plotting == Fig2order.sc[i]],
         col = rgb(0,0,0, alpha = 0.5),
         pch = 19)
  points(x = rep(vec2.sc[i], length(SC[,nut][SC$plotting == Fig2paired.sc[i]])),
         y = SC[,nut][SC$plotting == Fig2paired.sc[i]],
         col = rgb(1,0,0, alpha = 0.5),
         pch = 19)
  
  points(x = vec1.sc[i]+0.1,
         y = mean(SC[,nut][SC$plotting == Fig2order.sc[i]]),
         col = rgb(0,0,0, alpha = 0.5),
         pch = 17)
  segments(y0 = (mean(SC[,nut][SC$plotting == Fig2order.sc[i]])-se(SC[,nut][SC$plotting == Fig2order.sc[i]])), x0 = vec1.sc[i]+0.1, 
           y1 = (mean(SC[,nut][SC$plotting == Fig2order.sc[i]])+se(SC[,nut][SC$plotting == Fig2order.sc[i]])), x1 = vec1.sc[i]+0.1, 
           col = rgb(0,0,0),lwd = 1.5)
  
  points(x = vec2.sc[i]+0.1,
         y = mean(SC[,nut][SC$plotting == Fig2paired.sc[i]]),
         col = rgb(1,0,0, alpha = 0.5),
         pch = 17)
  segments(y0 = (mean(SC[,nut][SC$plotting == Fig2paired.sc[i]])-se(SC[,nut][SC$plotting == Fig2paired.sc[i]])), x0 = vec2.sc[i]+0.1, 
           y1 = (mean(SC[,nut][SC$plotting == Fig2paired.sc[i]])+se(SC[,nut][SC$plotting == Fig2paired.sc[i]])), x1 = vec2.sc[i]+0.1, 
           col = rgb(1,0,0),lwd = 1.5)
  
}

## change the nutrients of interest based on the column name in soils df
nut <- 'prop.ann.sp'
plot(x = c(0.5:4.5),
     y = c(0.5:4.5),
     ylim = c(0,1),
     las = 1,
     cex.axis = 1.5,
     ylab = "", ## density
     type = "n",
     xaxt = "n",
     xlab = "") ## disturbance history
abline(h = 0.5, lty = 2)
axis(1, at = c(1:4), line = 1, tick = F, labels = c("Unburned", "Low", "Moderate", "High"), cex.axis = 1.5)
for(i in 1:length(Fig2order.q)){
  points(x = rep(vec1.q[i], length(Quarry[,nut][Quarry$plotting == Fig2order.q[i]])),
         y = Quarry[,nut][Quarry$plotting == Fig2order.q[i]],
         col = rgb(0,0,0, alpha = 0.5),
         pch = 19)
  points(x = rep(vec2.q[i], length(Quarry[,nut][Quarry$plotting == Fig2paired.q[i]])),
         y = Quarry[,nut][Quarry$plotting == Fig2paired.q[i]],
         col = rgb(1,0,0, alpha = 0.5),
         pch = 19)
  
  points(x = vec1.q[i]+0.1,
         y = mean(Quarry[,nut][Quarry$plotting == Fig2order.q[i]]),
         col = rgb(0,0,0, alpha = 0.5),
         pch = 17)
  segments(y0 = (mean(Quarry[,nut][Quarry$plotting == Fig2order.q[i]])-se(Quarry[,nut][Quarry$plotting == Fig2order.q[i]])), x0 = vec1.q[i]+0.1, 
           y1 = (mean(Quarry[,nut][Quarry$plotting == Fig2order.q[i]])+se(Quarry[,nut][Quarry$plotting == Fig2order.q[i]])), x1 = vec1.q[i]+0.1, 
           col = rgb(0,0,0),lwd = 1.5)
  
  points(x = vec2.q[i]+0.1,
         y = mean(Quarry[,nut][Quarry$plotting == Fig2paired.q[i]]),
         col = rgb(1,0,0, alpha = 0.5),
         pch = 17)
  segments(y0 = (mean(Quarry[,nut][Quarry$plotting == Fig2paired.q[i]])-se(Quarry[,nut][Quarry$plotting == Fig2paired.q[i]])), x0 = vec2.q[i]+0.1, 
           y1 = (mean(Quarry[,nut][Quarry$plotting == Fig2paired.q[i]])+se(Quarry[,nut][Quarry$plotting == Fig2paired.q[i]])), x1 = vec2.q[i]+0.1, 
           col = rgb(1,0,0),lwd = 1.5)
  
}

plot(x = c(0.5:2.5),
     y = c(0.5:2.5),
     ylim = c(0,1),
     las = 1,
     cex.axis = 1.5,
     ylab = "", ## density
     type = "n",
     xaxt = "n",
     xlab = "") ## disturbance history
abline(h = 0.5, lty = 2)
axis(1, at = c(1:2), line = 1, tick = F, labels = c("Unburned", "Low"), cex.axis = 1.5)
for(i in 1:length(Fig2order.sc)){
  points(x = rep(vec1.sc[i], length(SC[,nut][SC$plotting == Fig2order.sc[i]])),
         y = SC[,nut][SC$plotting == Fig2order.sc[i]],
         col = rgb(0,0,0, alpha = 0.5),
         pch = 19)
  points(x = rep(vec2.sc[i], length(SC[,nut][SC$plotting == Fig2paired.sc[i]])),
         y = SC[,nut][SC$plotting == Fig2paired.sc[i]],
         col = rgb(1,0,0, alpha = 0.5),
         pch = 19)
  
  points(x = vec1.sc[i]+0.1,
         y = mean(SC[,nut][SC$plotting == Fig2order.sc[i]]),
         col = rgb(0,0,0, alpha = 0.5),
         pch = 17)
  segments(y0 = (mean(SC[,nut][SC$plotting == Fig2order.sc[i]])-se(SC[,nut][SC$plotting == Fig2order.sc[i]])), x0 = vec1.sc[i]+0.1, 
           y1 = (mean(SC[,nut][SC$plotting == Fig2order.sc[i]])+se(SC[,nut][SC$plotting == Fig2order.sc[i]])), x1 = vec1.sc[i]+0.1, 
           col = rgb(0,0,0),lwd = 1.5)
  
  points(x = vec2.sc[i]+0.1,
         y = mean(SC[,nut][SC$plotting == Fig2paired.sc[i]]),
         col = rgb(1,0,0, alpha = 0.5),
         pch = 17)
  segments(y0 = (mean(SC[,nut][SC$plotting == Fig2paired.sc[i]])-se(SC[,nut][SC$plotting == Fig2paired.sc[i]])), x0 = vec2.sc[i]+0.1, 
           y1 = (mean(SC[,nut][SC$plotting == Fig2paired.sc[i]])+se(SC[,nut][SC$plotting == Fig2paired.sc[i]])), x1 = vec2.sc[i]+0.1, 
           col = rgb(1,0,0),lwd = 1.5)
  
}

#### Figure 3 - Cover ####
par(mfrow = c(3,2))

## change the nut variable (borrowed from soils code) based on the column name in grad df
nut <- 'plot.cov'
plot(x = c(0.5:4.5),
     y = c(0.5:4.5),
     ylim = c(0,max(grad[,nut])),
     las = 1,
     cex.axis = 1.5,
     ylab = "", ## density
     type = "n",
     xaxt = "n",
     xlab = "") ## disturbance history
axis(1, at = c(1:4), line = 1, tick = F, labels = c("Unburned", "Low", "Moderate", "High"), cex.axis = 1.5)
for(i in 1:length(Fig2order.q)){
  points(x = rep(vec1.q[i], length(Quarry[,nut][Quarry$plotting == Fig2order.q[i]])),
         y = Quarry[,nut][Quarry$plotting == Fig2order.q[i]],
         col = rgb(0,0,0, alpha = 0.5),
         pch = 19)
  points(x = rep(vec2.q[i], length(Quarry[,nut][Quarry$plotting == Fig2paired.q[i]])),
         y = Quarry[,nut][Quarry$plotting == Fig2paired.q[i]],
         col = rgb(1,0,0, alpha = 0.5),
         pch = 19)
  
  points(x = vec1.q[i]+0.1,
         y = mean(Quarry[,nut][Quarry$plotting == Fig2order.q[i]]),
         col = rgb(0,0,0, alpha = 0.5),
         pch = 17)
  segments(y0 = (mean(Quarry[,nut][Quarry$plotting == Fig2order.q[i]])-se(Quarry[,nut][Quarry$plotting == Fig2order.q[i]])), x0 = vec1.q[i]+0.1, 
           y1 = (mean(Quarry[,nut][Quarry$plotting == Fig2order.q[i]])+se(Quarry[,nut][Quarry$plotting == Fig2order.q[i]])), x1 = vec1.q[i]+0.1, 
           col = rgb(0,0,0),lwd = 1.5)
  
  points(x = vec2.q[i]+0.1,
         y = mean(Quarry[,nut][Quarry$plotting == Fig2paired.q[i]]),
         col = rgb(1,0,0, alpha = 0.5),
         pch = 17)
  segments(y0 = (mean(Quarry[,nut][Quarry$plotting == Fig2paired.q[i]])-se(Quarry[,nut][Quarry$plotting == Fig2paired.q[i]])), x0 = vec2.q[i]+0.1, 
           y1 = (mean(Quarry[,nut][Quarry$plotting == Fig2paired.q[i]])+se(Quarry[,nut][Quarry$plotting == Fig2paired.q[i]])), x1 = vec2.q[i]+0.1, 
           col = rgb(1,0,0),lwd = 1.5)
  
}

plot(x = c(0.5:2.5),
     y = c(0.5:2.5),
     ylim = c(0,max(grad[,nut])),
     las = 1,
     cex.axis = 1.5,
     ylab = "", ## density
     type = "n",
     xaxt = "n",
     xlab = "") ## disturbance history
axis(1, at = c(1:2), line = 1, tick = F, labels = c("Unburned", "Low"), cex.axis = 1.5)
for(i in 1:length(Fig2order.sc)){
  points(x = rep(vec1.sc[i], length(SC[,nut][SC$plotting == Fig2order.sc[i]])),
         y = SC[,nut][SC$plotting == Fig2order.sc[i]],
         col = rgb(0,0,0, alpha = 0.5),
         pch = 19)
  points(x = rep(vec2.sc[i], length(SC[,nut][SC$plotting == Fig2paired.sc[i]])),
         y = SC[,nut][SC$plotting == Fig2paired.sc[i]],
         col = rgb(1,0,0, alpha = 0.5),
         pch = 19)
  
  points(x = vec1.sc[i]+0.1,
         y = mean(SC[,nut][SC$plotting == Fig2order.sc[i]]),
         col = rgb(0,0,0, alpha = 0.5),
         pch = 17)
  segments(y0 = (mean(SC[,nut][SC$plotting == Fig2order.sc[i]])-se(SC[,nut][SC$plotting == Fig2order.sc[i]])), x0 = vec1.sc[i]+0.1, 
           y1 = (mean(SC[,nut][SC$plotting == Fig2order.sc[i]])+se(SC[,nut][SC$plotting == Fig2order.sc[i]])), x1 = vec1.sc[i]+0.1, 
           col = rgb(0,0,0),lwd = 1.5)
  
  points(x = vec2.sc[i]+0.1,
         y = mean(SC[,nut][SC$plotting == Fig2paired.sc[i]]),
         col = rgb(1,0,0, alpha = 0.5),
         pch = 17)
  segments(y0 = (mean(SC[,nut][SC$plotting == Fig2paired.sc[i]])-se(SC[,nut][SC$plotting == Fig2paired.sc[i]])), x0 = vec2.sc[i]+0.1, 
           y1 = (mean(SC[,nut][SC$plotting == Fig2paired.sc[i]])+se(SC[,nut][SC$plotting == Fig2paired.sc[i]])), x1 = vec2.sc[i]+0.1, 
           col = rgb(1,0,0),lwd = 1.5)
  
}

## change the nutrients of interest based on the column name in soils df
nut <- 'prop.inv'
plot(x = c(0.5:4.5),
     y = c(0.5:4.5),
     ylim = c(0,1),
     las = 1,
     cex.axis = 1.5,
     ylab = "", ## density
     type = "n",
     xaxt = "n",
     xlab = "") ## disturbance history
abline(h = 0.5, lty = 2)
axis(1, at = c(1:4), line = 1, tick = F, labels = c("Unburned", "Low", "Moderate", "High"), cex.axis = 1.5)
for(i in 1:length(Fig2order.q)){
  points(x = rep(vec1.q[i], length(Quarry[,nut][Quarry$plotting == Fig2order.q[i]])),
         y = Quarry[,nut][Quarry$plotting == Fig2order.q[i]],
         col = rgb(0,0,0, alpha = 0.5),
         pch = 19)
  points(x = rep(vec2.q[i], length(Quarry[,nut][Quarry$plotting == Fig2paired.q[i]])),
         y = Quarry[,nut][Quarry$plotting == Fig2paired.q[i]],
         col = rgb(1,0,0, alpha = 0.5),
         pch = 19)
  
  points(x = vec1.q[i]+0.1,
         y = mean(Quarry[,nut][Quarry$plotting == Fig2order.q[i]]),
         col = rgb(0,0,0, alpha = 0.5),
         pch = 17)
  segments(y0 = (mean(Quarry[,nut][Quarry$plotting == Fig2order.q[i]])-se(Quarry[,nut][Quarry$plotting == Fig2order.q[i]])), x0 = vec1.q[i]+0.1, 
           y1 = (mean(Quarry[,nut][Quarry$plotting == Fig2order.q[i]])+se(Quarry[,nut][Quarry$plotting == Fig2order.q[i]])), x1 = vec1.q[i]+0.1, 
           col = rgb(0,0,0),lwd = 1.5)
  
  points(x = vec2.q[i]+0.1,
         y = mean(Quarry[,nut][Quarry$plotting == Fig2paired.q[i]]),
         col = rgb(1,0,0, alpha = 0.5),
         pch = 17)
  segments(y0 = (mean(Quarry[,nut][Quarry$plotting == Fig2paired.q[i]])-se(Quarry[,nut][Quarry$plotting == Fig2paired.q[i]])), x0 = vec2.q[i]+0.1, 
           y1 = (mean(Quarry[,nut][Quarry$plotting == Fig2paired.q[i]])+se(Quarry[,nut][Quarry$plotting == Fig2paired.q[i]])), x1 = vec2.q[i]+0.1, 
           col = rgb(1,0,0),lwd = 1.5)
  
}

plot(x = c(0.5:2.5),
     y = c(0.5:2.5),
     ylim = c(0,1),
     las = 1,
     cex.axis = 1.5,
     ylab = "", ## density
     type = "n",
     xaxt = "n",
     xlab = "") ## disturbance history
axis(1, at = c(1:2), line = 1, tick = F, labels = c("Unburned", "Low"), cex.axis = 1.5)
abline(h = 0.5, lty = 2)
for(i in 1:length(Fig2order.sc)){
  points(x = rep(vec1.sc[i], length(SC[,nut][SC$plotting == Fig2order.sc[i]])),
         y = SC[,nut][SC$plotting == Fig2order.sc[i]],
         col = rgb(0,0,0, alpha = 0.5),
         pch = 19)
  points(x = rep(vec2.sc[i], length(SC[,nut][SC$plotting == Fig2paired.sc[i]])),
         y = SC[,nut][SC$plotting == Fig2paired.sc[i]],
         col = rgb(1,0,0, alpha = 0.5),
         pch = 19)
  
  points(x = vec1.sc[i]+0.1,
         y = mean(SC[,nut][SC$plotting == Fig2order.sc[i]]),
         col = rgb(0,0,0, alpha = 0.5),
         pch = 17)
  segments(y0 = (mean(SC[,nut][SC$plotting == Fig2order.sc[i]])-se(SC[,nut][SC$plotting == Fig2order.sc[i]])), x0 = vec1.sc[i]+0.1, 
           y1 = (mean(SC[,nut][SC$plotting == Fig2order.sc[i]])+se(SC[,nut][SC$plotting == Fig2order.sc[i]])), x1 = vec1.sc[i]+0.1, 
           col = rgb(0,0,0),lwd = 1.5)
  
  points(x = vec2.sc[i]+0.1,
         y = mean(SC[,nut][SC$plotting == Fig2paired.sc[i]]),
         col = rgb(1,0,0, alpha = 0.5),
         pch = 17)
  segments(y0 = (mean(SC[,nut][SC$plotting == Fig2paired.sc[i]])-se(SC[,nut][SC$plotting == Fig2paired.sc[i]])), x0 = vec2.sc[i]+0.1, 
           y1 = (mean(SC[,nut][SC$plotting == Fig2paired.sc[i]])+se(SC[,nut][SC$plotting == Fig2paired.sc[i]])), x1 = vec2.sc[i]+0.1, 
           col = rgb(1,0,0),lwd = 1.5)
  
}

## change the nutrients of interest based on the column name in soils df
nut <- 'prop.ann'
plot(x = c(0.5:4.5),
     y = c(0.5:4.5),
     ylim = c(0,1),
     las = 1,
     cex.axis = 1.5,
     ylab = "", ## density
     type = "n",
     xaxt = "n",
     xlab = "") ## disturbance history
abline(h = 0.5, lty = 2)
axis(1, at = c(1:4), line = 1, tick = F, labels = c("Unburned", "Low", "Moderate", "High"), cex.axis = 1.5)
for(i in 1:length(Fig2order.q)){
  points(x = rep(vec1.q[i], length(Quarry[,nut][Quarry$plotting == Fig2order.q[i]])),
         y = Quarry[,nut][Quarry$plotting == Fig2order.q[i]],
         col = rgb(0,0,0, alpha = 0.5),
         pch = 19)
  points(x = rep(vec2.q[i], length(Quarry[,nut][Quarry$plotting == Fig2paired.q[i]])),
         y = Quarry[,nut][Quarry$plotting == Fig2paired.q[i]],
         col = rgb(1,0,0, alpha = 0.5),
         pch = 19)
  
  points(x = vec1.q[i]+0.1,
         y = mean(Quarry[,nut][Quarry$plotting == Fig2order.q[i]]),
         col = rgb(0,0,0, alpha = 0.5),
         pch = 17)
  segments(y0 = (mean(Quarry[,nut][Quarry$plotting == Fig2order.q[i]])-se(Quarry[,nut][Quarry$plotting == Fig2order.q[i]])), x0 = vec1.q[i]+0.1, 
           y1 = (mean(Quarry[,nut][Quarry$plotting == Fig2order.q[i]])+se(Quarry[,nut][Quarry$plotting == Fig2order.q[i]])), x1 = vec1.q[i]+0.1, 
           col = rgb(0,0,0),lwd = 1.5)
  
  points(x = vec2.q[i]+0.1,
         y = mean(Quarry[,nut][Quarry$plotting == Fig2paired.q[i]]),
         col = rgb(1,0,0, alpha = 0.5),
         pch = 17)
  segments(y0 = (mean(Quarry[,nut][Quarry$plotting == Fig2paired.q[i]])-se(Quarry[,nut][Quarry$plotting == Fig2paired.q[i]])), x0 = vec2.q[i]+0.1, 
           y1 = (mean(Quarry[,nut][Quarry$plotting == Fig2paired.q[i]])+se(Quarry[,nut][Quarry$plotting == Fig2paired.q[i]])), x1 = vec2.q[i]+0.1, 
           col = rgb(1,0,0),lwd = 1.5)
  
}

plot(x = c(0.5:2.5),
     y = c(0.5:2.5),
     ylim = c(0,1),
     las = 1,
     cex.axis = 1.5,
     ylab = "", ## density
     type = "n",
     xaxt = "n",
     xlab = "") ## disturbance history
abline(h = 0.5, lty = 2)
axis(1, at = c(1:2), line = 1, tick = F, labels = c("Unburned", "Low"), cex.axis = 1.5)
for(i in 1:length(Fig2order.sc)){
  points(x = rep(vec1.sc[i], length(SC[,nut][SC$plotting == Fig2order.sc[i]])),
         y = SC[,nut][SC$plotting == Fig2order.sc[i]],
         col = rgb(0,0,0, alpha = 0.5),
         pch = 19)
  points(x = rep(vec2.sc[i], length(SC[,nut][SC$plotting == Fig2paired.sc[i]])),
         y = SC[,nut][SC$plotting == Fig2paired.sc[i]],
         col = rgb(1,0,0, alpha = 0.5),
         pch = 19)
  
  points(x = vec1.sc[i]+0.1,
         y = mean(SC[,nut][SC$plotting == Fig2order.sc[i]]),
         col = rgb(0,0,0, alpha = 0.5),
         pch = 17)
  segments(y0 = (mean(SC[,nut][SC$plotting == Fig2order.sc[i]])-se(SC[,nut][SC$plotting == Fig2order.sc[i]])), x0 = vec1.sc[i]+0.1, 
           y1 = (mean(SC[,nut][SC$plotting == Fig2order.sc[i]])+se(SC[,nut][SC$plotting == Fig2order.sc[i]])), x1 = vec1.sc[i]+0.1, 
           col = rgb(0,0,0),lwd = 1.5)
  
  points(x = vec2.sc[i]+0.1,
         y = mean(SC[,nut][SC$plotting == Fig2paired.sc[i]]),
         col = rgb(1,0,0, alpha = 0.5),
         pch = 17)
  segments(y0 = (mean(SC[,nut][SC$plotting == Fig2paired.sc[i]])-se(SC[,nut][SC$plotting == Fig2paired.sc[i]])), x0 = vec2.sc[i]+0.1, 
           y1 = (mean(SC[,nut][SC$plotting == Fig2paired.sc[i]])+se(SC[,nut][SC$plotting == Fig2paired.sc[i]])), x1 = vec2.sc[i]+0.1, 
           col = rgb(1,0,0),lwd = 1.5)
  
}

#### Management Brief Cover Figure ####
par(mfrow = c(2,2))
nut <- "plot.cov"
plot(x = c(0.5:4.5),
     y = c(0.5:4.5),
     ylim = c(0,max(Quarry$plot.cov)),
     las = 1,
     cex.axis = 1.5,
     ylab = "", ## density
     type = "n",
     xaxt = "n",
     xlab = "") ## disturbance history
axis(1, at = c(1:4), line = 1, tick = F, labels = c("Unburned", "Low", "Moderate", "High"), cex.axis = 1.5)
for(i in 1:length(Fig2order.q)){
  points(x = rep(vec1.q[i], length(Quarry[,nut][Quarry$plotting == Fig2order.q[i]])),
         y = Quarry[,nut][Quarry$plotting == Fig2order.q[i]],
         col = rgb(0,0,0, alpha = 0.5),
         pch = 19)
  points(x = rep(vec2.q[i], length(Quarry[,nut][Quarry$plotting == Fig2paired.q[i]])),
         y = Quarry[,nut][Quarry$plotting == Fig2paired.q[i]],
         col = rgb(1,0,0, alpha = 0.5),
         pch = 19)
  
  points(x = vec1.q[i]+0.1,
         y = mean(Quarry[,nut][Quarry$plotting == Fig2order.q[i]]),
         col = rgb(0,0,0, alpha = 0.5),
         pch = 17)
  segments(y0 = (mean(Quarry[,nut][Quarry$plotting == Fig2order.q[i]])-se(Quarry[,nut][Quarry$plotting == Fig2order.q[i]])), x0 = vec1.q[i]+0.1, 
           y1 = (mean(Quarry[,nut][Quarry$plotting == Fig2order.q[i]])+se(Quarry[,nut][Quarry$plotting == Fig2order.q[i]])), x1 = vec1.q[i]+0.1, 
           col = rgb(0,0,0),lwd = 1.5)
  
  points(x = vec2.q[i]+0.1,
         y = mean(Quarry[,nut][Quarry$plotting == Fig2paired.q[i]]),
         col = rgb(1,0,0, alpha = 0.5),
         pch = 17)
  segments(y0 = (mean(Quarry[,nut][Quarry$plotting == Fig2paired.q[i]])-se(Quarry[,nut][Quarry$plotting == Fig2paired.q[i]])), x0 = vec2.q[i]+0.1, 
           y1 = (mean(Quarry[,nut][Quarry$plotting == Fig2paired.q[i]])+se(Quarry[,nut][Quarry$plotting == Fig2paired.q[i]])), x1 = vec2.q[i]+0.1, 
           col = rgb(1,0,0),lwd = 1.5)
  
}
nut <- "prop.inv"
plot(x = c(0.5:4.5),
     y = c(0.5:4.5),
     ylim = c(0,1),
     las = 1,
     cex.axis = 1.5,
     ylab = "", ## density
     type = "n",
     xaxt = "n",
     xlab = "") ## disturbance history
abline(h = 0.5, lty = 2)
axis(1, at = c(1:4), line = 1, tick = F, labels = c("Unburned", "Low", "Moderate", "High"), cex.axis = 1.5)
for(i in 1:length(Fig2order.q)){
  points(x = rep(vec1.q[i], length(Quarry[,nut][Quarry$plotting == Fig2order.q[i]])),
         y = Quarry[,nut][Quarry$plotting == Fig2order.q[i]],
         col = rgb(0,0,0, alpha = 0.5),
         pch = 19)
  points(x = rep(vec2.q[i], length(Quarry[,nut][Quarry$plotting == Fig2paired.q[i]])),
         y = Quarry[,nut][Quarry$plotting == Fig2paired.q[i]],
         col = rgb(1,0,0, alpha = 0.5),
         pch = 19)
  
  points(x = vec1.q[i]+0.1,
         y = mean(Quarry[,nut][Quarry$plotting == Fig2order.q[i]]),
         col = rgb(0,0,0, alpha = 0.5),
         pch = 17)
  segments(y0 = (mean(Quarry[,nut][Quarry$plotting == Fig2order.q[i]])-se(Quarry[,nut][Quarry$plotting == Fig2order.q[i]])), x0 = vec1.q[i]+0.1, 
           y1 = (mean(Quarry[,nut][Quarry$plotting == Fig2order.q[i]])+se(Quarry[,nut][Quarry$plotting == Fig2order.q[i]])), x1 = vec1.q[i]+0.1, 
           col = rgb(0,0,0),lwd = 1.5)
  
  points(x = vec2.q[i]+0.1,
         y = mean(Quarry[,nut][Quarry$plotting == Fig2paired.q[i]]),
         col = rgb(1,0,0, alpha = 0.5),
         pch = 17)
  segments(y0 = (mean(Quarry[,nut][Quarry$plotting == Fig2paired.q[i]])-se(Quarry[,nut][Quarry$plotting == Fig2paired.q[i]])), x0 = vec2.q[i]+0.1, 
           y1 = (mean(Quarry[,nut][Quarry$plotting == Fig2paired.q[i]])+se(Quarry[,nut][Quarry$plotting == Fig2paired.q[i]])), x1 = vec2.q[i]+0.1, 
           col = rgb(1,0,0),lwd = 1.5)
  
}
nut <- "prop.ann"
plot(x = c(0.5:4.5),
     y = c(0.5:4.5),
     ylim = c(0,1),
     las = 1,
     cex.axis = 1.5,
     ylab = "", ## density
     type = "n",
     xaxt = "n",
     xlab = "") ## disturbance history
abline(h = 0.5, lty = 2)
axis(1, at = c(1:4), line = 1, tick = F, labels = c("Unburned", "Low", "Moderate", "High"), cex.axis = 1.5)
for(i in 1:length(Fig2order.q)){
  points(x = rep(vec1.q[i], length(Quarry[,nut][Quarry$plotting == Fig2order.q[i]])),
         y = Quarry[,nut][Quarry$plotting == Fig2order.q[i]],
         col = rgb(0,0,0, alpha = 0.5),
         pch = 19)
  points(x = rep(vec2.q[i], length(Quarry[,nut][Quarry$plotting == Fig2paired.q[i]])),
         y = Quarry[,nut][Quarry$plotting == Fig2paired.q[i]],
         col = rgb(1,0,0, alpha = 0.5),
         pch = 19)
  
  points(x = vec1.q[i]+0.1,
         y = mean(Quarry[,nut][Quarry$plotting == Fig2order.q[i]]),
         col = rgb(0,0,0, alpha = 0.5),
         pch = 17)
  segments(y0 = (mean(Quarry[,nut][Quarry$plotting == Fig2order.q[i]])-se(Quarry[,nut][Quarry$plotting == Fig2order.q[i]])), x0 = vec1.q[i]+0.1, 
           y1 = (mean(Quarry[,nut][Quarry$plotting == Fig2order.q[i]])+se(Quarry[,nut][Quarry$plotting == Fig2order.q[i]])), x1 = vec1.q[i]+0.1, 
           col = rgb(0,0,0),lwd = 1.5)
  
  points(x = vec2.q[i]+0.1,
         y = mean(Quarry[,nut][Quarry$plotting == Fig2paired.q[i]]),
         col = rgb(1,0,0, alpha = 0.5),
         pch = 17)
  segments(y0 = (mean(Quarry[,nut][Quarry$plotting == Fig2paired.q[i]])-se(Quarry[,nut][Quarry$plotting == Fig2paired.q[i]])), x0 = vec2.q[i]+0.1, 
           y1 = (mean(Quarry[,nut][Quarry$plotting == Fig2paired.q[i]])+se(Quarry[,nut][Quarry$plotting == Fig2paired.q[i]])), x1 = vec2.q[i]+0.1, 
           col = rgb(1,0,0),lwd = 1.5)
  
}

rm(Quarry);rm(SC);rm(Fig2order.q);rm(Fig2paired.q);rm(Fig2order.sc);rm(Fig2paired.sc)
rm(i);rm(vec1.sc);rm(vec2.sc);rm(vec1.q);rm(vec2.q)
rm(nut)


# #### Supplemental - cov ~ PO4 ####
# par(mfrow = c(1,1))
# 
# mod <- lm(plot.cov ~ NO3+NH4+PO4+trt+sev, data = q.grad)
# plot(plot.cov ~ PO4, data = q.grad,
#      las = 1,
#      log = "y",
#      cex.axis = 1.5,
#      pch = 16,
#      cex = 0.75,
#      xlab = " ",
#      ylab = " ")
# newdata <- data.frame(NO3=mean(q.grad$NO3, na.rm = T),
#                       NH4=mean(q.grad$NH4, na.rm = T),
#                       PO4=seq(min(q.grad$PO4, na.rm = T), max(q.grad$PO4, na.rm = T), length.out = 30),
#                       trt=q.grad$trt[1],
#                       sev=q.grad$sev[1])
# preds <- predict(mod, newdata, type = "response", se.fit = TRUE)
# preds$upperci <- preds$fit + (1.96*preds$se.fit)
# preds$lowerci <- preds$fit - (1.96*preds$se.fit)
# polygon(x = c(newdata$PO4, rev(newdata$PO4)),
#         y = c(preds$lowerci, rev(preds$upperci)),
#         col = adjustcolor("black", alpha.f = 0.25),
#         border = NA)
# lines(newdata$PO4, preds$fit, lty = 1)
# 

#### Question 2 - Supplemental Ordinations ####
str(env)
env$sev <- factor(env$sev, levels = c("unburn", "low", "mod", "high"))
env$trt <- as.factor(env$trt)
levels(env$sev)
env$NO3 <- soils$NO3[match(env$plot, soils$Field.ID)]
env$NH4 <- soils$NH4[match(env$plot, soils$Field.ID)]
env$PO4 <- soils$PO4[match(env$plot, soils$Field.ID)]
env$tot.cov <- PlotCov$TotalPlotCover[match(env$plot, rownames(PlotCov))]
env$rich <- PlotRichness$richness[match(env$plot, rownames(PlotRichness))]


## Quarry Fire  
Quarry <- as.matrix(comm.sum[match(env$plot[env$site == "quarry"], rownames(comm.sum)),])
str(Quarry)
env.q <- env[match(rownames(Quarry),env$plot),]

## Stone Canyon 
SC <- as.matrix(comm.sum[match(env$plot[env$site == "sc"], rownames(comm.sum)),])
str(SC)
env.sc <- env[match(rownames(SC),env$plot),]

pal <- c("#f6d746", "#e55c30", "#84206b", "#140b34")
pal2 <- c("black", "red")


## Quarry NMDS Ordination
ord.nmds.stress.q <- rep(NA,10)
for(i in 1:10){
  ord.nmds.stress.q[i] <- metaMDS(Quarry, k = i, try = 1000, distance = "bray")$stress 
}
par(mfrow = c(2,2))
plot(ord.nmds.stress.q,
     ylim = c(0,1),
     ylab = "Stress",
     xlab = "Number of Dimensions",
     main = "Quarry Fire",
     las = 1,
     pch = 16)
abline(h = 0.2)

set.seed(1)
NMDSord.q <- metaMDS(Quarry, k = 3, try = 1000, distance = "bray") ## convergence
stressplot(NMDSord.q,
           main = "Quarry Fire")
## Stone Canyon NMDS Ordination
ord.nmds.stress.sc <- rep(NA,10)
for(i in 1:10){
  ord.nmds.stress.sc[i] <- metaMDS(SC, k = i, try = 1000, distance = "bray")$stress 
}
plot(ord.nmds.stress.sc,
     ylim = c(0,1),
     ylab = "Stress",
     xlab = "Number of Dimensions",
     main = "Stone Canyon",
     las = 1,
     pch = 16)
abline(h = 0.2)

set.seed(1)
NMDSord.sc <- metaMDS(SC, k = 4, try = 1000, distance = "bray") ## convergence
stressplot(NMDSord.sc,
           main = "Stone Canyon")

par(mfrow = c(2,2))
## a - severity
plot(NMDSord.q, type = "n",display = "sites", las = 1) 
points(NMDSord.q, display = "sites", pch = 19, cex = .75, col = pal[as.factor(env.q$sev)])
vectors <- envfit(NMDSord.q, env.q[,c(11,14:19)],na.rm = TRUE)
ordiellipse(NMDSord.q, display = "sites", env.q$sev, draw = "lines",
            col = pal, label = FALSE) ## ellipses based on slide exposure
plot(vectors, col = "black")
# legend("bottomright", legend = c("Unburned", "Low", "Mod", "High"), col = pal, pch = 15, cex = 1, ncol = 2, bty = "n")

## b - fire retardant
plot(NMDSord.q, type = "n",display = "sites", las = 1) 
points(NMDSord.q, display = "sites", pch = 19, cex = .75, col = pal2[as.factor(env.q$trt)])
vectors <- envfit(NMDSord.q, env.q[,c(11,14:19)],na.rm = TRUE)
ordiellipse(NMDSord.q, display = "sites", env.q$trt, draw = "lines",
            col = pal2, label = FALSE) ## ellipses based on slide exposure
plot(vectors, col = "black")
# legend("bottomright", legend = c("Control", "Fire Retardant"), col = pal2, pch = 15, cex = 1, ncol = 2, bty = "n")

## c - severity
plot(NMDSord.sc, type = "n",display = "sites", las = 1) 
points(NMDSord.sc, display = "sites", pch = 19, cex = .75, col = pal[as.factor(env.sc$sev)])
vectors <- envfit(NMDSord.sc, env.sc[,c(11,14:19)],na.rm = TRUE)
ordiellipse(NMDSord.sc, display = "sites", env.sc$sev, draw = "lines",
            col = pal, label = FALSE) ## ellipses based on slide exposure
plot(vectors, col = "black")
# legend("bottomright", legend = c("Unburned", "Low", "Mod", "High"), col = pal, pch = 15, cex = 1, ncol = 2, bty = "n")

## d - fire retardant
plot(NMDSord.sc, type = "n",display = "sites", las = 1) 
points(NMDSord.sc, display = "sites", pch = 19, cex = .75, col = pal2[as.factor(env.sc$trt)])
vectors <- envfit(NMDSord.sc, env.sc[,c(11,14:19)],na.rm = TRUE)
ordiellipse(NMDSord.sc, display = "sites", env.sc$trt, draw = "lines",
            col = pal2, label = FALSE) ## ellipses based on slide exposure
plot(vectors, col = "black")
# legend("bottomright", legend = c("Control", "Fire Retardant"), col = pal2, pch = 15, cex = 1, ncol = 2, bty = "n")

rm(i);rm(ord.nmds.stress.q);rm(pal);rm(pal2);rm(se);rm(vectors);rm(Quarry)
rm(SC);rm(env.q);rm(env.sc);rm(NMDSord.q)
rm(NMDSord.sc);rm(ord.nmds.stress.sc);rm(mod);rm(preds);rm(newdata)


#### Supplemental Table Species Frequencies ####
summ.tab <- comm.sum.long
summ.tab$duration <- sp.info$duration[match(summ.tab$code, sp.info$code)]
summ.tab$status <- sp.info$status[match(summ.tab$code, sp.info$code)]
summ.tab$FG <- sp.info$functional.group[match(summ.tab$code, sp.info$code)]
summ.tab$name <- sp.info$species[match(summ.tab$code, sp.info$code)]
summ.tab$plotavg <- 1

summ.tab <- summ.tab[complete.cases(summ.tab),]
(summ.tab$plot %in% summ.tab$plot[grep("SC", summ.tab$plot)])
summ.tab$site <- ifelse((summ.tab$plot %in% summ.tab$plot[grep("SC", summ.tab$plot)]), "SC", "Q")

## splitting into SC (easier site)
summ.tab.sc <- summ.tab[summ.tab$site == "SC",]
table(summ.tab.sc$plot)
## BURN
## CON
## FRL

summ.tab.sc$trt <- NA
summ.tab.sc$trt[grep("BURN",summ.tab.sc$plot)] <- "burn"
summ.tab.sc$trt[grep("CON",summ.tab.sc$plot)] <- "con"
summ.tab.sc$trt[grep("FRL",summ.tab.sc$plot)] <- "fr"
table(summ.tab.sc$trt)

trts <- c("burn","con", "fr")
trts.list <- list()
for(i in 1:3){
  summ.tab.sc.i <- summ.tab.sc[summ.tab.sc$trt == trts[i],]
  summ.tab.sc.i <- summ.tab.sc.i[!duplicated(summ.tab.sc.i$code),]
  
  summary.list <- list()
  summary.list[[1]] <- trts[i]
  summary.list[[2]] <- table(summ.tab.sc.i$duration)
  dur.df <- aggregate(plotavg ~ name + duration, FUN = sum, data = summ.tab.sc[summ.tab.sc$trt == trts[i],])
  dur.df <- dur.df[dur.df$duration == "annual",]
  dur.df <- dur.df[rev(order(dur.df$plotavg))[c(1:10)], c(1,3)]
  dur.df[,2] <- round(dur.df[,2]/(ifelse(trts[i] == "con", 17, 18)),3)
  dur.vec <- paste0(dur.df[,1], sep = " (", dur.df[,2], sep = ")")
  dur.vec <- paste0(dur.vec, collapse = ", ")
  summary.list[[3]] <- dur.vec
  
  summary.list[[4]] <- table(summ.tab.sc.i$status)
  dur.df <- aggregate(plotavg ~ name + status, FUN = sum, data = summ.tab.sc[summ.tab.sc$trt == trts[i],])
  dur.df <- dur.df[dur.df$status == "I",]
  dur.df <- dur.df[rev(order(dur.df$plotavg))[c(1:10)], c(1,3)]
  dur.df[,2] <- round(dur.df[,2]/(ifelse(trts[i] == "con", 17, 18)),3)
  dur.vec <- paste0(dur.df[,1], sep = " (", dur.df[,2], sep = ")")
  dur.vec <- paste0(dur.vec, collapse = ", ")
  summary.list[[5]] <- dur.vec
  
  summary.list[[6]] <- table(summ.tab.sc.i$FG)
  dur.df <- aggregate(plotavg ~ name + FG, FUN = sum, data = summ.tab.sc[summ.tab.sc$trt == trts[i],])
  dur.df <- dur.df[dur.df$FG == "gram",]
  dur.df <- dur.df[rev(order(dur.df$plotavg))[c(1:10)], c(1,3)]
  dur.df[,2] <- round(dur.df[,2]/(ifelse(trts[i] == "con", 17, 18)),3)
  dur.vec <- paste0(dur.df[,1], sep = " (", dur.df[,2], sep = ")")
  dur.vec <- paste0(dur.vec, collapse = ", ")
  summary.list[[7]] <- dur.vec
  dur.df <- aggregate(plotavg ~ name + FG, FUN = sum, data = summ.tab.sc[summ.tab.sc$trt == trts[i],])
  dur.df <- dur.df[dur.df$FG == "forb",]
  dur.df <- dur.df[rev(order(dur.df$plotavg))[c(1:10)], c(1,3)]
  dur.df[,2] <- round(dur.df[,2]/(ifelse(trts[i] == "con", 17, 18)),3)
  dur.vec <- paste0(dur.df[,1], sep = " (", dur.df[,2], sep = ")")
  dur.vec <- paste0(dur.vec, collapse = ", ")
  summary.list[[8]] <- dur.vec
  
  dur.df <- as.data.frame(table(summ.tab.sc$name[summ.tab.sc$duration == "annual" & summ.tab.sc$trt == trts[i] & summ.tab.sc$status == "I"]))
  dur.df <- dur.df[rev(order(dur.df$Freq))[c(1:10)],]
  dur.df[,2] <- round(dur.df[,2]/(ifelse(trts[i] == "con", 17, 18)),3)
  dur.vec <- paste0(dur.df[,1], sep = " (", dur.df[,2], sep = ")")
  dur.vec <- paste0(dur.vec, collapse = ", ")
  summary.list[[9]] <- length(unique(summ.tab.sc$name[summ.tab.sc$duration == "annual" & summ.tab.sc$trt == trts[i] & summ.tab.sc$status == "I"]))
  summary.list[[10]] <- dur.vec
  
  trts.list[[i]] <- summary.list
  
}

trts.list

## splitting into Quarry
summ.tab.q <- summ.tab[summ.tab$site == "Q",]
table(summ.tab.q$plot)
## UC
## UFR
## LC
## LFR
## MC
## MFR
## HC
## HFR

summ.tab.q$trt <- NA
summ.tab.q$trt[grep("UC",summ.tab.q$plot)] <- "uc"
summ.tab.q$trt[grep("UFR",summ.tab.q$plot)] <- "ufr"
summ.tab.q$trt[grep("LC",summ.tab.q$plot)] <- "lc"
summ.tab.q$trt[grep("LFR",summ.tab.q$plot)] <- "lfr"
summ.tab.q$trt[grep("MC",summ.tab.q$plot)] <- "mc"
summ.tab.q$trt[grep("MFR",summ.tab.q$plot)] <- "mfr"
summ.tab.q$trt[grep("HC",summ.tab.q$plot)] <- "hc"
summ.tab.q$trt[grep("HFR",summ.tab.q$plot)] <- "hfr"

table(summ.tab.q$trt)

trts <- c("uc","ufr", "lc", "lfr", "mc", "mfr", "hc", "hfr")
trts.list <- list()
for(i in 1:length(trts)){
  summ.tab.q.i <- summ.tab.q[summ.tab.q$trt == trts[i],]
  summ.tab.q.i <- summ.tab.q.i[!duplicated(summ.tab.q.i$code),]
  
  summary.list <- list()
  
  summary.list[[1]] <- trts[i]
  summary.list[[2]] <- table(summ.tab.q.i$duration)
  dur.df <- aggregate(plotavg ~ name + duration, FUN = sum, data = summ.tab.q[summ.tab.q$trt == trts[i],])
  dur.df <- dur.df[dur.df$duration == "annual",]
  dur.df <- dur.df[rev(order(dur.df$plotavg))[c(1:10)], c(1,3)]
  dur.df[,2] <- round(dur.df[,2]/(ifelse(trts[i] == "mfr", 6, 7)),3)
  dur.vec <- paste0(dur.df[,1], sep = " (", dur.df[,2], sep = ")")
  dur.vec <- paste0(dur.vec, collapse = ", ")
  summary.list[[3]] <- dur.vec
  
  summary.list[[4]] <- table(summ.tab.q.i$status)
  dur.df <- aggregate(plotavg ~ name + status, FUN = sum, data = summ.tab.q[summ.tab.q$trt == trts[i],])
  dur.df <- dur.df[dur.df$status == "I",]
  dur.df <- dur.df[rev(order(dur.df$plotavg))[c(1:10)], c(1,3)]
  dur.df[,2] <- round(dur.df[,2]/(ifelse(trts[i] == "mfr", 6, 7)),3)
  dur.vec <- paste0(dur.df[,1], sep = " (", dur.df[,2], sep = ")")
  dur.vec <- paste0(dur.vec, collapse = ", ")
  summary.list[[5]] <- dur.vec
  
  summary.list[[6]] <- table(summ.tab.q.i$FG)
  dur.df <- aggregate(plotavg ~ name + FG, FUN = sum, data = summ.tab.q[summ.tab.q$trt == trts[i],])
  dur.df <- dur.df[dur.df$FG == "gram",]
  dur.df <- dur.df[rev(order(dur.df$plotavg))[c(1:10)], c(1,3)]
  dur.df[,2] <- round(dur.df[,2]/(ifelse(trts[i] == "mfr", 6, 7)),3)
  dur.vec <- paste0(dur.df[,1], sep = " (", dur.df[,2], sep = ")")
  dur.vec <- paste0(dur.vec, collapse = ", ")
  summary.list[[7]] <- dur.vec
  dur.df <- aggregate(plotavg ~ name + FG, FUN = sum, data = summ.tab.q[summ.tab.q$trt == trts[i],])
  dur.df <- dur.df[dur.df$FG == "forb",]
  dur.df <- dur.df[rev(order(dur.df$plotavg))[c(1:10)], c(1,3)]
  dur.df[,2] <- round(dur.df[,2]/(ifelse(trts[i] == "mfr", 6, 7)),3)
  dur.vec <- paste0(dur.df[,1], sep = " (", dur.df[,2], sep = ")")
  dur.vec <- paste0(dur.vec, collapse = ", ")
  summary.list[[8]] <- dur.vec
  
  dur.df <- as.data.frame(table(summ.tab.q$name[summ.tab.q$duration == "annual" & summ.tab.q$trt == trts[i] & summ.tab.q$status == "I"]))
  dur.df <- dur.df[rev(order(dur.df$Freq))[c(1:10)],]
  dur.df[,2] <- round(dur.df[,2]/(ifelse(trts[i] == "mfr", 6, 7)),3)
  dur.vec <- paste0(dur.df[,1], sep = " (", dur.df[,2], sep = ")")
  dur.vec <- paste0(dur.vec, collapse = ", ")
  summary.list[[9]] <- length(unique(summ.tab.q$name[summ.tab.q$duration == "annual" & summ.tab.q$trt == trts[i] & summ.tab.q$status == "I"]))
  summary.list[[10]] <- dur.vec
  
  trts.list[[i]] <- summary.list
  
}
trts.list
