#### Begin - Set Working Directory ####
## quick script for cleaning environmental data
library(here)
here()
setwd(here()) ## set working directory to the data folder

## plot data 
trtdata <- read.csv("./data/plotMetadata.csv")
trtdata <- trtdata[trtdata$site == "quarry",]
head(trtdata)
env <- read.csv("./data/Quarry_ENV.csv")
head(env)
class <- read.csv("./data/class_2026.csv")
head(class)


colnames(trtdata)
env_com <- data.frame(plot = trtdata$plot,
                      sev = trtdata$sev,
                      trt = trtdata$trt,
                      class = class$class[match(trtdata$plot, class$plot)],
                      elev = env$Elevation[match(trtdata$plot,env$GPX)],
                      slope = env$Slope[match(trtdata$plot,env$GPX)],
                      tpi = env$TPI[match(trtdata$plot,env$GPX)],
                      aspect = env$aspect[match(trtdata$plot,env$GPX)])

write.csv(env_com, "./data/ENV_Data.csv")
rm(list = ls()) ## cleaning global env
gc()

