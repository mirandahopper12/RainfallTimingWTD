#### Rainfall Timing and White-tailed Deer ####

#### Libraries ####

library(tidyverse)
library(nlme)
library(lme4)
library(lmerTest)
library(AICcmodavg)

#### Datasets ####

FawnsFull <- read.csv("./FawnsFull.csv", header = TRUE, row.names = NULL)
YearlingsFull <- read.csv("./YearlingsFull.csv", header = TRUE, row.names = NULL)


##### Fawns #####

FawnsFull$ranch <- as.factor(FawnsFull$ranch)
FawnsFull$Year <- as.factor(FawnsFull$Year)
FawnsFull$Sex <- as.factor(FawnsFull$Sex)
FawnsFull$MassKg <- as.numeric(FawnsFull$MassKg)
FawnsFull$April <- FawnsFull$Apr/10

###### Fawn Models ######

FlmeNull <- with(FawnsFull, lmer(MassKg ~ 1 + (1|Year) + (1|ranch), na.action = na.omit, REML = FALSE))
FlmeGes <- with(FawnsFull, lmer(MassKg ~ Ges + Sex + (1|Year) + (1|ranch), na.action = na.omit, REML = FALSE))
FlmeTrdTri <- with(FawnsFull, lmer(MassKg ~ TrdTri + Sex + (1|Year) + (1|ranch), na.action = na.omit, REML = FALSE))
FlmeApr <- with(FawnsFull, lmer(MassKg ~ Apr + Sex + (1|Year) + (1|ranch), na.action = na.omit, REML = FALSE))
FlmeFawn <- with(FawnsFull, lmer(MassKg ~ Fawn + Sex + (1|Year) + (1|ranch), na.action = na.omit, REML = FALSE))
FlmeFstMon <- with(FawnsFull, lmer(MassKg ~ FstMon + Sex + (1|Year) + (1|ranch), na.action = na.omit, REML = FALSE))
FlmeSept <- with(FawnsFull, lmer(MassKg ~ Sept + Sex + (1|Year) + (1|ranch), na.action = na.omit, REML = FALSE))
FlmeLac <- with(FawnsFull, lmer(MassKg ~ Lac + Sex + (1|Year) + (1|ranch), na.action = na.omit, REML = FALSE))

###### Fawn AIC ######

fawnmodels <- list(FlmeNull, FlmeGes, FlmeTrdTri, FlmeApr, FlmeFawn, FlmeFstMon, 
                   FlmeSept, FlmeLac)

modelnames <- c("Null", "Gestation", "Third Tri.", "E.G.S.", "Fawning", 
                "First Month", "L.G.S.", "Lactation")

FawnsAIC <- aictab(cand.set = fawnmodels, modnames = modelnames,
                   second.ord = FALSE, sort = TRUE)

view(FawnsAIC)

summary(FlmeApr)


##### Yearlings #####

YearlingsFull$ranch <- as.factor(YearlingsFull$ranch)
YearlingsFull$Year <- as.factor(YearlingsFull$Year)
YearlingsFull$Sex <- as.factor(YearlingsFull$Sex)
YearlingsFull$MassKg <- as.numeric(YearlingsFull$MassKg)

###### Yearling Models ######

YlmeNull <- with(YearlingsFull, lmer(MassKg ~ 1 + (1|Year) + (1|ranch), na.action = na.omit, REML = FALSE))
YlmeApr <- with(YearlingsFull, lmer(MassKg ~ Apr + Sex + (1|Year) + (1|ranch), na.action = na.omit, REML = FALSE))
YlmeSept <- with(YearlingsFull, lmer(MassKg ~ Sept + Sex + (1|Year) + (1|ranch), na.action = na.omit, REML = FALSE))
PYlmeApr <- with(YearlingsFull, lmer(MassKg ~ PApr + Sex + (1|Year) + (1|ranch), na.action = na.omit, REML = FALSE))
AYlmeApr <- with(YearlingsFull, lmer(MassKg ~ Apr + Sex + PApr + (1|Year) + (1|ranch), na.action = na.omit, REML = FALSE))
AYlmeSept <- with(YearlingsFull, lmer(MassKg ~ Sept + Sex + PApr + (1|Year) + (1|ranch), na.action = na.omit, REML = FALSE))

###### Yearling AIC ######

yearlingmodels <- list(YlmeNull, YlmeApr, YlmeSept, PYlmeApr, 
                       AYlmeApr, AYlmeSept)

Ymodelnames <- c("Null", "CY EGS", "CY LGS", "PY EGS", "PY EGS + CY EGS",
                 "PY EGS + CY LGS")

YearlingsAIC <- aictab(cand.set = yearlingmodels, modnames = Ymodelnames,
                       second.ord = FALSE, sort = TRUE)

view(YearlingsAIC)


summary(AYlmeApr)