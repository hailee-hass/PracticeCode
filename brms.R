# Bayesian Regression Models using stan (brms)
# Estimating phylogenetic multilevel models with brms
# REF: https://cran.r-project.org/web/packages/brms/vignettes/brms_phylogenetics.html

### Analysis of sustained damage responses to genome size ###

#This calculates the overall amount of damage each species sustained and fits this against nutrient treatment and GS in a phylogenetic linear mixed effect model, across all species

#Load in libraries
library(brms)
library(ape)
library(ggplot2)
library(lme4)
library(dplyr)

#Set and check wd; Read in data
getwd()

bigdata <- read.csv("DATA NAME.csv") 

#Remove odd duplicates and species with no GS data
bigdata <- unique(bigdata[!is.na(bigdata$GS),]) #857 observations; 23 excluded

#Build brms models
# HOW TO INTERPRET OUTPUTS:
  #Estimate / error = Z-score
  #1.96 is the cut-off value for significance using 95% CI; i.e., if greater than 1.96 then it's sig at alpha = 0.05
  #HAS TO GO OVER/THROUGH 0 TO BE SIGNIFICANT
  #Posterior plots need to be bell curve/normal distribution

#1. Overall model
phylogeny <- read.tree("small-tree3.tre") #will need trees to input here

#Reformat factor levels for nutrients
bigdata$NP <- factor(bigdata$NP, levels = c("C", "N", "P", "NP"))
bigdata$logGS <- log(bigdata$GS)

library(mosaic)
#Edit the data to standardize MAT/MAP
bigdata <- mutate(bigdata, MAT_std = scale(MAT_v2))
bigdata <- mutate(bigdata, MAP_std = scale(MAP_v2))

# Get rid of 1 damage outlier; Has 84% damage
library(dplyr)
bigdata2 <- bigdata %>% filter(TOTALdamage < 75) #856 observations of 17 variables
bigdata2 <- mutate(bigdata2, SQRTinsectWP = sqrt(Insect_TOTAL_WP))
bigdata2 <- mutate(bigdata2, LOGfungal = log(Fungal_TOTAL_WP))
bigdata2[is.na(bigdata2) | bigdata2 == "-Inf"] <- NA #"Inf" values in logFungal, so making them NAs

bigdata2 <- mutate(bigdata2, SQRTtotal = sqrt(TOTALdamage)) #563 obs
bigdata_forb <- bigdata %>% filter(Lifeform== "FORB")

#Get the vcv matrix up 
A <- ape:::vcv.phylo(phylogeny)

'### If you get the "compilecode" error use the below code ###
remove.packages(c("StanHeaders","rstan"))
install.packages(c("StanHeaders","rstan"),type="source")

## Or this one ##
install.packages("StanHeaders", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
install.packages("rstan", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))'

#INDIVIDUAL FORB MODELS (15 PLUS ANOTHER WITH SITE AS RANDOM)
bigdata_forb <- mutate(bigdata2, SQRTinsect = Insect_TOTAL_WP)
library(brms)

#### FUNGAL DAMAGE ####
forbA <- brm(
  LOGfungal ~ logGS,
  data = bigdata_forb,
  family = gaussian(),
  data2 = list(A = A),
  prior = c(prior(normal(0, 1), "b")),
  cores = getOption("mc.cores", 3),
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))

summary(forbA)
WAIC(forbA)

forbB <- brm(
  LOGfungal ~ Treatment,
  data = bigdata_forb,
  family = gaussian(),
  data2 = list(A = A),
  prior = c(prior(normal(0, 1), "b")),
  cores = getOption("mc.cores", 3),
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))

summary(forbB)
WAIC(forbB)

forbC <- brm(
  LOGfungal ~ MAT_std,
  data = bigdata_forb,
  family = gaussian(),
  data2 = list(A = A),
  prior = c(prior(normal(0, 1), "b")),
  cores = getOption("mc.cores", 3),
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))

summary(forbC)
WAIC(forbC)

forbD <- brm(
  LOGfungal ~ MAP_std,
  data = bigdata_forb,
  family = gaussian(),
  data2 = list(A = A),
  prior = c(prior(normal(0, 1), "b")),
  cores = getOption("mc.cores", 3),
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))

summary(forbD)
WAIC(forbD)

forbE <- brm(
  LOGfungal ~ logGS + Treatment + logGS*Treatment,
  data = bigdata_forb,
  family = gaussian(),
  data2 = list(A = A),
  prior = c(prior(normal(0, 1), "b")),
  cores = getOption("mc.cores", 3),
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))

summary(forbE)
WAIC(forbE)

forbF <- brm(
  LOGfungal ~ MAT_std + MAP_std + MAT_std*MAP_std,
  data = bigdata_forb,
  family = gaussian(),
  data2 = list(A = A),
  prior = c(prior(normal(0, 1), "b")),
  cores = getOption("mc.cores", 3),
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))

summary(forbF)
WAIC(forbF)

forbG <- brm(
  LOGfungal ~ logGS + MAT_std + MAT_std*logGS,
  data = bigdata_forb,
  family = gaussian(),
  data2 = list(A = A),
  prior = c(prior(normal(0, 1), "b")),
  cores = getOption("mc.cores", 3),
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))

summary(forbG)
WAIC(forbG)

forbH <- brm(
  LOGfungal ~ logGS + MAP_std + MAP_std*logGS,
  data = bigdata_forb,
  family = gaussian(),
  data2 = list(A = A),
  prior = c(prior(normal(0, 1), "b")),
  cores = getOption("mc.cores", 3),
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))

summary(forbH)
WAIC(forbH)

forbI <- brm(
  LOGfungal ~ Treatment + MAP_std + MAP_std*Treatment,
  data = bigdata_forb,
  family = gaussian(),
  data2 = list(A = A),
  prior = c(prior(normal(0, 1), "b")),
  cores = getOption("mc.cores", 3),
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))

summary(forbI)
WAIC(forbI)

forbJ <- brm(
  LOGfungal ~ Treatment + MAT_std + MAT_std*Treatment,
  data = bigdata_forb,
  family = gaussian(),
  data2 = list(A = A),
  prior = c(prior(normal(0, 1), "b")),
  cores = getOption("mc.cores", 3),
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))

summary(forbJ)
WAIC(forbJ)

forbK <- brm(
  LOGfungal ~ Treatment + MAP_std + MAT_std+ MAP_std*Treatment + MAT_std*Treatment + Treatment*MAP_std+MAT_std,
  data = bigdata_forb,
  family = gaussian(),
  data2 = list(A = A),
  prior = c(prior(normal(0, 1), "b")),
  cores = getOption("mc.cores", 3),
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))

summary(forbK)
WAIC(forbK)

forbL <- brm(
  LOGfungal ~ logGS + MAP_std + MAT_std+ MAP_std*logGS + MAT_std*logGS + logGS*MAP_std+MAT_std,
  data = bigdata_forb,
  family = gaussian(),
  data2 = list(A = A),
  prior = c(prior(normal(0, 1), "b")),
  cores = getOption("mc.cores", 3),
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))

summary(forbL)
WAIC(forbL)

forbM <- brm(
  LOGfungal ~ logGS + Treatment + MAT_std + logGS*Treatment + logGS*MAT_std + Treatment*MAT_std + logGS*Treatment*MAT_std,
  data = bigdata_forb,
  family = gaussian(),
  data2 = list(A = A),
  prior = c(prior(normal(0, 1), "b")),
  cores = getOption("mc.cores", 3),
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))

summary(forbM)
WAIC(forbM)

forbM <- brm(
  LOGfungal ~ logGS + Treatment + MAP_std + logGS*Treatment + logGS*MAP_std + Treatment*MAP_std + logGS*Treatment*MAP_std,
  data = bigdata_forb,
  family = gaussian(),
  data2 = list(A = A),
  prior = c(prior(normal(0, 1), "b")),
  cores = getOption("mc.cores", 3),
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))

summary(forbM)
WAIC(forbM)

forbN <- brm(
  LOGfungal ~ logGS + Treatment + MAP_std + 
    logGS*Treatment + logGS*MAP_std + Treatment*MAP_std + 
    logGS*Treatment*MAP_std +MAT_std + logGS*MAT_std + Treatment*MAT_std +
    MAT_std + MAP_std + MAT_std*MAP_std + MAT_std*logGS*Treatment*MAP_std,
  data = bigdata_forb,
  family = gaussian(),
  data2 = list(A = A),
  prior = c(prior(normal(0, 1), "b")),
  cores = getOption("mc.cores", 3),
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))

summary(forbN)
WAIC(forbN)

#Best fungal model based on WAIC
forbBM <- brm(
  LOGfungal ~ MAT_std + MAP_std + MAT_std*MAP_std + (1|Site),
  data = bigdata_forb,
  family = gaussian(),
  data2 = list(A = A),
  prior = c(prior(normal(0, 1), "b")),
  cores = getOption("mc.cores", 3),
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))

summary(forbBM)
WAIC(forbBM)

#### INSECT DAMAGE ####
forb1 <- brm(
  LOGfungal ~ logGS,
  data = bigdata_forb,
  family = gaussian(),
  data2 = list(A = A),
  prior = c(prior(normal(0, 1), "b")),
  cores = getOption("mc.cores", 3),
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))

summary(forb1)
WAIC(forb1)

forb2 <- brm(
  LOGfungal ~ Treatment,
  data = bigdata_forb,
  family = gaussian(),
  data2 = list(A = A),
  prior = c(prior(normal(0, 1), "b")),
  cores = getOption("mc.cores", 3),
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))

summary(forb2)
WAIC(forb2)

forb3 <- brm(
  LOGfungal ~ MAT_std,
  data = bigdata_forb,
  family = gaussian(),
  data2 = list(A = A),
  prior = c(prior(normal(0, 1), "b")),
  cores = getOption("mc.cores", 3),
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))

summary(forb3)
WAIC(forb3)

forb4 <- brm(
  LOGfungal ~ MAP_std,
  data = bigdata_forb,
  family = gaussian(),
  data2 = list(A = A),
  prior = c(prior(normal(0, 1), "b")),
  cores = getOption("mc.cores", 3),
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))

summary(forb4)
WAIC(forb4)

forb5 <- brm(
  LOGfungal ~ logGS + Treatment + logGS*Treatment,
  data = bigdata_forb,
  family = gaussian(),
  data2 = list(A = A),
  prior = c(prior(normal(0, 1), "b")),
  cores = getOption("mc.cores", 3),
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))

summary(forb5)
WAIC(forb5)

forb6 <- brm(
  LOGfungal ~ MAT_std + MAP_std + MAT_std*MAP_std,
  data = bigdata_forb,
  family = gaussian(),
  data2 = list(A = A),
  prior = c(prior(normal(0, 1), "b")),
  cores = getOption("mc.cores", 3),
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))

summary(forb6)
WAIC(forb6)

forb7 <- brm(
  LOGfungal ~ logGS + MAT_std + MAT_std*logGS,
  data = bigdata_forb,
  family = gaussian(),
  data2 = list(A = A),
  prior = c(prior(normal(0, 1), "b")),
  cores = getOption("mc.cores", 3),
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))

summary(forb7)
WAIC(forb7)

forb8 <- brm(
  LOGfungal ~ logGS + MAP_std + MAP_std*logGS,
  data = bigdata_forb,
  family = gaussian(),
  data2 = list(A = A),
  prior = c(prior(normal(0, 1), "b")),
  cores = getOption("mc.cores", 3),
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))

summary(forb8)
WAIC(forb8)

forb9 <- brm(
  LOGfungal ~ Treatment + MAP_std + MAP_std*Treatment,
  data = bigdata_forb,
  family = gaussian(),
  data2 = list(A = A),
  prior = c(prior(normal(0, 1), "b")),
  cores = getOption("mc.cores", 3),
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))

summary(forb9)
WAIC(forb9)

forb10 <- brm(
  LOGfungal ~ Treatment + MAT_std + MAT_std*Treatment,
  data = bigdata_forb,
  family = gaussian(),
  data2 = list(A = A),
  prior = c(prior(normal(0, 1), "b")),
  cores = getOption("mc.cores", 3),
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))

summary(forb10)
WAIC(forb10)

forb11 <- brm(
  LOGfungal ~ Treatment + MAP_std + MAT_std+ MAP_std*Treatment + MAT_std*Treatment + Treatment*MAP_std+MAT_std,
  data = bigdata_forb,
  family = gaussian(),
  data2 = list(A = A),
  prior = c(prior(normal(0, 1), "b")),
  cores = getOption("mc.cores", 3),
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))

summary(forb11)
WAIC(forb11)

forb12 <- brm(
  LOGfungal ~ logGS + MAP_std + MAT_std+ MAP_std*logGS + MAT_std*logGS + logGS*MAP_std+MAT_std,
  data = bigdata_forb,
  family = gaussian(),
  data2 = list(A = A),
  prior = c(prior(normal(0, 1), "b")),
  cores = getOption("mc.cores", 3),
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))

summary(forb12)
WAIC(forb12)

forb13 <- brm(
  LOGfungal ~ logGS + Treatment + MAT_std + logGS*Treatment + logGS*MAT_std + Treatment*MAT_std + logGS*Treatment*MAT_std,
  data = bigdata_forb,
  family = gaussian(),
  data2 = list(A = A),
  prior = c(prior(normal(0, 1), "b")),
  cores = getOption("mc.cores", 3),
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))

summary(forb13)
WAIC(forb13)

forb14 <- brm(
  LOGfungal ~ logGS + Treatment + MAP_std + logGS*Treatment + logGS*MAP_std + Treatment*MAP_std + logGS*Treatment*MAP_std,
  data = bigdata_forb,
  family = gaussian(),
  data2 = list(A = A),
  prior = c(prior(normal(0, 1), "b")),
  cores = getOption("mc.cores", 3),
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))

summary(forb14)
WAIC(forb14)

forb15 <- brm(
  LOGfungal ~ logGS + Treatment + MAP_std + 
    logGS*Treatment + logGS*MAP_std + Treatment*MAP_std + 
    logGS*Treatment*MAP_std +MAT_std + logGS*MAT_std + Treatment*MAT_std +
    MAT_std + MAP_std + MAT_std*MAP_std + MAT_std*logGS*Treatment*MAP_std,
  data = bigdata_forb,
  family = gaussian(),
  data2 = list(A = A),
  prior = c(prior(normal(0, 1), "b")),
  cores = getOption("mc.cores", 3),
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))

summary(forb15)
WAIC(forb15)

#### Best model for forb insect damage ####
forbBM2 <- brm(
  SQRTinsectWP ~ MAT_std + MAP_std + MAT_std*MAP_std + (1|Site),
  data = bigdata_forb,
  family = gaussian(),
  data2 = list(A = A),
  prior = c(prior(normal(0, 1), "b")),
  cores = getOption("mc.cores", 3),
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))

summary(forbBM2)
WAIC(forbBM2)



#### Plant traits and herbivory for forbs

brms_forb_insect <- brm(
  SQRTinsectWP ~ Height + Trichomes + Phenology + FoliarN + CN_Ratio + AvgSLA + logGS,
  data = traits_forb,
  family = gaussian(),
  data2 = list(A = A),
  prior = c(prior(normal(0, 1), "b")),
  cores = getOption("mc.cores", 3),
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))

summary(brms_forb_insect)
WAIC(brms_forb_insect)
loo(brms_forb_insect)


#### Plant traits and fungal pathogen for forbs
traits_forb[is.infinite(traits_forb) <- NA


brms_forb_fungal <- brm(
  logFungalWP ~ Height + Trichomes + Phenology + FoliarN + CN_Ratio + AvgSLA + logGS,
  data = forbdata,
  family = gaussian(),
  data2 = list(A = A),
  prior = c(prior(normal(0, 1), "b")),
  cores = getOption("mc.cores", 3),
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))

summary(brms_forb_fungal)
WAIC(brms_forb_fungal)
loo(brms_forb_fungal)




############################ YELLOW MODELS #####################################
#logGS + Treatment + logGS*Treatment
library(rstan)
library(brms)

# Yellow- TOTAL DAMAGE, all FG
yellow_total_brms <- brm(
  SQRTtotal ~ logGS + Treatment + logGS*Treatment,    #Fixed effects
  data = bigdata2,                           #Data source; This excludes the one high damage observation
  family = gaussian(),                       #Distribution of data
  data2 = list(A = A),                       #Source of variance-covariance matrix for phylogeny
  prior = c(prior(normal(0, 1), "b")),       #Define any priors for the model
  cores = getOption("mc.cores", 3),          #Run on 3 cores to speed up models
  control = list(adapt_delta = 0.9,
                  max_treedepth = 15))       #Set based on preliminary runs to ensure convergence

summary(yellow_total_brms) #View model output
WAIC(yellow_total_brms)
loo(yellow_total_brms)

'#View posterior distributions and check for convergence (caterpillar plots)
plot(full_brms, N = 2, ask = TRUE) #More than 7 plots. Take a minute to load      
#View model outputs graphically
plot(conditional_effects(full_brms), points = TRUE) #Lots of good plots here
#Computing the phylogenetic signal for this model (Model 1)
hyp <- "sd_phylo__Intercept^2 / (sd_phylo__Intercept^2 + sigma^2) = 0"
(hyp <- hypothesis(full_brms, hyp, class = NULL))
plot(hyp)'

# Yellow- INSECT damage, all FG
yellow_insect_brms <- brm(
  SQRTinsectWP ~ logGS + Treatment + logGS*Treatment,    
  data = bigdata2,                         
  family = gaussian(),                       
  data2 = list(A = A),                       
  prior = c(prior(normal(0, 1), "b")),   
  cores = getOption("mc.cores", 3),       
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))

summary(yellow_insect_brms) #View model output
WAIC(yellow_insect_brms)
loo(yellow_insect_brms)


# Yellow- FUNGAL damage, all FG
yellow_fungal_brms <- brm(
  LOGfungal ~ logGS + Treatment + logGS*Treatment,    
  data = bigdata2,                         
  family = gaussian(),                       
  data2 = list(A = A),                       
  prior = c(prior(normal(0, 1), "b")),   
  cores = getOption("mc.cores", 3),       
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))

summary(yellow_fungal_brms) #View model output
WAIC(yellow_fungal_brms)
loo(yellow_fungal_brms)

############################# GREEN MODELS #####################################
#logGS + Treatment + logGS*Treatment + MAT_std + MAT_std*logGS

# Green- TOTAL damage, all FG
green_total_brms <- brm(
  SQRTtotal ~ logGS + Treatment + logGS*Treatment + MAT_std + MAT_std*logGS,    
  data = bigdata2,                         
  family = gaussian(),                       
  data2 = list(A = A),                       
  prior = c(prior(normal(0, 1), "b")),   
  cores = getOption("mc.cores", 3),       
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))

summary(green_total_brms)
WAIC(green_total_brms)
loo(green_total_brms)

# Green- INSECT damage, all FG
green_insect_brms <- brm(
  SQRTinsectWP ~ logGS + Treatment + logGS*Treatment + MAT_std + MAT_std*logGS,    
  data = bigdata2,                         
  family = gaussian(),                       
  data2 = list(A = A),                       
  prior = c(prior(normal(0, 1), "b")),   
  cores = getOption("mc.cores", 3),       
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))

summary(green_insect_brms)
WAIC(green_insect_brms)
loo(green_insect_brms)

# Green- FUNGAL damage, all FG
green_fungal_brms <- brm(
  LOGfungal ~ logGS + Treatment + logGS*Treatment + MAT_std + MAT_std*logGS,    
  data = bigdata2,                         
  family = gaussian(),                       
  data2 = list(A = A),                       
  prior = c(prior(normal(0, 1), "b")),   
  cores = getOption("mc.cores", 3),       
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))

summary(green_fungal_brms)
WAIC(green_fungal_brms)
loo(green_fungal_brms)

############################# BLUE MODELS #####################################
#logGS + Treatment + logGS*Treatment + MAP_std + MAP_std*logGS

# Blue- TOTAL damage, all FG
blue_total_brms <- brm(
  SQRTtotal ~ logGS + Treatment + logGS*Treatment + MAP_std + MAP_std*logGS,    
  data = bigdata2,                         
  family = gaussian(),                       
  data2 = list(A = A),                       
  prior = c(prior(normal(0, 1), "b")),   
  cores = getOption("mc.cores", 3),       
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))

summary(blue_total_brms)
WAIC(blue_total_brms)
loo(blue_total_brms)

# Blue- INSECT damage, all FG
blue_insect_brms <- brm(
  SQRTinsectWP ~ logGS + Treatment + logGS*Treatment + MAP_std + MAP_std*logGS,    
  data = bigdata2,                         
  family = gaussian(),                       
  data2 = list(A = A),                       
  prior = c(prior(normal(0, 1), "b")),   
  cores = getOption("mc.cores", 3),       
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))

summary(blue_insect_brms)
WAIC(blue_insect_brms)
loo(blue_insect_brms)

# Blue- FUNGAL damage, all FG
blue_fungal_brms <- brm(
  LOGfungal ~ logGS + Treatment + logGS*Treatment + MAP_std + MAP_std*logGS,    
  data = bigdata2,                         
  family = gaussian(),                       
  data2 = list(A = A),                       
  prior = c(prior(normal(0, 1), "b")),   
  cores = getOption("mc.cores", 3),       
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))

summary(blue_fungal_brms)
WAIC(blue_fungal_brms)
loo(blue_fungal_brms)

############################# MAGENTA MODELS #####################################
#logGS + Treatment + logGS*Treatment + MAP_std + MAT_std + MAT_std*logGS + MAP_std*logGS

# Magenta- TOTAL damage, all FG
magenta_total_brms <- brm(
  SQRTtotal ~ logGS + Treatment + logGS*Treatment + MAP_std + MAT_std + 
    MAT_std*logGS + MAP_std*logGS,    
  data = bigdata2,                         
  family = gaussian(),                       
  data2 = list(A = A),                       
  prior = c(prior(normal(0, 1), "b")),   
  cores = getOption("mc.cores", 3),       
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))

summary(magenta_total_brms)
WAIC(magenta_total_brms)
loo(magenta_total_brms)

# Magenta- INSECT damage, all FG
magenta_insect_brms <- brm(
  SQRTinsectWP ~ logGS + Treatment + logGS*Treatment + MAP_std + MAT_std + 
    MAT_std*logGS + MAP_std*logGS,    
  data = bigdata2,                         
  family = gaussian(),                       
  data2 = list(A = A),                       
  prior = c(prior(normal(0, 1), "b")),   
  cores = getOption("mc.cores", 3),       
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))

summary(magenta_insect_brms)
WAIC(magenta_insect_brms)
loo(magenta_insect_brms)

plot(conditional_effects(magenta_insect_brms), points = TRUE)
ggplot(data = bigdata2) + geom_point(mapping= aes(x = MAT_std*logGS, y = SQRTinsectWP))


# Magenta- FUNGAL damage, all FG
magenta_fungal_brms <- brm(
  LOGfungal ~ logGS + Treatment + logGS*Treatment + MAP_std + MAT_std +
    MAP_std*logGS + MAT_std*logGS,    
  data = bigdata2,                         
  family = gaussian(),                       
  data2 = list(A = A),                       
  prior = c(prior(normal(0, 1), "b")),   
  cores = getOption("mc.cores", 3),       
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))

summary(magenta_fungal_brms)
WAIC(magenta_fungal_brms)
loo(magenta_fungal_brms)

########################## FUNCTIONAL GROUP MODELS #############################
### Edit the larger "bigdata" set to get individual forb, grass, legume data

###################### FORB DATA FIX ############################
forbdata <- read.csv("CAREER_NUT_HERB_11MAY.csv") #880 observations of 12 variables

#Remove odd duplicates and species with no GS data
forbdata <- unique(forbdata[!is.na(forbdata$GS),]) #857 observations; 23 excluded

#Reformat factor levels for nutrients
forbdata$NP <- factor(forbdata$NP, levels = c("C", "N", "P", "NP"))
forbdata$logGS <- log(forbdata$GS)
forbdata <- mutate(forbdata, MAT_std = scale(MAT_v2))
forbdata <- mutate(forbdata, MAP_std = scale(MAP_v2))

# Get rid of 1 damage outlier; Has 84% damage
library(dplyr)
forbdata <- forbdata %>% filter(TOTALdamage < 75) %>% filter(Lifeform== "FORB") #562 observations of 12 variables for forbs
forbdata <- mutate(forbdata, SQRTinsectWP = sqrt(Insect_TOTAL_WP))
forbdata <- mutate(forbdata, LOGfungal = log(Fungal_TOTAL_WP))
forbdata[is.na(forbdata) | forbdata == "-Inf"] <- NA #"Inf" values in logFungal, so making them NAs

forbdata <- mutate(forbdata, SQRTtotal = sqrt(TOTALdamage))
# forb data has 562 obs of 18 variables

################################# YELLOW FORB MODELS ###########################
#logGS + Treatment + logGS*Treatment
library(rstan)
library(brms)

# Yellow- TOTAL DAMAGE, FORB
yellow_totalFORB_brms <- brm(
  SQRTtotal ~ logGS + Treatment + logGS*Treatment,    #Fixed effects
  data = forbdata,                           #Data source; This excludes the one high damage observation
  family = gaussian(),                       #Distribution of data
  data2 = list(A = A),                       #Source of variance-covariance matrix for phylogeny
  prior = c(prior(normal(0, 1), "b")),       #Define any priors for the model
  cores = getOption("mc.cores", 3),          #Run on 3 cores to speed up models
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))       #Set based on preliminary runs to ensure convergence

summary(yellow_totalFORB_brms) #View model output
WAIC(yellow_totalFORB_brms)
loo(yellow_totalFORB_brms)

# Yellow- INSECT damage, FORB
yellow_insectFORB_brms <- brm(
  SQRTinsectWP ~ logGS + Treatment + logGS*Treatment,    
  data = forbdata,                         
  family = gaussian(),                       
  data2 = list(A = A),                       
  prior = c(prior(normal(0, 1), "b")),   
  cores = getOption("mc.cores", 3),       
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))

summary(yellow_insectFORB_brms) #View model output
WAIC(yellow_insectFORB_brms)
loo(yellow_insectFORB_brms)

# Yellow- FUNGAL damage, FORB
yellow_fungalFORB_brms <- brm(
  LOGfungal ~ logGS + Treatment + logGS*Treatment,    
  data = forbdata,                         
  family = gaussian(),                       
  data2 = list(A = A),                       
  prior = c(prior(normal(0, 1), "b")),   
  cores = getOption("mc.cores", 3),       
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))

summary(yellow_fungalFORB_brms) #View model output
WAIC(yellow_fungalFORB_brms)
loo(yellow_fungalFORB_brms)

############################# GREEN FORB MODELS ################################
#logGS + Treatment + logGS*Treatment + MAT_std + MAT_std*logGS

# Green- TOTAL damage, FORB
green_totalFORB_brms <- brm(
  SQRTtotal ~ logGS + Treatment + logGS*Treatment + MAT_std + MAT_std*logGS,    
  data = forbdata,                         
  family = gaussian(),                       
  data2 = list(A = A),                       
  prior = c(prior(normal(0, 1), "b")),   
  cores = getOption("mc.cores", 3),       
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))

summary(green_totalFORB_brms)
WAIC(green_totalFORB_brms)
loo(green_totalFORB_brms)

# Green- INSECT damage, FORB
green_insectFORB_brms <- brm(
  SQRTinsectWP ~ logGS + Treatment + logGS*Treatment + MAT_std + MAT_std*logGS,    
  data = forbdata,                         
  family = gaussian(),                       
  data2 = list(A = A),                       
  prior = c(prior(normal(0, 1), "b")),   
  cores = getOption("mc.cores", 3),       
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))

summary(green_insectFORB_brms)
WAIC(green_insectFORB_brms)
loo(green_insectFORB_brms)

# Green- FUNGAL damage, FORB
green_fungalFORB_brms <- brm(
  LOGfungal ~ logGS + Treatment + logGS*Treatment + MAT_std + MAT_std*logGS,    
  data = forbdata,                         
  family = gaussian(),                       
  data2 = list(A = A),                       
  prior = c(prior(normal(0, 1), "b")),   
  cores = getOption("mc.cores", 3),       
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))

summary(green_fungalFORB_brms)
WAIC(green_fungalFORB_brms)
loo(green_fungalFORB_brms)

############################# BLUE FORB MODELS ################################
#logGS + Treatment + logGS*Treatment + MAP_std + MAP_std*logGS

# Blue- TOTAL damage, FORB
blue_totalFORB_brms <- brm(
  SQRTtotal ~ logGS + Treatment + logGS*Treatment + MAP_std + MAP_std*logGS,    
  data = forbdata,                         
  family = gaussian(),                       
  data2 = list(A = A),                       
  prior = c(prior(normal(0, 1), "b")),   
  cores = getOption("mc.cores", 3),       
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))

summary(blue_totalFORB_brms)
WAIC(blue_totalFORB_brms)
loo(blue_totalFORB_brms)

# Blue- INSECT damage, FORB
blue_insectFORB_brms <- brm(
  SQRTinsectWP ~ logGS + Treatment + logGS*Treatment + MAP_std + MAP_std*logGS,    
  data = forbdata,                         
  family = gaussian(),                       
  data2 = list(A = A),                       
  prior = c(prior(normal(0, 1), "b")),   
  cores = getOption("mc.cores", 3),       
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))

summary(blue_insectFORB_brms)
WAIC(blue_insectFORB_brms)
loo(blue_insectFORB_brms)

# Blue- FUNGAL damage, FORB
blue_fungalFORB_brms <- brm(
  LOGfungal ~ logGS + Treatment + logGS*Treatment + MAP_std + MAP_std*logGS,    
  data = forbdata,                         
  family = gaussian(),                       
  data2 = list(A = A),                       
  prior = c(prior(normal(0, 1), "b")),   
  cores = getOption("mc.cores", 3),       
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))

summary(blue_fungalFORB_brms)
WAIC(blue_fungalFORB_brms)
loo(blue_fungalFORB_brms)

############################# MAGENTA FORB MODELS ################################
#logGS + Treatment + logGS*Treatment + MAP_std + MAT_std + MAT_std*logGS + MAP_std*logGS

# Blue- TOTAL damage, FORB
magenta_totalFORB_brms <- brm(
  SQRTtotal ~ logGS + Treatment + logGS*Treatment + MAP_std + MAT_std+ 
    MAT_std*logGS + MAP_std*logGS,    
  data = forbdata,                         
  family = gaussian(),                       
  data2 = list(A = A),                       
  prior = c(prior(normal(0, 1), "b")),   
  cores = getOption("mc.cores", 3),       
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))

summary(magenta_totalFORB_brms)
WAIC(magenta_totalFORB_brms)
loo(magenta_totalFORB_brms)

# Magenta- INSECT damage, FORB
magenta_insectFORB_brms <- brm(
  SQRTinsectWP ~ logGS + Treatment + logGS*Treatment + MAP_std + MAT_std + 
    MAT_std*logGS + MAP_std*logGS,    
  data = forbdata,                         
  family = gaussian(),                       
  data2 = list(A = A),                       
  prior = c(prior(normal(0, 1), "b")),   
  cores = getOption("mc.cores", 3),       
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))

summary(magenta_insectFORB_brms)
WAIC(magenta_insectFORB_brms)
loo(magenta_insectFORB_brms)

# Magenta- FUNGAL damage, FORB
magenta_fungalFORB_brms <- brm(
  LOGfungal ~ logGS + Treatment + logGS*Treatment + MAP_std + MAT_std + 
    MAT_std*logGS + MAP_std*logGS,    
  data = forbdata,                         
  family = gaussian(),                       
  data2 = list(A = A),                       
  prior = c(prior(normal(0, 1), "b")),   
  cores = getOption("mc.cores", 3),       
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))

summary(magenta_fungalFORB_brms)
WAIC(magenta_fungalFORB_brms)
loo(magenta_fungalFORB_brms)


# Not running the below code as we are only doing brms on functional groups with a phylogenetic signal
# grasses and legumes did not have a significant phylogenetic signal, so we only ran brms for forbs

'###################### GRASS DATA FIX ############################
library(dplyr)
grassdata <- read.csv("CAREER_NUT_HERB_11MAY.csv") #880 observations of 12 variables

#Remove odd duplicates and species with no GS data
grassdata <- unique(grassdata[!is.na(grassdata$GS),]) #857 observations; 23 excluded

#Reformat factor levels for nutrients
grassdata$NP <- factor(grassdata$NP, levels = c("C", "N", "P", "NP"))
grassdata$logGS <- log(grassdata$GS)
grassdata <- mutate(grassdata, MAT = MAT_v2)
grassdata <- mutate(grassdata, MAP = MAP_v2)

# Get rid of 1 damage outlier; Has 84% damage
grassdata <- grassdata %>% filter(TOTALdamage < 75) %>% filter(Lifeform== "GRASS") #203 observations of 12 variables for GRASSES


###################### LEGUME DATA FIX ############################
library(dplyr)
legumedata <- read.csv("CAREER_NUT_HERB_11MAY.csv") #880 observations of 12 variables

#Remove odd duplicates and species with no GS data
legumedata <- unique(legumedata[!is.na(legumedata$GS),]) #857 observations; 23 excluded

#Reformat factor levels for nutrients
legumedata$NP <- factor(legumedata$NP, levels = c("C", "N", "P", "NP"))
legumedata$logGS <- log(legumedata$GS)
legumedata <- mutate(legumedata, MAT = MAT_v2)
legumedata <- mutate(legumedata, MAP = MAP_v2)

# Get rid of 1 damage outlier; Has 84% damage
legumedata <- legumedata %>% filter(TOTALdamage < 75) %>% filter(Lifeform== "LEGUME") # 63 observations of 12 variables for LEGUMES

#Get the vcv matrix up 
A <- ape:::vcv.phylo(phylogeny) #Already set up in prior codes

################################################################################
######################### INDIVIDUAL PHYLOGENY MODELS ##########################
#Use below code if you get an "invalid connection" error
remove.packages(c("StanHeaders", "rstan"))
install.packages("rstan", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))'

'### MODEL 4 ###
########################## FORB BRMS MODEL #####################################
library(rstan)
library(brms)

forbTOTAL_brms <- brm(
  TOTALdamage ~ logGS + Treatment + logGS*Treatment,   #Fixed effects
  data = forbdata,                               #Data source; This excludes the one high damage observation
  family = gaussian(),                           #Distribution of data
  data2 = list(A = A),                           #Source of variance-covariance matrix for phylogeny
  prior = c(prior(normal(0, 1), "b")),           #Define any priors for the model
  cores = getOption("mc.cores", 3),              #Run on 3 cores to speed up models
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))  

summary(forbTOTAL_brms)

# Interpret FORB MODEL here:

### MODEL 5 ###
########################## GRASS BRMS MODEL #####################################
library(rstan)
library(brms)

grassTOTAL_brms <- brm(
  TOTALdamage ~ logGS + Treatment + logGS*Treatment,   #Fixed effects
  data = grassdata,                              #Data source; This excludes the one high damage observation
  family = gaussian(),                           #Distribution of data
  data2 = list(A = A),                           #Source of variance-covariance matrix for phylogeny
  prior = c(prior(normal(0, 1), "b")),           #Define any priors for the model
  cores = getOption("mc.cores", 3),              #Run on 3 cores to speed up models
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))  

summary(grassTOTAL_brms)

# Interpret GRASS MODEL here:

### MODLE 6 ###
########################## LEGUME BRMS MODEL ###################################
library(rstan)
library(brms)

legumeTOTAL_brms <- brm(
  TOTALdamage ~ logGS + Treatment + logGS*Treatment,   #Fixed effects
  data = legumedata,                             #Data source; This excludes the one high damage observation
  family = gaussian(),                           #Distribution of data
  data2 = list(A = A),                           #Source of variance-covariance matrix for phylogeny
  prior = c(prior(normal(0, 1), "b")),           #Define any priors for the model
  cores = getOption("mc.cores", 3),              #Run on 3 cores to speed up models
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))  

summary(legumeTOTAL_brms)

################################################################################
############################## INSECT MODELS####################################
################################################################################
### MODEL 7 ###
########################## FORB-INSECT BRMS MODEL ##############################
library(rstan)
library(brms)

forbinsect_brms <- brm(
  Insect_TOTAL_WP ~ logGS + Treatment + logGS*Treatment,   #Fixed effects
  data = forbdata,                               #Data source; This excludes the one high damage observation
  family = gaussian(),                           #Distribution of data
  data2 = list(A = A),                           #Source of variance-covariance matrix for phylogeny
  prior = c(prior(normal(0, 1), "b")),           #Define any priors for the model
  cores = getOption("mc.cores", 3),              #Run on 3 cores to speed up models
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))  

summary(forbinsect_brms)


### MODEL 8 ###
########################## GRASS-INSECT BRMS MODEL #####################################
library(rstan)
library(brms)

grassinsect_brms <- brm(
  Insect_TOTAL_WP ~ logGS + Treatment + logGS*Treatment,   #Fixed effects
  data = grassdata,                              #Data source; This excludes the one high damage observation
  family = gaussian(),                           #Distribution of data
  data2 = list(A = A),                           #Source of variance-covariance matrix for phylogeny
  prior = c(prior(normal(0, 1), "b")),           #Define any priors for the model
  cores = getOption("mc.cores", 3),              #Run on 3 cores to speed up models
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))  

summary(grassinsect_brms)


### MODEL 9 ###
########################## LEGUME-INSECT BRMS MODEL ###################################
library(rstan)
library(brms)

legumeinsect_brms <- brm(
  Insect_TOTAL_WP ~ logGS + Treatment + logGS*Treatment,   #Fixed effects
  data = legumedata,                             #Data source; This excludes the one high damage observation
  family = gaussian(),                           #Distribution of data
  data2 = list(A = A),                           #Source of variance-covariance matrix for phylogeny
  prior = c(prior(normal(0, 1), "b")),           #Define any priors for the model
  cores = getOption("mc.cores", 3),              #Run on 3 cores to speed up models
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))  

summary(legumeinsect_brms)

################################################################################
############################## FUNGAL MODELS####################################
################################################################################
### MODEL 10 ###
fungal_total_brms <- brm(
  Fungal_TOTAL_WP ~ logGS + Treatment + logGS*Treatment + MAT + MAP + MAT*logGS + MAP*logGS,   #Fixed effects
  data = bigdata2,                               #Data source; This excludes the one high damage observation
  family = gaussian(),                           #Distribution of data
  data2 = list(A = A),                           #Source of variance-covariance matrix for phylogeny
  prior = c(prior(normal(0, 1), "b")),           #Define any priors for the model
  cores = getOption("mc.cores", 3),              #Run on 3 cores to speed up models
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))  

summary(fungal_total_brms)

### MODEL 11 ###
########################## FORB-FUNGAL BRMS MODEL ##############################
library(rstan)
library(brms)

forbfungal_brms <- brm(
  Fungal_TOTAL_WP ~ logGS + Treatment + logGS*Treatment + MAT + MAP + MAT*logGS + MAP*logGS,   #Fixed effects
  data = forbdata,                               #Data source; This excludes the one high damage observation
  family = gaussian(),                           #Distribution of data
  data2 = list(A = A),                           #Source of variance-covariance matrix for phylogeny
  prior = c(prior(normal(0, 1), "b")),           #Define any priors for the model
  cores = getOption("mc.cores", 3),              #Run on 3 cores to speed up models
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))  

summary(forbfungal_brms)

### MODEL 12 ###
########################## GRASS-FUNGAL BRMS MODEL #####################################
library(rstan)
library(brms)

grassfungal_brms <- brm(
  Fungal_TOTAL_WP ~ logGS + Treatment + logGS*Treatment + MAT + MAP + MAT*logGS + MAP*logGS,   #Fixed effects
  data = grassdata,                              #Data source; This excludes the one high damage observation
  family = gaussian(),                           #Distribution of data
  data2 = list(A = A),                           #Source of variance-covariance matrix for phylogeny
  prior = c(prior(normal(0, 1), "b")),           #Define any priors for the model
  cores = getOption("mc.cores", 3),              #Run on 3 cores to speed up models
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))  

summary(grassfungal_brms)

### MODLE 13 ###
########################## LEGUME-FUNGAL BRMS MODEL ###################################
library(rstan)
library(brms)

legumefungal_brms <- brm(
  Fungal_TOTAL_WP ~ logGS + Treatment + logGS*Treatment + MAT + MAP + MAT*logGS + MAP*logGS,   #Fixed effects
  data = legumedata,                             #Data source; This excludes the one high damage observation
  family = gaussian(),                           #Distribution of data
  data2 = list(A = A),                           #Source of variance-covariance matrix for phylogeny
  prior = c(prior(normal(0, 1), "b")),           #Define any priors for the model
  cores = getOption("mc.cores", 3),              #Run on 3 cores to speed up models
  control = list(adapt_delta = 0.9,
                 max_treedepth = 15))  

summary(legumefungal_brms)


# Plotting the models (need to change names of individual models before running)
plot(forb_brms, N = 2, ask = FALSE) #Will plot all posterior model plots without asking

plot(conditional_effects(forb_brms), points = TRUE) # Plots models but asks each time

#Computing lambda (AKA the phylogenetic signal) with hypothesis method
hyp <- "sd_total.phylo_Intercept^2 / (sd_total.phylo_Intercept^2 + sigma^2)= 0"
(hyp <- hypothesis(model_simple, hyp, class= NULL))
plot(hyp)'