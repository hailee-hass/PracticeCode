# Script includes pruning a larger phylogenetic tree into three trees, AND testing for phylogenetic signal of these trees. 

# Pruning the small tree (from larger NutNet tree) into three separate trees. All lifeforms, grasses, forbs (to include forbs and legumes)
# This will be used for phylogenetic signal analysis

# Set working directory to "Phylogeny" folder and double check using getwd. #
getwd()

# Load packages 
library(ape)
library(phytools)
library(dplyr)

# Load in mega tree
NutNet_tree <- read.tree("NutNet Tree.tre")

# Load in your species lists
all_species <- read.csv("SpeciesList_ALL_April2024.csv") #101 obs of 2 variables

#Create sp_list as a character string, not a table, so it can be matched against the tip label character string
sp_list_all <-  all_species$Taxon # Finally worked as a string... Need the i in front of Taxon

# Match species in your list to tips on the NutNet tree 
tip_labels = lapply( 1:length(sp_list_all), 
                     function( x ) grep( paste(sp_list_all[[x]], collapse='.+' ),
                                         NutNet_tree$tip.label, ignore.case=T ) )

# Work out which species on your list are not in the tree, so you can investigate
not_found = which( sapply( tip_labels, length )==0 )
sp_list_all[not_found] #0 not found (this is a good thing!)

# List of corrections made to "SpeciesList_ALL_April2024.csv":
  # Mimosa roemeriana changed to closely-related species of Mimosa quadrivalvis
  # Dicanthelium oligosanthes changed to -> Panicum_oligosanthes 
  # Sporobolus albus = EXCLUDED (not in NutNet tree)
  # Chrysanthemum leucanthemum -> Chrysanthemum indicum 
  # Silene latifolia ssp. Alba -> Silene latifolia
  # Echinaceae pallida -> Echinacea pallida (spelling error)
  # Echinaceae angustifolia -> Echinaceae angustifolia (spelling error)
  # Brickellia eupatroides -> Brickellia eupatorioides (spelling error)
  # Taraxacum officinale -> Taraxacum_campylodes (changed to closely related species)
  # Zizia azurea -> EXCLUDED, NO GS available; Zizia aurea (spelling error)
  # Gallardia pulchella -> Gaillardia pulchella (spelling error)
  # Hieracium caespitosa -> Hieracium caespitosum (spelling error)
  # Monarda sp. = EXCLUDED as it is genus only, and we have two other species
  # Antennaria parlinii = EXCLUDED 
  # Helianthus dayli = EXCLUDED 
  # Rosa arkansana= EXCLUDED (woody)


# To make substitutions (if you want to include species rather than just leaving them out)
#Create a second row in our table with "phylo" names to match to the tree (you can then have the taxon name and phylo name in your 
  # raw data to have the actual name and the name you use for phylogenetic analysis)
all_species$phylo <- all_species$Taxon

# Make sure there are no NAs in the tip labels 
tip_labels <- sapply( tip_labels, function( x ) x[ 1 ] )
not_NA <- !is.na(tip_labels )
tip_labels <- tip_labels[ not_NA ]
NutNet_tree$tip.label[ tip_labels ] <- sp_list_all[ not_NA ]

# Construct the tree - removing all species from the larger tree not in your list
phylogeny_all <- keep.tip(NutNet_tree, tip=tip_labels )

# Save file
write.tree(phylogeny_all, "AllLifeform_tree.tre" )
data_all <- all_species
# Tree for all combined lifeforms= pruned!

#################################### GRASS #####################################
# Load in grass species list
grass_species <- read.csv("SpeciesList_GRASS_April2024.csv") 

#Create sp_list as a character string, not a table, so it can be matched against the tip label character string
sp_list_grass <-  grass_species$Taxon # Finally worked as a string... Need the i in front of Taxon

# Match species in your list to tips on the NutNet tree
tip_labels = lapply( 1:length(sp_list_grass), 
                     function(x) grep(paste(sp_list_grass[[x]], collapse='.+'),
                                      NutNet_tree$tip.label, ignore.case=T))

# Work out which species on your list are not in the tree, so you can investigate
not_found = which(sapply(tip_labels, length)==0 )
sp_list_grass[not_found] #0 species not found

# Corrections made:
  # Dicanthelium oligosanthes changed to -> Panicum_oligosanthes
  # Sporobolus albus = EXCLUDED (not in NutNet tree)

# To make substitutions (if you want to include species rather than just leaving them out)
#Create a second row in our table with "phylo" names to match to the tree (you can then have the taxon name and phylo name in your 
# raw data to have the actual name and the name you use for phylogenetic analysis)
grass_species$phylo <- grass_species$Taxon

# Make sure there are no NAs in the tip labels 
tip_labels <- sapply(tip_labels, function(x) x[1])
not_NA <- !is.na(tip_labels)
tip_labels <- tip_labels[not_NA]
NutNet_tree$tip.label[ tip_labels] <- sp_list_grass[not_NA]

# Construct the tree - removing all species from the larger tree not in your list
phylogeny_grass <- keep.tip(NutNet_tree, tip=tip_labels )

# Save file
write.tree(phylogeny_grass, "grass_tree.tre" )
data_grass <- grass_species
#grass tree = pruned!

#################################### FORB ######################################
## This includes forbs and legumes
# Load in your species lists
forb_species <- read.csv("SpeciesList_Forb_April2024.csv") 

#Create sp_list as a character string, not a table, so it can be matched against the tip label character string
sp_list_forb <-  forb_species$Taxon # Finally worked as a string... Need the i in front of Taxon

# Match species in your list to tips on the NutNet tree
tip_labels = lapply(1:length(sp_list_forb), 
                    function(x) grep(paste(sp_list_forb[[x]], collapse='.+'),
                                     NutNet_tree$tip.label, ignore.case=T))

# Work out which species on your list are not in the tree, so you can investigate
not_found = which(sapply(tip_labels, length)==0 )
sp_list_forb[not_found] # 0 species not found

# Corrections made:
  # Antennaria_parlinii= EXCLUDED
  # Brickellia eupatroides -> Brickellia eupatorioides (spelling error)
  # Chrysanthemum leucanthemum -> Chrysanthemum indicum
  # Echinaceae angustifolia -> Echinacea angustifolia (spelling error)
  # Echinaceae pallida -> Echinacea pallida (spelling error)
  # Gallardia pulchella -> Gaillardia pulchella (spelling error)
  # Helianthus dayli = EXCLUDED 
  # Hieracium_caespitosa -> Hieracium caespitosum (spelling error)
  # Monarda sp. = EXCLUDED
  # Silene latifolia ssp. Alba -> Silene latifolia
  # Taraxacum officinale -> Taraxacum_campylodes (changed to closely related species)
  # Zizia azurea -> EXCLUDED, NO GS AVAILABLE; Zizia aurea (spelling error)
  # Rosa arkansana = EXCLUDED (woody)
  # Mimosa roemeriana changed to closely-related species of Mimosa quadrivalvis

# To make substitutions (if you want to include species rather than just leaving them out)
#Create a second row in our table with "phylo" names to match to the tree (you can then have the taxon name and phylo name in your 
# raw data to have the actual name and the name you use for phylogenetic analysis)
forb_species$phylo <- forb_species$Taxon

# Make sure there are no NAs in the tip labels 
tip_labels <- sapply(tip_labels, function(x) x[1])
not_NA <- !is.na(tip_labels)
tip_labels <- tip_labels[not_NA]
NutNet_tree$tip.label[ tip_labels] <- sp_list_forb[not_NA]

# Construct the tree - removing all species from the larger tree not in your list
phylogeny_forb <- keep.tip(NutNet_tree, tip=tip_labels )

# Save file
write.tree(phylogeny_forb, "forb_tree.tre" )
data_forb <- forb_species
#Forb tree= pruned!

################## Testing for phylogenetic signal #############################
# Use the "AllLifeform_tree.tre" file for this
# Set and check your working directory; "Phylogeny" file
getwd()

# Load in packages
library(ape)
library(picante)
library(phytools)
library(tidyverse)

# Load the tree and data #
new_tree <- read.tree("AllLifeform_tree.tre")
new_tree # Tree with 101 tips and 99 internal nodes; Rooted, includes branch lengths

# Breaking the tree information apart into tip label, node/branch lengths, etc. 
str(new_tree)
new_tree$edge # Provides a list of beginning and end node #s
new_tree$tip.label # View each individual tip label (taxa) in the tree
new_tree$edge.length # View edge length (length of each edge in relation to the root of the tree)
new_tree$Nnode # Number of internal nodes on the tree (110 internal nodes)

#Building a phylogenetic tree image with R
plot(new_tree, edge.width = 1.0, label.offset = 0.75, 
     cex = 0.25, type = "cladogram") 

# Check to see if tree is binary. 
is.binary(new_tree) #It is not binary

# Check if the tree is rooted
is.rooted(new_tree) # It's rooted.

# Change any zero-length branches to one-ten-thousandth of the tree size (Winternitz, 2016)
new_tree$edge.length[new_tree$edge.length==0] <- max(nodeHeights(new_tree))*1e-4

# Make sure tree tips match the data EXACTLY 
new_tree$tip.label <- gsub(" ", "_", new_tree$tip.label) # Replace white space with _

# We want to test phylogenetic signal using Pagel's lambda
# This has the smallest Type I error, and small Type II errors for phylogenies >20 species
# REF: Munkemuller et al. 2012

#Testing Pagel's lambda and Bloomberg's K
library(phytools)

# IMPORT DATA  
phydata <- read.csv("CAREER_NUT_HERB_FINAL.csv") #841 obs of 110 variables

#Data Transformations
phydata <- mutate(phydata, SQRTinsectWP = sqrt(Insect_TOTAL_WP)) #SQRT insect damage
phydata <- mutate(phydata, LOGfungalWP = log(Fungal_TOTAL_WP +1)) #Log +1 fungal damage
phydata <- mutate(phydata, SQRTtotal = sqrt(Insect_TOTAL_WP)) #SQRT total damage
phydata <- mutate(phydata, logGS = log(GS + 1)) #Log +1 GS

# Rearrange the data so you have separate datasets for forbs and grasses
phydata_grass <- phydata %>% filter(Lifeform2== "Grass") #220 OBSERVATIONS OF 114 VARIABLES
phydata_forb <- phydata %>% filter(Lifeform2== "AllForb") #617 OBSERVATIONS OF 114 VARIABLES
# Will use "phydata" to test for signal in all lifeforms (grasses, all forbs)

########################## PHYLOGENETIC SIGNAL #################################
#### Test for signal in the overall model
# Thanks to Joe Morton for the help here. R didn't like that I had more than one GS value for unique species as they differ at sites

#First lets isolate the species and their respective GS values. The columns we are isolating from "phydata" are Taxa, GS, and logGS
#All functional groups (forbs, grasses)
unique_sp_ALL <- unique(phydata[,c(4, 11, 112)], row.names=1) #605 obs of 3 variables

#Grass
unique_GS_grass <- unique(phydata_grass[,c(4, 11, 114)], row.names=1) #29 obs. of 3 variables

#Forb
unique_GS_forb <- unique(phydata_forb[,c(4, 11, 114)], row.names=1) #108 obs. of 3 variables

#Now create a list of named numbers (the species and the GS/log GS value) as a vector not a table
# ALL FUNCTIONAL GROUPS
GS <- setNames(unique_sp_ALL$GS, unique_sp_ALL$Taxa)
logGS <- setNames(unique_sp_ALL$GS, unique_sp_ALL$Taxa) #Renaming column
logGS <- log(unique_sp_ALL$GS) #log transforming

#GRASS
GS_grass <- setNames(unique_GS_grass$GS, unique_GS_grass$Taxa)
logGS_grass <- setNames(unique_GS_grass$logGS, unique_GS_grass$Taxa)

#FORB
GS_forb <- setNames(unique_GS_forb$GS, unique_GS_forb$Taxa)
logGS_forb <- setNames(unique_GS_forb$logGS, unique_GS_forb$Taxa)

#Now use phylosig with the tree and your vector of named GS values for each lifeform you want to test
############################## Pagel's lambda  ###############################
#ALL FUNCTIONAL GROUPS
phylosig(new_tree, GS, method = "lambda", test = TRUE) 
'phylosig(new_tree, logGS, method = "lambda", test = TRUE) ' # Use this is you want to test logGS signal; We don't need this because we want untransformed GS

#Phylo signal= 0.409344
#p-value= 0.000696413

#GRASS
phylosig(new_tree, GS_grass, method = "lambda", test = TRUE) 

#Phylo signal= 0.520676
#p-value= 0.04167
'grass_tree <- read.tree("grass_tree_3Dec2023.tre")
grass_tree
phylosig(grass_tree, GS_grass, method = "lambda", test = TRUE) '
# The phylogenetic signal is the same in the large tree or the individual grass tree!

#FORB
phylosig(new_tree, GS_forb, method = "lambda", test = TRUE) 
'phylosig(new_tree, logGS_forb, method = "lambda", test = TRUE)' # Again, don't need because looking at un-transformed GS for signal

#Phylo signal= 0.326296
#p-value= 0.0148986

############################### Bloomberg's K  #################################
# Blomberg's K #
# ALL FUNCTIONAL GROUPS
phylosig(new_tree, GS, method = "K", test = T, nsim = 10000)
# signal = 0.0359002
# p-value = 0.0005 (based on 10,000 randomizations)
#This indicates that species are not as closely related as expected under random drift

# GRASS
phylosig(new_tree, GS_grass, method = "K", test = T, nsim= 10000)
# signal = 0.263026
# p-value = 0.0139 (based on 10,000 randomizations)

# FORB
phylosig(new_tree, GS_forb, method = "K", test = T, nsim= 10000)
# signal = 0.0428667
# p-value = 0.0101 (based on 10,000 randomizations)

############ Phylogenetic Generalized Linear Mixed Model (PGLMM) ###############
# See Ch. 4 (https://leanpub.com/correlateddata) for more information/examples
#rm(list=ls()) #Delete all data; Start fresh. May not want to do this if running code consecutively
library(phyr)
library(ape)
library(geiger)
library(nlme)
library(phytools)
library(ggplot2)
library(lme4)
library(dplyr)
library(afex)
library(lmerTest)

#Essentially this is a mixed-effects model with the phylogeny as a covariate argument in the pglmm function
# This function gives you p-values for random effects and is easier to interpret.
#To get r2 values, use the function r.squaredGLMM in the MuMln package

#Set working directory to "R" folder
getwd()

# Exclusions, corrections made to "CAREER_NUT_HERB_4Dec2023_Edited.csv":
  # Polygonum convulvus = EXCLUDED; woody, 3 rows from cdcr.us
  # Sympyocarpos albus= EXCLUDED; woody, 1 row from msum.us

bigdata <- read.csv("CAREER_NUT_HERB_4Dec2023_Edited.csv") #837 obs of 110 variables
bigdata <- unique(bigdata[!is.na(bigdata$GS),])
bigdata$logGS <- log(bigdata$GS +1)

library(mosaic)
library(tidyverse)

# Transform necessary columns
#standardized MAT and MAP are already in the file, no need to transform them here
bigdata <- mutate(bigdata, SQRTinsectWP = sqrt(Insect_TOTAL_WP))
bigdata <- mutate(bigdata, SQRTtotal = sqrt(Total_WP_Damage))
bigdata <- mutate(bigdata, LOGfungal = log(Fungal_TOTAL_WP+1.0))
bigdata <- mutate(bigdata, logGS = log(GS +1))

# Read in data for forb and grass trees
forb_tree <- read.tree("forb_tree_4Dec2023.tre")
grass_tree <- read.tree("grass_tree_3Dec2023.tre")
'legume_tree <- read.tree("legume_tree_15Dec2023.tre")'

forb_data <- filter(bigdata, Lifeform2== "AllForb") #617 observations of 114 variables
grass_data <- filter(bigdata, Lifeform2== "Grass") #220 observations of 114 variables
'legume_data <- filter(bigdata, Lifeform== "LEGUME") #64 observations of 114 variables'

#Create a correlation matrix that explains the expected correlation from root to tip under Brownian motion
  # The expected correlation is proportional to the amount of evolutionary history, from root to tips that these species share through common descent. 
  # Data is essentially re-weighted per species
vv <- vcv(forb_tree, model= "Brownian", corr= TRUE)
species_to_keep <- c("Achillea_millefolium",        "Ambrosia_artemisiifolia",      "Ambrosia_psilostachya", "Ambrosia_trifida",
                     "Apocynum_cannabinum", "Artemisia_ludoviciana", "Asclepias_syriaca", "Asclepias_verticillata", "Berteroa_incana", "Brassica_sp.",           
                     "Brickellia_eupatorioides",   "Centaurea_americana","Centaurea_stoebe",          
                     "Chenopodium_album",  "Chrysanthemum_indicum",   "Clinopodium_vulgare",         
                     "Commelina_erecta",  "Coreopsis_tripteris",  "Daucus_carota",               
                     "Echinacea_angustifolia", "Echinacea_pallida",  "Erigeron_annuus",             
                     "Erigeron_sp.",  "Erigeron_strigosus",    "Eryngium_yuccifolium",        
                     "Euphorbia_dentata",   "Euthamia_graminifolia",     "Fragaria_vesca",              
                     "Galium_aparine",    "Galium_boreale",             
                     "Gaillardia_pulchella"  , "Geranium_carolinianum",   "Helianthus_annuus" ,   "Helianthus_pauciflorus",       
                     "Hieracium_caespitosum",  "Hypericum_perforatum",  "Liatris_aspera" ,             
                     "Liatris_mucronata", "Linum_sulcatum", "Monarda_citriodora", "Monarda_fistulosa" ,         
                     "Oenothera_biennis",   "Oxalis_stricta",   "Physalis_pumila" ,  "Physalis_virginiana",      
                     "Plantago_lanceolata",          "Polytaenia_nuttallii",         "Potentilla_recta",            
                     "Ratibida_pinnata",             "Rudbeckia_hirta",              "Ruellia_humilis"  ,           
                     "Rumex_acetosella",             "Rumex_crispus",                "Salvia_azurea"     ,          
                     "Silene_latifolia",             "Solanum_carolinense",         
                     "Solanum_dimidiatum",           "Solidago_altissima",           "Solidago_canadensis" ,        
                     "Solidago_gigantea",            "Solidago_missouriensis",       "Solidago_nemoralis"   ,       
                     "Solidago_rigida",              "Solidago_speciosa",            "Symphyotrichum_ericoides",    
                     "Symphyotrichum_oblongifolium", "Symphyotrichum_pilosum",     
                     "Taraxacum_campylodes" ,        "Tragopogon_dubius",  "Vernonia_baldwinii",          
                     "Veronica_sp.",  "Viola_sp.", "Amorpha_canescens",   "Baptisia_alba",   "Baptisia_australis", 
                     "Chamaecrista_fasciculata",        "Dalea_candida",      "Dalea_purpurea",
                     "Lespedeza_capitata",         "Mimosa_quadrivalvis",     "Trifolium_pratense",
                     "Vicia_grandiflora",  "Vicia_sativa")

# Get the row and column indices for the selected FORB species
forb_species_traits <- read.csv("CAREER_NUT_HERB_AllForbTraitDam_2Jan2024.csv") #617 obs of 25 variables; this is coded for 0/1 (absent/present) damage 
species_indices <- which(forb_species_traits$Taxa %in% species_to_keep)
species_indices <- as.matrix(species_indices)
is.matrix(species_indices) #TRUE 

# Subset the covariance matrix based on the selected species
reduced_cov_matrix <- cov(species_indices) # Now you can set up PGLMM for forbs

#Double checking everything; looking at tree for forbs, checking species names
plot(forb_tree)
name.check(forb_tree, forb_species)

#Order tip labels alphabetically
unique_forb <- sort(unique(forb_tree$tip.label))
unique_forb #83 unique forb species; #16 grass species

#Ordering data
unique_forb2 <- sort(unique(forb_species$Taxa))

#Checking if the names are the same in phylogeny and data
setdiff(unique_forb, unique_forb2)

#Renaming forb data
forb_data_final <- forb_species_traits #626 obs; includes forbs and legumes
forb_data_noZ_insect <- filter(forb_data_final, InsectDamage_Cat== "1") #71 excluded; 546 obs of 26 variables
forb_data_noZ_fungal <- filter(forb_data_final, FungalDamage_Cat== "1") #100 excluded; 517 obs of 26 variables
forb_data_noZ_total<- filter(forb_data_final, TotalDam_Cat== "1") #601 obs of 26 variables
################################################################################
vv_g <- vcv(grass_tree, model= "Brownian", corr= TRUE)
species_to_keep_g <- c("Andropogon_gerardii",   "Anthoxanthum_odoratum",   "Bouteloua_curtipendula", 
                     "Bromus_inermis",        "Dactylis_glomerata",      "Elymus_canadensis",
                     "Elymus_repens",         "Festuca_arundinacea",     "Panicum_capillare",
                     "Panicum_oligosanthes",  "Panicum_virgatum",        "Phleum_pratense",
                     "Poa_pratensis",         "Schizachyrium_scoparium", "Setaria_pumila",
                     "Sorghastrum_nutans",    "Sorghum_halpense",        "Sporobolus_heterolepis")

# Get the row and column indices for the selected GRASS species
grass_species_traits <- read.csv("CAREER_NUT_HERB_GrassTraitDam_2Jan2024.csv") #220 obs of 25 variables
species_indices_g <- which(grass_species_traits$Taxa %in% species_to_keep_g)
species_indices_g <- as.matrix(species_indices_g)
is.matrix(species_indices_g) #TRUE 

# Subset the covariance matrix based on the selected species
reduced_cov_matrix_g <- cov(species_indices_g) # Now you can set up PGLMM for forbs

#Renaming the grass data 
grass_data_final <- grass_species_traits #220 obs of 25 variables
grass_data_noZ_insect <- filter(grass_data_final, InsectDamage_Cat== "1") #25 excluded; 195 obs of 25 variables
grass_data_noZ_fungal <- filter(grass_data_final, FungalDamage_Cat== "1") #40 excluded; 180 obs of 25 variables
grass_data_noZ_total <- filter(grass_data_final, TotalDam_Cat== "1") #214 obs of 26 variables

'################################################################################
vv_l <- vcv(legume_tree, model= "Brownian", corr= TRUE)
species_to_keep_l <- c("Amorpha_canescens",   "Baptisia_alba",   "Baptisia_australis", 
                       "Chamaecrista_fasciculata",        "Dalea_candida",      "Dalea_purpurea",
                       "Lespedeza_capitata",         "Mimosa_quadrivalvis",     "Trifolium_pratense",
                       "Vicia_grandiflora",  "Vicia_sativa")

# Get the row and column indices for the selected LEGUME species
legume_species_traits <- read.csv("CAREER_NUT_HERB_LegumeTraitDam_15Dec2023.csv") #63 obs of 23 variables
species_indices_l <- which(legume_species_traits$Taxa %in% species_to_keep_l)
species_indices_l <- as.matrix(species_indices_l)
is.matrix(species_indices_l) #TRUE 

# Subset the covariance matrix based on the selected species
reduced_cov_matrix_l <- cov(species_indices_l) # Now you can set up PGLMM for legumes

#Renaming the legume data 
legume_data_final <- legume_species_traits #63 obs of 23 variables'

# Testing how insect damage is influenced by GS, TRT, MAT, MAP, and their interactions with site and block nested within site as random factors
library(phytools)
pglmm_forb <- pglmm(SQRT_Insect ~ logGS + Treatment + Std_MAT + Std_MAP +
                   Treatment*logGS + Std_MAT*logGS + Std_MAP*logGS +
                     (1|Taxa__) + (1|Block@Site) + (1|Site), 
                   data= forb_data_noZ_insect, 
                   cov_ranef = list(Taxa= vv),
                   family="gaussian") #Message "cov matrix is not standardized, we will do this now"... then wait until the stop sign in the top right of the console goes away

pglmm_forb #view table after cov matrix has been standardized by R (AKA wait until the small stop sign in the top right pane of the console disapears)

# Testing how insect damage is influenced by GS, TRT, MAT, MAP, and their interactions with site and block nested within site as random factors
pglmm_grass <- pglmm(SQRT_Insect ~ logGS + Treatment + Std_MAT + Std_MAP +
                       Treatment*logGS + Std_MAT*logGS + Std_MAP*logGS +
                       (1|Taxa__) + (1|Block@Site) + (1|Site), 
                     data= grass_data_noZ_insect, 
                     cov_ranef = list(Taxa= vv_g),
                     family="gaussian") #Message "cov matrix is not standardized, we will do this now"... then wait until the stop sign in the top righ tof the console goes away

pglmm_grass

'# Testing how insect damage is influenced by GS, TRT, MAT, MAP, and their interactions with site and block nested within site as random factors
pglmm_legume <- pglmm(SQRT_Insect ~ logGS + Treatment + Std_MAT + Std_MAP +
                       Treatment*logGS + Std_MAT*logGS + Std_MAP*logGS +
                       (1|Taxa__) + (1|Block@Site) + (1|Site), 
                     data= legume_data_final, 
                     cov_ranef = list(Taxa= vv_l),
                     family="gaussian") #Message "cov matrix is not standardized, we will do this now"... then wait until the stop sign in the top righ tof the console goes away

pglmm_legume'

### PGLMMs with Fungal Pathogen Damage ###
pglmm_forb_fung <- pglmm(LOG_Fungal ~ logGS + Treatment + Std_MAT + Std_MAP +
                      Treatment*logGS + Std_MAT*logGS + Std_MAP*logGS +
                      (1|Taxa__) + (1|Block@Site) + (1|Site), 
                    data= forb_data_noZ_fungal, 
                    cov_ranef = list(Taxa= vv),
                    family="gaussian") #Message "cov matrix is not standardized, we will do this now"... then wait until the stop sign in the top right of the console goes away

pglmm_forb_fung #view table after cov matrix has been standardized by R
#RESULTS: Forb fungal damage = NS

# Testing how fungal damage is influenced by GS, TRT, MAT, MAP, and their interactions with site and block nested within site as random factors
pglmm_grass_fung <- pglmm(LOG_Fungal ~ logGS + Treatment + Std_MAT + Std_MAP +
                       Treatment*logGS + Std_MAT*logGS + Std_MAP*logGS +
                       (1|Taxa__) + (1|Block@Site) + (1|Site), 
                     data= grass_data_noZ_fungal, 
                     cov_ranef = list(Taxa= vv_g),
                     family="gaussian") #Message "cov matrix is not standardized, we will do this now"... then wait until the stop sign in the top righ tof the console goes away

pglmm_grass_fung
#RESULTS: NS

'# Testing how insect damage is influenced by GS, TRT, MAT, MAP, and their interactions with site and block nested within site as random factors
pglmm_legume_fung <- pglmm(LOG_Fungal ~ logGS + Treatment + Std_MAT + Std_MAP +
                        Treatment*logGS + Std_MAT*logGS + Std_MAP*logGS +
                        (1|Taxa__) + (1|Block@Site) + (1|Site), 
                      data= legume_data_final, 
                      cov_ranef = list(Taxa= vv_l),
                      family="gaussian") #Message "cov matrix is not standardized, we will do this now"... then wait until the stop sign in the top righ tof the console goes away

pglmm_legume_fung'


### TOTAL DAMAGE ###
pglmm_forb_TOTAL <- pglmm(SQRT_Total~ logGS + Treatment + Std_MAT + Std_MAP +
                           Treatment*logGS + Std_MAT*logGS + Std_MAP*logGS +
                           (1|Taxa__) + (1|Block@Site) + (1|Site), 
                         data= forb_data_noZ_total, 
                         cov_ranef = list(Taxa= vv),
                         family="gaussian") #Message "cov matrix is not standardized, we will do this now"... then wait until the stop sign in the top right of the console goes away

pglmm_forb_TOTAL


pglmm_grass_TOTAL <- pglmm(SQRT_Total~ logGS + Treatment + Std_MAT + Std_MAP +
                            Treatment*logGS + Std_MAT*logGS + Std_MAP*logGS +
                            (1|Taxa__) + (1|Block@Site) + (1|Site), 
                          data= grass_data_noZ_total, 
                          cov_ranef = list(Taxa= vv_g),
                          family="gaussian") #Message "cov matrix is not standardized, we will do this now"... then wait until the stop sign in the top right of the console goes away

pglmm_grass_TOTAL

#Using the "afex" package to get effect tables for phylogenetically uncorrected models using the "lme" package
library(afex)
library(lme4)
library(lmerTest)

## FORB, Insect (no zeros)
lmm1 <- mixed(SQRT_Insect ~ logGS + Treatment + Std_MAT + Std_MAP +
                Treatment*logGS + Std_MAT*logGS + Std_MAP*logGS +
                (1|Site/Block), data= forb_data_noZ_insect, check_contrasts = FALSE, test_intercept= TRUE, method= "KR")

lmm1 #to view F tests
summary(lmm1) #to view parameter estimates

## GRASS, Insect
lmm2 <- mixed(SQRT_Insect ~ logGS + Treatment + Std_MAT + Std_MAP +
                Treatment*logGS + Std_MAT*logGS + Std_MAP*logGS +
                (1|Site/Block), data= grass_data_noZ_insect, check_contrasts = FALSE, test_intercept= TRUE, method = "KR")

lmm2 #to view F tests
summary(lmm2) #to view parameter estimates and random effects

'## LEGUME, Insect
lmm3 <- mixed(SQRT_Insect ~ logGS + Treatment + Std_MAT + Std_MAP +
                Treatment*logGS + Std_MAT*logGS + Std_MAP*logGS +
                (1|Site/Block), data= legume_data_final, check_contrasts = FALSE, test_intercept= TRUE, method = "KR")

lmm3 #to view F tests
summary(lmm3) #to view parameter estimates and random effects'

## FORB, Fungal
lmm1b <- mixed(LOG_Fungal ~ logGS + Treatment + Std_MAT + Std_MAP +
                 Treatment*logGS + Std_MAT*logGS + Std_MAP*logGS +
                 (1|Site/Block), data= forb_data_noZ_fungal, check_contrasts = FALSE, test_intercept= TRUE, method = "KR")

lmm1b #to view F tests
summary(lmm1b) #to view parameter estimates and random effects

## GRASS, Fungal
lmm2b <- mixed(LOG_Fungal ~ logGS + Treatment + Std_MAT + Std_MAP +
                 Treatment*logGS + Std_MAT*logGS + Std_MAP*logGS +
                 (1|Site/Block), data= grass_data_noZ_fungal, check_contrasts = FALSE, test_intercept= TRUE, method = "KR")

lmm2b #to view F tests
summary(lmm2b) #to view parameter estimates and random effects

## TOTAL DAMAGE ###
transform(forb_data_noZ_insect, SQRT_Total = as.numeric(SQRT_Total))
lmm <- mixed(SQRT_Total~ logGS + Treatment + Std_MAT + Std_MAP +
                Treatment*logGS + Std_MAT*logGS + Std_MAP*logGS +
                (1|Site/Block), data= forb_data_noZ_insect, check_contrasts = FALSE, test_intercept= TRUE, method= "KR")

lmmT #to view F tests
summary(lmmT) #to view parameter estimates


'## LEGUME, Fungal
lmm3b <- mixed(LOG_Fungal ~ logGS + Treatment + Std_MAT + Std_MAP +
                 Treatment*logGS + Std_MAT*logGS + Std_MAP*logGS +
                 (1|Site/Block), data= legume_data_final, check_contrasts = FALSE, test_intercept= TRUE, method = "KR")

lmm3b #to view F tests
summary(lmm3b) #to view parameter estimates and random effects'

################ GRAPHS ################
library(ggplot2)

#Figure of GS influence on insect damage for all FG (forb, grass, legume)
ggplot(data = bigdata, mapping = aes(x = logGS, y = SQRTinsectWP, color= Lifeform)) + 
  geom_point() + geom_smooth(method = "lm", se=FALSE) + aes(fill=Lifeform) + theme_classic() +
  ggtitle("") + xlab("Log GS (pg)") +
  ylab("SQRT Insect Damage (%)")
#Forbs and grasses with larger GS experience more insect herbivory damage, while the opposite is true for legumes. 

#Figure of GS influence on fungal damage for all FG (forb/legume, grass)
ggplot(data = bigdata, mapping = aes(x = logGS, y = LOGfungal, color= Lifeform)) + 
  geom_point() + geom_smooth(method = "lm", se=FALSE) + aes(fill=Lifeform) + theme_classic() +
  ggtitle("") + xlab("Log GS (pg)") +
  ylab("LOG Fungal Damage (%)")
# Legumes with larger GS experience greater fungal pathogen damage, while the opposite is true to grasses. However, there is an intermediate effect on forbs. 

write.csv(bigdata, file= "bigdata.csv")

# 3 April 2024: Contour/Color Density Plot for MAT and GS in forbs/insect damage
library(ggplot2)

ggplot(forb_data_noZ_insect, aes(x= logGS, y= Std_MAT)) + 
  geom_density_2d_filled() + 
  theme_classic() +
  scale_fill_brewer() +
  guides(fill = guide_legend(title = "Insect Damage"))

# OR USE THIS ONE

ggplot(forb_data_noZ_insect, aes(x= logGS, y= Std_MAT, fill= ..level..)) + 
  stat_density_2d(geom= "polygon") + 
  theme_classic() +
  guides(fill = guide_legend(title = "Insect Damage"))
