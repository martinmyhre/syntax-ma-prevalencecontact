### Meta-Analyse - Tjenestekontakt før selvmord ###

# Syntaks for alle analyser gjennomført i forbindelse på meta-analysen: #
# Tjenestekontakt for selvmord                                          #
#########################################################################

# Last inn nødvendige pakker # 

library(metafor)
library(meta)
library(rmeta)
library(Rcpp)
library(dplyr)
library(tidyr)
library(readr)

#######################################################################
############ Mental Health Services ###################################
#######################################################################

############### Forberedelser ####################

# Les inn data # 

MHS <- read_delim("//kant/med-klinmed-pha-nssf-felles/Kartleggingssystemet/Underprosjekter/Review_kontakt_foer_selvmord/Analyse/Analyse_final/Metaanalyse_tjenestekontakt_2017/Data_to_import/Import_R_MHS_300317.csv", 
                  ";", escape_double = FALSE, na = "NA", 
                  trim_ws = TRUE)
print(MHS)
str(MHS)

# Legge til faktorer på de kategoriske variablene #

MHS$Region <- as.factor(MHS$Region)

MHS$Region <- factor(MHS$Region, levels=c(1:7),
                     labels=c("Scandinavia", "United Kingdom", "Continental Europe", 
                              "United States of America", "Eastern Asia", "Australia & Oceania",
                              "Canada"))

MHS$Agegroup <- as.factor(MHS$Agegroup)

MHS$Agegroup <- factor(MHS$Agegroup, levels=c(0,1),
                       labels=c("NO", "YES"))

# Opprett data.frame for 1 år #

MHS_1year <- MHS %>%
  filter(MHS_1year > 0, Agegroup == "NO") %>%
  select(Author, N, MHS_1year, Median_sample_year, Region, Agegroup)
print(MHS_1year)

# Tilrettelegg data.frame for 1 år så den kan merges #

MHS_1year$Setting <- as.factor(c(3, 3, 3, 3, 3, 3, 3))

names(MHS_1year)[names(MHS_1year) == "MHS_1year"] <- "N_contact"

names(MHS_1year)[names(MHS_1year) == "Agegroup"] <- "Agegroups"

###############################################################################

################ Meta-analyse Mental Health Services 1 år #####################

# Meta-analyse # 

Meta_MHS_1year <- metaprop(N_contact, N, Author, data = MHS_1year,
                           method = "Inverse", method.tau = "DL",
                           comb.random = T, comb.fixed = F,
                           backtransf = T, sm = "PFT",
                           title = "Mental Health Services 1 year")
Meta_MHS_1year

forest(Meta_MHS_1year,
       leftcols = c("studlab", "n", "effect.ci", "w.random"),
       rightcols = FALSE,
       leftlabs = c("Study", "Suicides", "Estimate"),
       col.diamond = "black",
       col.square = "grey",
       col.inside = "black",
       col.study = "black",
       col.by = "black",
       lwd = 1,
       prediction = T,
       xlab = "Prevalence of contact with services",
       xlim = c(0, 0.5))


# Publikasjonbias #

funnel(Meta_MHS_1year)

meta

# Meta-regresjon #

metareg_MHS_1year <- metareg(Meta_MHS_1year, MHS_1year$Median_sample_year)
metareg_MHS_1year

###############################################################################
################## Inpatients #################################################
###############################################################################

############### Forberedelser ################################################

# Les inn data #

IP <- read_delim("//kant/med-klinmed-pha-nssf-felles/Kartleggingssystemet/Underprosjekter/Review_kontakt_foer_selvmord/Analyse/Analyse_final/Metaanalyse_tjenestekontakt_2017/Data_to_import/Import_R_IP_290317.csv", 
                 ";", escape_double = FALSE, na = "NA", 
                 trim_ws = TRUE)
print(IP)
str(IP)

# Legge til faktorer på de kategoriske variablene #

IP$Region <- as.factor(IP$Region)

IP$Region <- factor(IP$Region, levels=c(1:7),
                    labels=c("Scandinavia", "United Kingdom", "Continental Europe", 
                             "United States of America", "Eastern Asia", "Australia & Oceania",
                             "Canada"))

IP$Agegroups <- as.factor(IP$Agegroups)

IP$Agegroups <- factor(IP$Agegroups, levels=c(0,1),
                       labels=c("NO", "YES"))

# Opprett data.frame for current IP #

IP_current <- IP %>%
  filter(IP_current > 0, Agegroups == "NO") %>%
  select(Author, N, IP_current, Median_sample_year, Region, Agegroups)
print(IP_current)
str(IP_current)

# Opprett data.frame for 1 år #

IP_1year <- IP %>%
  filter(IP_1year > 0, Agegroups == "NO") %>%
  select(Author, N, IP_1year, Median_sample_year, Region, Agegroups)
print(IP_1year)
str(IP_1year)

# Tilrettelegg data.frame for 1 år så den kan merges # 

IP_1year$Setting <- as.factor(c(1,1,1,1,1,1,1,1,1,1,1,1))

names(IP_1year)[names(IP_1year) == "IP_1year"] <- "N_contact"

###############################################################################

####################### Meta-analyse Inpatients current #######################

Meta_IP_current <- metaprop(IP_current, N, Author, data = IP_current,
                            method = "Inverse", method.tau = "DL",
                            comb.random = T, comb.fixed = F,
                            backtransf = T, sm ="PFT",
                            title = "Inpatients current")
Meta_IP_current

forest(Meta_IP_current,
       leftcols = c("studlab", "n", "effect.ci", "w.random"),
       rightcols = FALSE,
       leftlabs = c("Study", "Suicides", "Estimate", "Weight"),
       col.diamond = "black",
       col.square = "grey",
       col.inside = "black",
       col.study = "black",
       col.by = "black", 
       fs.heading = 12,                                             # Juster skriftstørrelse
       fs.random = 11, fs.random.label = 11, fs.predict = 11, 
       fs.study = 11, fs.axis = 10, fs.xlab = 10,
       ff.random = 0,                                              # Fjerne fet skrift på modellen
       lwd = 1,
       addrow = F,                                                 # Fjern avstanden mellom data og overall
       xlab = "Prevalence of contact with services prior to suicide",
       xlim = c(0, 0.1))
grid.text("Figure 2. Forestplot of Currently Admitted Suicide cases", 0.34, 0.75,
          gp=gpar(fontface=2, fontsize=12))


# Publikasjonsbias # 

funnel(Meta_IP_current)

metabias(Meta_IP_current, method.bias = "rank", k.min = 9) # Begg test
metabias(Meta_IP_current, method.bias = "linreg", k.min = 9) # Egger test

# Meta-regresjon # 

metareg_IP_current <- metareg(Meta_IP_current, Median_sample_year)
metareg_IP_current

##############################################################################

################### Meta-analyse Inpatients 1 year ###########################

Meta_IP_1year <- metaprop(N_contact, N, Author, data = IP_1year,
                          method = "Inverse", method.tau = "DL",
                          comb.random = T, comb.fixed = F,
                          backtransf = T, sm = "PFT",
                          title = "Inpatients 1 year")
Meta_IP_1year

forest(Meta_IP_1year,
       xlab = "Prevalence",
       xlim = c(0, 0.5),
       col.diamond = "black",
       col.square = "grey53",
       col.inside = "black",
       col.study = "black",
       lwd = 1.5)

# Publikasjonsbias #

funnel(Meta_IP_1year,
       shade = "Grey")

metabias(Meta_IP_1year, method.bias = "rank") # Begg test
metabias(Meta_IP_1year, method.bias = "linreg") # Egger test

# Meta-regresjon

metareg_IP_1year <- metareg(Meta_IP_1year, IP_1year$Median_sample_year)
metareg_IP_1year

############################################################################
################## Outpatients #############################################
############################################################################

############### Forberedelser ####################

# Last inn datasettet # 

OP <- read_delim("//kant/med-klinmed-pha-nssf-felles/Kartleggingssystemet/Underprosjekter/Review_kontakt_foer_selvmord/Analyse/Analyse_final/Metaanalyse_tjenestekontakt_2017/Data_to_import/Import_R_OP_080617.csv", 
                 ";", escape_double = FALSE, na = "NA", 
                 trim_ws = TRUE)
print(OP)
str(OP)

# Legg til faktorer på de kategoriske variablene # 

OP$Region <- as.factor(OP$Region)

OP$Region <- factor(OP$Region, levels=c(1:7),
                    labels=c("Scandinavia", "United Kingdom", "Continental Europe", 
                             "United States of America", "Eastern Asia", "Australia & Oceania",
                             "Canada"))

OP$Agegroups <- as.factor(OP$Agegroups)

OP$Agegroups <- factor(OP$Agegroups, levels=c(0,1),
                       labels=c("NO", "YES"))

# Opprett data.frame for 1 år #

OP_1year <- OP %>%
  filter(OP_1year > 0, Agegroups == "NO") %>%
  select(Author, N, OP_1year, Median_sample_year, Region, Agegroups)
print(OP_1year)
str(OP_1year)

# Tilrettelegg data.frame for 1 år så den kan merges # 

OP_1year$Setting <- as.factor(c(2,2,2,2,2))

names(OP_1year)[names(OP_1year) == "OP_1year"] <- "N_contact"

###############################################################################

####################### Meta-analyse Outpatients 1 year #######################

Meta_OP_1year <- metaprop(OP_1year, N, Author, data = OP_1year,
                          method = "Inverse", method.tau = "DL",
                          comb.random = T, comb.fixed = F,
                          backtransf = T, sm ="PFT",
                          title = "Outpatients 1 year")
Meta_OP_1year

forest(Meta_OP_1year,
       xlab = "Prevalence",
       xlim = c(0, 0.6))


# Publikasjonsbias # 

funnel(Meta_OP_1year)

metabias(Meta_OP_1year, method.bias = "linreg", k.min = 4) # Egger test
metabias(Meta_OP_1year, method.bias = "rank", k.min = 4) # Begg test

# Meta-regresjon #

metareg_OP_1year <- metareg(Meta_OP_1year, Median_sample_year)
metareg_OP_1year

bubble(metareg_OP_1year)

#############################################################################
#################### Forestplot subgruppert på setting ######################
#############################################################################

# Merge sammen de separate datasettene # 

All_1year <- bind_rows(MHS_1year, IP_1year, OP_1year)
print(All_1year)
str(All_1year)

All_1year$Setting <- factor(All_1year$Setting, levels = c(1,2,3),
                            labels = c("Inpatients",
                                       "Outpatients", "Mental Health Services"))

All_1year$Agegroups <- NULL

# Meta-analyse subgruppert på setting uten oppsummering #

Meta_all_1year <- metaprop(N_contact, N, Author, data = All_1year,
                           byvar = Setting, 
                           method = "Inverse", method.tau = "DL",
                           comb.random = T, comb.fixed = F,
                           backtransf = T, sm = "PFT",
                           title = "Contact with services within 1 year")
Meta_all_1year

forest(Meta_all_1year,
       print.byvar = F,                                             # Ikke print byvar før undergruppene
       overall = F,                                                 # Ikke print ett overall estimat
       weight.study = "random",                                     # juster størrelsen på boksene etter en random effects vekting                                       
       leftcols = c("studlab", "n", "effect.ci", "w.random"),       # endre kolonnene
       rightcols = FALSE,                                           # fjern kolonner på høyre side av plottet   
       leftlabs = c("Study", "Suicides", "Prevalence", "Weight"),   # endre titler på kolonnene på høyre side
       col.diamond = "black",                                       # Juster fargene i forestplottet
       col.square = "grey", col.inside = "black", 
       col.study = "black", col.by = "black",
       fs.heading = 12,                                             # Juster skriftstørrelse
       fs.random = 11, fs.random.label = 11, fs.predict = 11, 
          fs.study = 11, fs.axis = 10, fs.xlab = 10,
       ff.random = 0,                                              # Fjerne fet skrift på modellen
       lwd = 1,
       addrow = F,                                                 # Fjern avstanden mellom data og overall
       xlab = "Prevalence of contact prior to suicide")            # Tittel på x aksen
grid.text("Figure 3. Forestplot of Prevalence of Suicides in Contact with Services within 1 year Stratified by Setting", 0.44, 0.9,
          gp=gpar(fontface=2, fontsize=12))

            
# Publikasjons bias #

funnel(Meta_all_1year)

##############################################################################
################## IP 1 year BY gender #######################################
##############################################################################

############### Forberedelser ####################

# Last inn datasettet # 

Gender <- read_delim("//kant/med-klinmed-pha-nssf-felles/Kartleggingssystemet/Underprosjekter/Review_kontakt_foer_selvmord/Analyse/Analyse_final/Metaanalyse_tjenestekontakt_2017/Data_to_import/Import_R_gender_050417.csv", 
                     ";", escape_double = FALSE, na = "NA", 
                     trim_ws = TRUE)
print(Gender)
str(Gender)

# Legg inn faktorer på de kategoriske variablene # 

Gender$Gender <- as.factor(Gender$Gender)

Gender$Gender <- factor(Gender$Gender, levels = c(0, 1),
                        labels = c("Male", "Female"))

Gender$Agegroups <- as.factor(Gender$Agegroups)

Gender$Agegroups <- factor(Gender$Agegroups, levels = c(0, 1),
                           labels = c("NO", "YES"))

# Fjern spesifikke aldersgrupper og rader med NA fra datasettet # 

Gender <- Gender %>%
  filter(P_1year > 0, Agegroups == "NO")
print(Gender)
str(Gender)

###############################################################################

####################### Meta-analyse IP 1 year BY gender #######################

Meta_gender <- metaprop(P_1year, N, Author, data = Gender,
                        byvar = Gender, 
                        method = "Inverse", method.tau = "DL",
                        comb.random = T, comb.fixed = F,
                        backtransf = T, sm ="PFT",
                        title = "IP 1 year by gender")
Meta_gender

forest(Meta_gender)

# Meta-regresjon # 

metareg_gender <- metareg(Meta_gender)
metareg_gender

############################################################################