# Script to load and clean raw data set leptuca cta
# Diogo Jackson
# Last update: 2019-11-01

# Start----
# Packages ----------------------------------------------------------------
library(tidyverse)
library(inspectdf)
library(forcats)
library(tidyr)

# Load data ---------------------------------------------------------------
rd <- read.csv(file = "data/raw/raw_leptuca_cta.csv")

# 1. Basic checks --------------------------------------------------------------
nrow(rd)             # How many rows
str(rd)              # Variables classes
attributes(rd)       # Attributres
head(rd)             # First rows
any(duplicated(rd))  # There is any duplicated rows?
any(is.na(rd))       # There are NAs in the data?
dat <- rd

# 2. Fix factor variables-------------------------------------------------------
# factor variables
levels(dat$spe_LNCN)
levels(dat$spe_LCCA)
levels(dat$spe_LCLA)
levels(dat$spe_LACA)
levels(dat$spe_LPCB)
levels(dat$spe_LBCP)
levels(dat$spe_BRCTA)
levels(dat$spe_LNLP)


# 3. Rename cols -------------------------------------------------------------
data_clean <- dplyr::rename(dat,
                            sp_LNCN = spe_LNCN,
                            sp_LCCA  = spe_LCCA,
                            sp_LCLA = spe_LCLA ,
                            sp_LACA = spe_LACA,
                            sp_LPCB = spe_LPCB,
                            sp_LBCP = spe_LBCP,
                            sp_BRCTA = spe_BRCTA,
                            sp_LNLP = spe_LNLP)

head(data_clean)

#1. Leptuca leptodactyla natural (LN) versus Leptuca cumulanta natural(CN) -----

#1.1 extract LNCN colums and agrouped by species
data_1 <- dplyr::select(data_clean, 
                        sp_LNCN, 
                        choice_LNCN) #select colums
g_1   <- group_by(data_1, 
                  sp_LNCN)
data_LNCN <- summarise(g_1,
          soma = sum(choice_LNCN))
data_LNCN

#2. Leptuca leptodactyla cinza (LC) versus Leptuca cumulanta amarelo (CA) -----

#2.1 extract LCCA colums and agrouped by species
data_2 <- dplyr::select(data_clean, 
                        sp_LCCA, 
                        choice_LCCA) #select colums
g_2   <- group_by(data_2, 
                  sp_LCCA)
data_LCCA <- summarise(g_2,
          soma = sum(choice_LCCA))
data_LCCA

# 3. Leptodactyla cinza (LC) versus Leptodactyla amarelo (LA) ---------------------------------

data_3 <- dplyr::select(data_clean, 
                        sp_LCLA, 
                        choice_LCLA) #select colums
g_3   <- group_by(data_3, 
                  sp_LCLA)
data_LCLA <- summarise(g_3,
          soma = sum(choice_LCLA))
data_LCLA

# 4. Leptodactyla amarelo (LA) versus Cumulanta amarelo (CA) -------------------

# Remove NAs
# list rows of data that have missing values


data_4 <- dplyr::select(data_clean, 
                        sp_LACA, 
                        choice_LACA) #select colums
g_4   <- group_by(data_4, 
                  sp_LACA)
summarise(g_4,
          soma = sum(choice_LACA))

data_4 <- data_4[complete.cases(data_4),]

g_4   <- group_by(data_4, 
                  sp_LACA)
data_LACA <- summarise(g_4,
          soma = sum(choice_LACA))
data_LACA

# 5. Leptodactyla preto (LP) versus Cumulanta Branco (CB)--------------------------------------
data_5 <- dplyr::select(data_clean, 
                        sp_LPCB, 
                        choice_LPCB) #select colums
g_5   <- group_by(data_5, 
                  sp_LPCB)
summarise(g_5,
          soma = sum(choice_LPCB))

data_5 <- data_5[complete.cases(data_5),]

g_5  <- group_by(data_5, 
                 sp_LPCB)
data_LPCB <- summarise(g_5,
          soma = sum(choice_LPCB))
data_LPCB

# 6. Leptodactyla branco (LB) versus Cumulanta preto(CP)------------------------
data_6 <- dplyr::select(data_clean, 
                        sp_LBCP, 
                        choice_LBCP) #select colums
g_6   <- group_by(data_6, 
                  sp_LBCP)
summarise(g_6,
          soma = sum(choice_LBCP))

data_6 <- data_6[complete.cases(data_6),]

g_6  <- group_by(data_6, 
                 sp_LBCP)
data_LBCP <- summarise(g_6,
          soma = sum(choice_LBCP))
data_LBCP

#7. Leptodactyla Barra do Rio (BR) vs Leptodactyla CTA (CTA)--------------------
data_7 <- dplyr::select(data_clean, 
                        sp_BRCTA, 
                        choice_BRCTA) #select colums
g_7   <- group_by(data_7, 
                  sp_BRCTA)
summarise(g_7,
          soma = sum(choice_BRCTA))

data_7 <- data_7[complete.cases(data_7),]

g_7  <- group_by(data_7, 
                 sp_BRCTA)
data_BRCTA <- summarise(g_7,
          soma = sum(choice_BRCTA))
data_BRCTA

#8. Leptodactyla natural (LP) vs Leptodactyla protetor (LP)------------------------
data_8 <- dplyr::select(data_clean, 
                        sp_LNLP, 
                        choice_LNLP) #select colums
g_8   <- group_by(data_8, 
                  sp_LNLP)
summarise(g_8,
          soma = sum(choice_LNLP))

data_8 <- data_8[complete.cases(data_8),]

g_8  <- group_by(data_8, 
                 sp_LNLP)
data_LNLP <- summarise(g_8,
          soma = sum(choice_LNLP))
data_LNLP

# 8. Save processed data -----------------------------------------------------
write.csv(x = data_LNCN, 
          file = "data/processed/processed_data_LNCN.csv", 
          row.names = FALSE)

write.csv(x = data_LCCA, 
          file = "data/processed/processed_data_LCCA.csv", 
          row.names = FALSE)

write.csv(x = data_LCLA, 
          file = "data/processed/processed_data_LCLA.csv", 
          row.names = FALSE)

write.csv(x = data_LACA, 
          file = "data/processed/processed_data_LACA.csv", 
          row.names = FALSE)

write.csv(x = data_LPCB, 
          file = "data/processed/processed_data_LPCB.csv", 
          row.names = FALSE)

write.csv(x = data_LBCP, 
          file = "data/processed/processed_data_LBCP.csv", 
          row.names = FALSE)

write.csv(x = data_BRCTA, 
          file = "data/processed/processed_data_BRCTA.csv", 
          row.names = FALSE)

write.csv(x = data_LNLP, 
          file = "data/processed/processed_data_LNLP.csv", 
          row.names = FALSE)

# Test saved data
data_test_LNCN <- read.csv("data/processed/processed_data_LNCN.csv")
head(data_test_LNCN)

data_test_LCCA <- read.csv("data/processed/processed_data_LCCA.csv")
head(data_test_LCCA)

data_test_LCLA <- read.csv("data/processed/processed_data_LCLA.csv")
head(data_test_LCLA)

data_test_LACA <- read.csv("data/processed/processed_data_LACA.csv")
head(data_test_LACA)

data_test_LPCB <- read.csv("data/processed/processed_data_LPCB.csv")
head(data_test_LPCB)

data_test_LBCP <- read.csv("data/processed/processed_data_LBCP.csv")
head(data_test_LBCP)

data_test_BRCTA <- read.csv("data/processed/processed_data_BRCTA.csv")
head(data_test_BRCTA)

data_test_LNLP <- read.csv("data/processed/processed_data_LNLP.csv")
head(data_test_LNLP)

# END-------

