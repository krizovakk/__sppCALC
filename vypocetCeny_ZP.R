# ------------------------------------- packages

library(tidyverse)
library(readxl)
library(writexl)
library(lubridate) # prace s datumy
library(zoo)
library(shiny) # aplikace


# ------------------------------------- paths

path <- "C:/Users/krizova/Documents/R/cenove_kalkukacky/_vyvoj/"


# ------------------------------------- load inputs

profil <- read_excel(paste0(path, "vstupy/input_profil.xlsx"), sheet = "List2", col_names = T) %>% 
  mutate(mesic = lubridate::month(datum),
         rok = lubridate::year(datum)) %>% 
  select(datum, rok, mesic, profilMWh)

# fwd <- read_excel(paste0(path, "vstupy/trans_001_FWD_SPP-CZ_KAM_Aktual.xlsx"), sheet = "List1") %>% 
fwd <- read_excel(paste0(path, "vstupy/input_fwdKrivka.xlsx"), sheet = "List1") %>% 
  rename("PFC" = NCG, "FX" = 'FX rate')

trayport <- "X:/Nakup _ NEW/02_Trayport_Ceny/Data_Trayport_Live.xlsx"

cal26 <- read_excel(trayport, sheet = "Output", range = "P22", col_names = FALSE)[[1]]
cal27 <- read_excel(trayport, sheet = "Output", range = "P23", col_names = FALSE)[[1]]
cal28 <- read_excel(trayport, sheet = "Output", range = "P24", col_names = FALSE)[[1]]
cal29 <- read_excel(trayport, sheet = "Output", range = "P25", col_names = FALSE)[[1]]


# ------------------------------------- set inputs

low_spot <- 24.25
aktual_spot <- low_spot + 0.4
surcharge <- 0.05 


# ------------------------------------- recalculate inputs

join <- profil %>% 
  left_join(fwd, by = c("datum" = "mesic")) %>% 
  group_by(rok) %>% 
  mutate(PFCratio = PFC/mean(PFC),
         PFCprepoc = round(case_when(rok == 2026 ~ PFCratio*cal26,
                                     rok == 2027 ~ PFCratio*cal27,
                                     rok == 2028 ~ PFCratio*cal28,
                                     rok == 2029 ~ PFCratio*cal29, TRUE ~ NA), 3),
         swapPoint = (FX-low_spot)*1000,
         cenaEUR = round(profilMWh*PFCprepoc, 0),
         FXrecalc = aktual_spot+swapPoint/1000+surcharge,
         vazenaCena = profilMWh*FXrecalc) %>% 
  ungroup()

data_vstup <- join # final df
rm(join)

# ------------------------------------- calculate fixed price

# vypocty pod tabulkou

suma_profil <- sum(data_vstup$profilMWh)
suma_cenaEUR <- sum(data_vstup$cenaEUR)
suma_vazenaCena <- sum(data_vstup$vazenaCena)
mean_PFC <- mean(data_vstup$PFCprepoc)
nakup <- suma_cenaEUR/suma_profil
prirazka_nakup <- 0.000 #  ???
kurz <- round(suma_vazenaCena/suma_profil, 2)
prodej_eur <- nakup+prirazka_nakup
prodej_czk <- prodej_eur*kurz

# vypocty nad tabulkou

naklad_profil <- round(nakup-mean_PFC, 3)
fin_cenaEUR <- ceiling(prodej_eur/0.025)*0.025 # zaokrouhleni na nejblizsi nejvyssi hranici 0,025 
fin_cenaCZK <- ceiling((fin_cenaEUR*kurz)/0.05)*0.05 # zaokrouhleni na nejblizsi nejvyssi hranici 0,05 

# dodaci obdobi

od <- min(profil$datum)
do <- max(profil$datum)+months(1)


# ------------------------------------- final price

fix_cena <- data.frame(
  Od = od,
  Do = do,
  `Cena EUR` = fin_cenaEUR,
  `Cena CZK` = fin_cenaCZK,
  `Naklad na profil` = naklad_profil,
  check.names = FALSE # aby nebyly v nazvu sloupcu misto mezer tecky
)

print(fix_cena)






