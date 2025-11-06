

# ------------------------------------- packages


library(tidyverse)
library(readxl)
library(writexl)
library(lubridate) # prace s datumy
library(zoo) # ?
library(shiny) # aplikace
library(rvest) # html


# ------------------------------------- paths and sys


path <- "C:/Users/krizova/Documents/R/02 cenoveKalkukacky/_vyvoj/"
Sys.setlocale("LC_TIME", "C")
tms_now <- Sys.time()


# ------------------------------------- initial set
# * sourcing * sourcing * sourcing *

low_spot <- 24.30
aktual_spot <- low_spot + 0.4
surcharge <- 0.05 
bsd <- 1.2


# ------------------------------------- INPUT :: forward


shell.exec(paste0(path, "vstupy/input_fwdKrivka.xlsx")) # open fwd and get most recent values

fwd <- read_excel(paste0(path, "vstupy/input_fwdKrivka.xlsx"), sheet = "List1") %>% 
  rename("PFC" = NCG, "FX" = 'FX rate')
fwd_akt <- lubridate::date(unique(fwd$akt))

# **************** kontrola dat ******************
if (Sys.Date()-fwd_akt>1) {
  stop("Nemas aktualni FWD data")
}
if (nrow(fwd)!=48) {
  stop("Nemas kompletni FWD data")
}
if (any(is.na(fwd$PFC))) {
  stop("Nemáš komplet PFC krivku")
}
if (any(is.na(fwd$FX))) {
  stop("Nemáš komplet FX krivku")
}
# **************** kontrola dat ******************


# ------------------------------------- INPUT :: OTC


html <- read_html("X:/OTC/HTML/CZ-VTP.html")
tab <- html_table(html, fill = TRUE)[[1]]
temp <- (tab[[1]][[1]]) 
tms_otc <- as.POSIXct(str_remove(temp, "\r\n "), format = "%d.%m.%Y %H:%M", tz = "Europe/Prague") 
otc <- html_table(html, fill = TRUE)[[1]] %>% 
  select("product" = X1, "price" = X2)  %>% 
  filter(str_detect(product, "^CZ")) %>% 
  mutate(price = as.numeric(str_replace(price, ",", ".")),
         year = case_when(str_detect(product, "2025|25") ~ 2025,
                          str_detect(product, "2026|26") ~ 2026,
                          str_detect(product, "2027|27") ~ 2027,
                          str_detect(product, "2028|28") ~ 2028,
                          str_detect(product, "2029|29") ~ 2029,
                          TRUE ~ NA),
         quater = case_when(str_detect(product, "Q1") ~ "Q1",
                            str_detect(product, "Q2") ~ "Q2",
                            str_detect(product, "Q3") ~ "Q3",
                            str_detect(product, "Q4") ~ "Q4",
                            TRUE ~ NA),
         month = case_when(str_detect(product, "Jan-") ~ 1,
                           str_detect(product, "Feb-") ~ 2,
                           str_detect(product, "Mar-") ~ 3,
                           str_detect(product, "Apr-") ~ 4,
                           str_detect(product, "May-") ~ 5,
                           str_detect(product, "Jun-") ~ 6,
                           str_detect(product, "Jul-") ~ 7,
                           str_detect(product, "Aug-") ~ 8,
                           str_detect(product, "Sep-") ~ 9,
                           str_detect(product, "Oct-") ~ 10,
                           str_detect(product, "Nov-") ~ 11,
                           str_detect(product, "Dec-") ~ 12,
                           TRUE ~ NA),
         cal = ifelse(str_detect(product, "^CZ VTP \\d{4}$"), year, NA))

write.table(otc, "clipboard", sep = "\t", row.names = FALSE)

# m1 <- otc[[2]][[3]]
# m2 <- otc[[2]][[4]]
# m3 <- otc[[2]][[5]]
# q1 <- otc[[2]][[8]]
# q2 <- otc[[2]][[9]]
# q3 <- otc[[2]][[10]]
# q4 <- otc[[2]][[11]]
# y1 <- otc[[2]][[18]]
# y2 <- otc[[2]][[19]]
# y3 <- otc[[2]][[20]]

# **************** kontrola dat ******************
if (any(is.na(otc$price))) {
  print(paste0("Chybi hodnota pro: ", otc$product[is.na(otc$price)]))
  stop("Nekompletni OTC data")
}


rm(html)
rm(tab)
rm(temp)


# ------------------------------------- delivery period
# v app Date Range Selector https://shiny.posit.co/r/components/inputs/date-range-selector/

delOd <- as.Date("2026-02-01")
delDo <- as.Date("2029-01-01")

delPer <- as.POSIXct(seq(from = delOd, to = delDo, by = "month") %>% head(-1)) # head = maze posledni element (1.1.2027)
delivery <- as.data.frame(delPer)


# ------------------------------------- INPUT :: profil


profil <- read_excel(paste0(path, "vstupy/input_profil.xlsx"), sheet = "List2", col_names = T) %>% 
  mutate(mesic = lubridate::month(datum),
         rok = lubridate::year(datum))


# ------------------------------------- FRAME 

frameOd <- as.Date("2025-01-01")
frameDo <- as.Date("2028-12-31")

framePer <- as.POSIXct(seq(from = frameOd, to = frameDo, by = "month"))
frame <- as.data.frame(framePer) %>% 
  mutate(year = year(framePer),
         month = month(framePer),
         quater = case_when(month <= 3 ~ "Q1",
                            month <= 6 ~ "Q2",
                            month <= 9 ~ "Q3",
                            month <= 12 ~ "Q4"),
         now = ifelse(month == month(tms_now), 1, 0)) %>% # jaky mesic je ted
  left_join(profil, by = c("framePer" = "datum")) %>% 
  left_join(fwd, by = c("framePer" = "mesic")) %>% 
  mutate(dodavka = ifelse(framePer %in% delPer, 1, 0)) %>% 
  select(framePer, year, quater, month, now, dodavka, profilMWh, PFC, FX)

view(frame)

# ------------------------------------- Data_Vstup


data <- frame %>% 
  group_by(year) %>% 
  mutate(
    
    # cena komodity
    
         PFCratio = PFC/mean(PFC),
         celyRok = if_else(all(dodavka == 1), "ANO", "NE")) %>% 
  ungroup() %>% 
  group_by(year, quater) %>% 
  mutate(celyQ = if_else(all(dodavka == 1), "ANO", "NE")) %>% 
  ungroup() %>% 
  mutate(


         PFCprepoc = case_when(celyRok == "ANO" ~ ))
           
           
           
           
           round(case_when(rok == 2026 ~ PFCratio*cal26,
                                     rok == 2027 ~ PFCratio*cal27,
                                     rok == 2028 ~ PFCratio*cal28,
                                     rok == 2029 ~ PFCratio*cal29, TRUE ~ NA), 3),
    # kurz EUR
    
         swapPoint = (FX-low_spot)*1000,
         FXrecalc = aktual_spot+swapPoint/1000+surcharge,
    
         cenaEUR = round(profilMWh*PFCprepoc, 0),
         vazenaCena = profilMWh*FXrecalc) %>% 
  
  ungroup()


# ------------------------------------- Kalkulace


data_vstup <- join %>%
  select(rok, mesic, profilMWh, PFCprepoc, cenaEUR, FXrecalc, vazenaCena)  # final df to match table on sheet Kalkulace


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
fin_cenaEUR <- ceiling(prodej_eur/0.025) * 0.025 # zaokrouhleni na nejblizsi nejvyssi hranici 0,025 
fin_cenaCZK <- ceiling((fin_cenaEUR*kurz)/0.05) * 0.05 # zaokrouhleni na nejblizsi nejvyssi hranici 0,05 

# dodaci obdobi

od <- min(profil$datum)
do <- max(profil$datum) + months(1)


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

