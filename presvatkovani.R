# ------------------------------------- packages

library(tidyverse)
library(readxl)
library(writexl)
library(lubridate) # prace s datumy
library(zoo)
library(shiny) # aplikace


# ------------------------------------- paths

path <- "C:/Users/krizova/Documents/R/cenove_kalkukacky/"

load <- read_excel(paste0(path, "Bohemia Sekt_profil 2024_EE - kopie.xlsx"), col_names = T)         # diagram za 2024
# load <- read_excel(paste0(path, "erbaLachema.xls"), col_names = T)                                  # diagram za 09/24-08-25

# ------------------------------------- load original diagram

diagram <- load %>% 
  mutate(
    year = lubridate::year(date),
    month = lubridate::month(date),
    week = lubridate::week(date),
    day = lubridate::day(date),
    weekday = lubridate::wday(date, week_start = 1),
    # pair = paste0(str_pad(week, 2, pad = "0"), "_", str_pad(weekday, 2, pad = "0"))
    md = paste0(month, "-", day),
    hol = case_when(
      md == ("1-1") ~ "novy_rok",
      md == ("5-1") ~ "svatek_prace",
      md == ("5-8") ~ "konec_valky",
      md == ("7-5") ~ "cyril_metod",
      md == ("7-6") ~ "jan_hus",
      md == ("9-28") ~ "sv_vaclav",
      md == ("10-28") ~ "vznik_statu",
      md == ("11-17") ~ "den_student",
      md == ("12-24") ~ "stedry_den",
      md == ("12-25") ~ "bozi_hod",
      md == ("12-26") ~ "sv_stepan",
      as.Date(date) %in% as.Date(c("2024-03-29", "2025-04-18")) ~ "easter_friday",
      as.Date(date) %in% as.Date(c("2024-03-30", "2025-04-19")) ~ "easter_saturday",
      as.Date(date) %in% as.Date(c("2024-03-31", "2025-04-20")) ~ "easter_sunday",
      as.Date(date) %in% as.Date(c("2024-04-01", "2025-04-21")) ~ "easter_monday", # pridat i ostatni roky ?
      TRUE ~ "reg"),
    mwh_reg = case_when(hol != "reg" & weekday %in% c(6:7) ~ mwh,
                        hol == "reg" ~ mwh, TRUE ~ NA)
  ) %>%
  group_by(date) %>% mutate(hour = seq_along(date)) %>% ungroup() %>%
  select(hol, date, year, month, week, weekday, hour, mwh, mwh_reg) 
# filter(hour == 12)


fill_nearest_reg_by_hour <- function(df) {
  
  df <- df %>% arrange(hour, date) # ensure day is Date
  reg_days <- unique(df$date[df$hol == "reg"]) # get unique days and identify which are "reg"
  df <- df %>%
    group_by(hour) %>%
    mutate(
      mwh_pair = sapply(seq_along(date), function(i) {
        if (!is.na(mwh_reg[i])) return(mwh_reg[i])  # if not missing, keep it
        if (length(reg_days) == 0) return(NA)     
        nearest_day <- reg_days[which.min(abs(as.numeric(date[i] - reg_days)))] # find nearest reg day
        reg_val <- df$mwh_reg[df$date == nearest_day & df$hour == hour[i] & df$hol == "reg"]   # get value from that reg day & same hour
        if (length(reg_val) == 0) return(NA)
        return(reg_val)
      })) %>%
    ungroup()
  df
}

diagram_upd <- fill_nearest_reg_by_hour(diagram) %>% 
  arrange(year, date, hour)

diagram_core <- diagram # puvodni diagram (zaloha)
diagram <- diagram_upd # upraveny diagram (pro praci)

# ------------------------------------- set time range

val_y1 <- unique(max(diagram$year))+1 # v pripade, ze nebude diagram za jeden kalendarni rok, ale treba prelom (proto max)
val_y2 <- unique(max(diagram$year))+2
val_y3 <- unique(max(diagram$year))+3

# ------------------------------------- create future

future <- tibble(
  timestamp = seq(
    from = as.POSIXct(paste0(val_y1, "-01-01 00:00:00"), tz = "Europe/Prague"),
    to   = as.POSIXct(paste0(val_y3, "-12-31 23:00:00"), tz = "Europe/Prague"),
    by   = "hour")) %>% 
  mutate(
    date = as_date(timestamp),
    year = lubridate::year(date),
    month = lubridate::month(date),
    week = lubridate::week(date),
    day = lubridate::day(date),
    weekday = lubridate::wday(date, week_start = 1),
    md = paste0(month, "-", day),
    hol = case_when(
      md == ("1-1") ~ "novy_rok",
      md == ("5-1") ~ "svatek_prace",
      md == ("5-8") ~ "konec_valky",
      md == ("7-5") ~ "cyril_metod",
      md == ("7-6") ~ "jan_hus",
      md == ("9-28") ~ "sv_vaclav",
      md == ("10-28") ~ "vznik_statu",
      md == ("11-17") ~ "den_student",
      md == ("12-24") ~ "stedry_den",
      md == ("12-25") ~ "bozi_hod",
      md == ("12-26") ~ "sv_stepan",
      as.Date(date) %in% as.Date(c("2024-03-29", "2025-04-18", "2026-04-03", "2028-03-26")) ~ "easter_friday",
      as.Date(date) %in% as.Date(c("2024-03-30", "2025-04-19", "2026-04-04", "2028-03-27")) ~ "easter_saturday",
      as.Date(date) %in% as.Date(c("2024-03-31", "2025-04-20", "2026-04-05", "2028-03-28")) ~ "easter_sunday",
      as.Date(date) %in% as.Date(c("2024-04-01", "2025-04-21", "2026-04-06", "2028-03-29")) ~ "easter_monday", # pridat i ostatni roky ?
      TRUE ~ "reg")) %>% 
  group_by(date) %>% mutate(hour = seq_along(date)) %>% 
  ungroup() %>% 
  select(hol, date, year, month, week, weekday, hour) 

# ------------------------------------- join

join <- future %>% 
  left_join(diagram %>% select(week, weekday, hour, mwh_pair), by = c("week", "weekday", "hour")) %>% 
  rename("mwh" = mwh_pair) 

# ------------------------------------- konec roku

fill <- join %>% 
  group_by(hour) %>% 
  mutate(mwh_fill = mwh) %>% 
  fill(mwh_fill, .direction = "down") %>% 
  select(hol, date, year, month, week, weekday, hour, "mwh" = mwh_fill)

# ------------------------------------- prepis svatku

to_replace <- c( "novy_rok", 
                 "easter_friday", "easter_saturday", "easter_sunday", "easter_monday",
                 "svatek_prace", "konec_valky",
                 "cyril_metod", "jan_hus",
                 "sv_vaclav", "vznik_statu", "den_student",
                 "stedry_den", "bozi_hod", "sv_stepan")

# funkce
for (lev in to_replace) {
  fill$mwh[fill$hol == lev] <- diagram$mwh[diagram$hol == lev]
}

# ------------------------------------- uloz presvatkovany diagram

write_xlsx(fill, paste0(path, "aut_Rpresvatkovany_bohemiasekt_2025-2027.xlsx"))
