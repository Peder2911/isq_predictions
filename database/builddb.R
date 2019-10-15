
library(RSQLite)
library(haven)
library(readxl)
library(dplyr)
library(glue)
library(sf)
library(lubridate)
library(stringr)
library(tidyr)

#con <- dbConnect(SQLite(),"data.sqlite")

cinfo <- read.csv("data/country_info.csv", stringsAsFactors = FALSE)
acd <- read.csv("data/ucdp-prio-acd-191.csv", stringsAsFactors = FALSE)
regioninfo <- read.csv("data/region_info.csv", stringsAsFactors = FALSE)

predictions <- read_dta("data/CountryYearResults.dta")
oos_predictions <- read_dta("data/predactual_01_09.dta")

cshapes <- read_sf("cshapes/cshapes.shp")

start <- 1946
end <- 2011

nCountries <- local({
   read_dta("data/conflict_data.dta") %>%
      group_by(year) %>%
      summarize(n_countries = length(unique(gwno)))
})

# ================================================
cshapes <- cshapes %>%
   mutate(date = ymd(glue("{GWSYEAR}-{GWSMONTH}-{GWSDAY}"))) %>%
   group_by(GWCODE) %>%
   filter(date == max(date))

names(cshapes) <- tolower(names(cshapes))

# ================================================
countries <- cinfo %>%
   select(name,isoname,cowname,
          cowcode, gwcode, alpha3,
          area, capname, caplong, caplat,
          isonum, alpha2 = alpha2.x,
          iso3166_2, abb = StateAbb)

regionref <- regioninfo %>%
   select(gwcode = gwno, isqregion = region)

countries <- unique(merge(countries,regionref, by = "gwcode"))

regions  <- tibble(isqregion = sort(unique(regionref$isqregion)),
                   regionname = c("Southern Africa",
                                  "South and Central America and the Carribbean",
                                  "Western Europe, North America and Oceania",
                                  "Eastern Europe",
                                  "Western Asia and North Africa",
                                  "West Africa",
                                  "East and Central Africa",
                                  "South and Central Asia",
                                  "East and South-East Asia"))

# =================================================
# Acd
# Splitting, 
acd <- acd %>%
   select(conflict_id, gwcode_loc = gwno_loc,
          side_a = side_a_id, 
          side_b = side_b_id,
          side_a_gwcode = gwno_a,
          side_b_gwcode = gwno_b,
          incompatibility, year,
          intensity_level, cumulative_intensity,
          type_of_conflict, start_date)

acd$gwcode_loc <- str_split(acd$gwcode_loc, ",")

acd <- unique(unnest(acd, gwcode_loc))

# ================================================
# Predictions treatment (renaming variables)
# otherwise nwtd

predictions <- predictions %>%
   select(gwcode = gwno, year,
          minor = sh_cnt_t1, 
          major = sh_cnt_t2,
          combined = sh_cnt_c)

# ================================================
# OOS predictions
# Just remove NAs and select relevant variables.

oos_predictions <- oos_predictions[complete.cases(oos_predictions),] %>%
   select(gwcode = gwno, year,
          minor = c1,
          major = c2,
          combined = sim)

# =================================================
con <- dbConnect(SQLite(), "isq.sqlite")

dbWriteTable(con,"countries",countries, overwrite = TRUE)
dbWriteTable(con,"regions",regions, overwrite = TRUE)
dbWriteTable(con,"acd",acd, overwrite = TRUE)
dbWriteTable(con,"isq",predictions, overwrite = TRUE)
dbWriteTable(con,"oos",oos_predictions, overwrite = TRUE)
dbWriteTable(con,"ncountries",nCountries, overwrite = TRUE)

dbDisconnect(con)
