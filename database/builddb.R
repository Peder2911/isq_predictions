
library(RSQLite)
library(haven)
library(readxl)
library(dplyr)
library(glue)
library(sf)
library(lubridate)
library(stringr)
library(tidyr)

# ================================================
# This file contains the code necessary to =======
# generate a database which is used to create ====
# for further analysis. ==========================
# ================================================
# The necessary raw data should be made available
# ================================================

# ================================================
# ================================================
#  Number of countries in each year.
#  Inferred from a file from the ISQ reproduction material,
#  which contains yearly data for each country. 
writeLines("\x1b[38;5;33m* Doing ncountries *\x1b[0m")

# This does not necessarily yield the actualy correct number of countries for
# each year, but rather the number of countries that were considered in the
# 2013 analysis.

# These numbers are used to construct the timeline in figure 5.

ncountries <- read_dta("data/conflict_data.dta") %>%
      group_by(year) %>%
      summarize(n_countries = length(unique(gwno)))


cshapes <- read_sf("data/cshapes/cshapes.shp")
st_geometry(cshapes) <- NULL

cshapes$startyear <- ifelse(cshapes$GWSYEAR == -1, 1946, cshapes$GWSYEAR)
cshapes$endyear <- ifelse(cshapes$GWEYEAR == -1, 2016, cshapes$GWEYEAR)

cshapes$year <- apply(cshapes,1,function(r){seq(r[["startyear"]],r[["endyear"]],1)})

ncountries_alt <- select(cshapes,year,gwcode = GWCODE) %>%
   unnest(year) %>%
   group_by(year) %>%
   summarize(n_countries = length(unique(gwcode)))

# ================================================
# ================================================
# Countries and regions information
# Used to name and group predictions.
writeLines("\x1b[38;5;44m* Doing country / region info *\x1b[0m")

countries <- read.csv("data/country_info.csv", stringsAsFactors = FALSE) %>%
   select(name,isoname,cowname,
          cowcode, gwcode, alpha3,
          area, capname, caplong, caplat,
          isonum, alpha2 = alpha2.x,
          iso3166_2, abb = StateAbb)

regionref <- read.csv("data/region_info.csv", stringsAsFactors = FALSE) %>%
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

# ================================================
# ================================================
# UCPD-PRIO ACD
# Transformed into a country-year dataset.
# Used as the reference to test predictions between
# 2001 - 2018
writeLines("\x1b[38;5;50m* Doing ACD *\x1b[0m")

acd <- read.csv("data/ucdp-prio-acd-191.csv", stringsAsFactors = FALSE) %>%
   select(conflict_id, gwcode_loc = gwno_loc,
          side_a = side_a_id, 
          side_b = side_b_id,
          side_a_gwcode = gwno_a,
          side_b_gwcode = gwno_b,
          incompatibility, year,
          intensity_level, cumulative_intensity,
          type_of_conflict, start_date)

acd$gwcode_loc <- str_split(acd$gwcode_loc, ",")

acd <- unique(unnest(acd, gwcode_loc)) %>%
   group_by(year,gwcode = gwcode_loc) %>%
   summarize(intensity_level = max(intensity_level)) %>%
   ungroup() %>%
   mutate(minor_actual = as.numeric(intensity_level == 1),
          major_actual = as.numeric(intensity_level == 2),
          either_actual = 1)

# ================================================
# ================================================
# Predictions 2010 - 2050 treatment 
# Renaming some variables, ensuring proper data types.
writeLines("\x1b[38;5;189m* Doing 2010-2050 predictions *\x1b[0m")

predictions_2010_2050 <- read_dta("data/CountryYearResults.dta")

predictions_2010_2050 <- predictions_2010_2050 %>%
   select(gwcode = gwno, year,
          minor = sh_cnt_t1, 
          major = sh_cnt_t2,
          combined = sh_cnt_c)

# Ensuring it's all numeric data:
predictions_2010_2050 <- lapply(predictions_2010_2050, as.numeric) %>%
   as.data.frame() 

# ================================================
# ================================================
# Predictions 2001 - 2009 
# Remove NAs, renaming some variables, ensuring proper data types.
writeLines("\x1b[38;5;105m* Doing 2001-2009 predictions *\x1b[0m")

predictions_2001_2009 <- read_dta("data/predactual_01_09.dta")

nonmissing <- complete.cases(predictions_2001_2009)
predictions_2001_2009 <- predictions_2001_2009[nonmissing,] %>%
   select(gwcode = gwno, year,
          minor = c1,
          major = c2,
          combined = sim) 

# Ensuring it's all numeric data:
predictions_2001_2009 <- lapply(predictions_2001_2009, as.numeric) %>%
   as.data.frame() 

# ================================================
# ================================================
# ================================================
# Writing everything to the database
writeLines("\x1b[38;5;226m* Writing database *\x1b[0m")

con <- dbConnect(SQLite(), "out/pac.sqlite")

dbWriteTable(con,"predictions_2010_2050",predictions_2010_2050, overwrite = TRUE)
dbWriteTable(con,"predictions_2001_2009",predictions_2001_2009, overwrite = TRUE)
dbWriteTable(con,"countries",countries, overwrite = TRUE)
dbWriteTable(con,"regions",regions, overwrite = TRUE)
dbWriteTable(con,"ncountries",ncountries, overwrite = TRUE)
dbWriteTable(con,"acd",acd, overwrite = TRUE)

dbDisconnect(con)

writeLines("\x1b[38;5;46m* Wrote to out/pac.sqlite *\x1b[0m")
writeLines("\x1b[38;5;46m* done! :) *\x1b[0m")
