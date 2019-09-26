#!/usr/bin/Rscript
# =================================================
# Fixes the regionmap data file by changing several 
# countries into south-africa.

# Writes the file to "fixed"

library(readxl)

regioninfo <- read_xls("data/regionmap.xls")
southaf <- readLines("data/southaf")
cinfo <- read.csv("data/country_info.csv")

# =================================================

southaf_info <- cinfo[cinfo$name %in% southaf,]
regioninfo$region <- ifelse(regioninfo$gwno %in% southaf_info$gwcode,
                            1,
                            regioninfo$region)

# =================================================
write.csv(regioninfo,"fixed/region_info.csv")
