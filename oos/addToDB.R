#!/usr/bin/Rscript

library(haven)
library(RSQLite)
library(dplyr)

# ================================================


dat <- read_dta("data/predictions_1946_2050.dta") %>%
   rename(gwcode = gwno,
          minor = sh_t1_r,
          major = sh_t2_r,
          combined = sh_c_r)

con <- dbConnect(SQLite(),"data/isq.sqlite")

if("isq_old" %in% dbListTables(con)) dbRemoveTable(con, "isq_old")
dbWriteTable(con,"isq_old", dat)
