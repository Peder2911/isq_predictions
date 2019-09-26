#!/usr/bin/Rscript

shh <- suppressPackageStartupMessages
library(RSQLite)
shh(library(dplyr))

# =================================================

# Creates prepped.rds in data, which is a file with
# predictions and results for each country-year.

# This file can be used for further analysis.

#' getfrom
#' 
#' Just a context-function that grabs q from the db
getfrom <- function(q,sqlite){
   con <- dbConnect(SQLite(),sqlite)
   res <- dbGetQuery(con,q)
   dbDisconnect(con)
   res
}

#' rollingFlip 
#' 
#' Rolls over a vector and "flips" the previous
#' value. Used to find onsets and terminations.
rollingFlip <- function(x,mode){
   flip <- function(x) as.numeric(!as.logical(x))
   mode_num <- switch(mode, onset = 1, term = 0)
   res <- sapply(1:length(x), function(i){
      if(i > 1){
         as.numeric(x[i-1] == flip(mode_num) &
                    x[i] == mode_num)
      } else {
         NA 
   }})
}

#' onsetAndTerm
#'
#' Applies rollingFlip to a possibly grouped dataset
#'
onsetAndTerm <- function(data,...){
   if(!is.null(groups(data)[[1]])) {
         grplen <- length(unique(data[[groups(data)[[1]]]]))
   } else {
      grplen <- 0
   }
   if(grplen > 1){
      grpvar <- groups(data)[[1]] # Only supports one grp. variable 

      print(glue::glue("Rolling over {length(unique(data[[grpvar]]))} separate groups "))

      prev <<- 0
      cat("Doing: ")
      res <- lapply(unique(data[[grpvar]]), function(grpval){
         nmbr <- as.character(grpval)
         cat(strrep("\b",prev))
         cat(nmbr)
         prev <<- nchar(nmbr)

         sub <- arrange(data[data[[grpvar]] == grpval,],year)
         onsetAndTerm(sub, ...)}) %>%
         bind_rows()
      cat("\n")
      res
   } else {
      variables <- sapply(substitute(list(...)),as.character)[-1]
      for(v in variables){
         data[[paste0("onset_",v)]] <- rollingFlip(data[[v]],"onset")
         data[[paste0("term_",v)]] <- rollingFlip(data[[v]],"term")
      }
      data
   }
}

#' escalation  
#'
#' Just a convenience function that makes an escalation-variable
#' This variable is 1 if a conflict goes from minor to major, and
#' -1 if it goes from major to minor.
escalation <- function(data,...){
   variables <- sapply(substitute(list(...)),as.character)[-1]
   for(v in variables){
      min_onset <- data[[paste0("onset_minor_",v)]]
      maj_onset <- data[[paste0("onset_major_",v)]]
      min_term <- data[[paste0("term_minor_",v)]]
      maj_term <- data[[paste0("term_major_",v)]]
      data[[paste0("escalation_",v)]] <- case_when(
         min_term == 1 & maj_onset == 1 ~ 1,
         maj_term == 1 & min_onset == 1 ~ -1,
         TRUE ~ 0
      )
   }
   data
}

#' fixna
#'
#' Throwaway function to replace missing values
fixna <- function(x){ifelse(is.na(x),0,x)}

# =================================================
DB <- "data/isq.sqlite"

predictions <- "
   SELECT DISTINCT
      CAST(gwcode AS INTEGER) || '-' || CAST(year AS INTEGER) AS cntryyear,
      *
   FROM isq 
   WHERE year <= 2018
   " %>%
   getfrom(DB)

occurrence <- "
   SELECT DISTINCT 
      gwcode_loc || '-' || year AS cntryyear,
      year,
      MAX(intensity_level) AS intensity_level
   FROM acd
   WHERE year <= 2018
   GROUP BY
      cntryyear 
   " %>%
   getfrom(DB) %>%
   mutate(minor_actual = as.numeric(intensity_level == 1),
          major_actual = as.numeric(intensity_level == 2)) %>%
   select(cntryyear,minor_actual,major_actual)

# =================================================

cinfo <- "
   SELECT 
      countries.gwcode,
      name,
      regions.regionname
   FROM countries
   JOIN regions ON countries.isqregion = regions.isqregion
   " %>%
   getfrom(DB)

pred_actual <- merge(predictions,occurrence,by="cntryyear",all.x = TRUE) %>%
   merge(cinfo,by = "gwcode") %>%
   unique() %>%
   rename(minor_prob = minor,
          major_prob = major) %>%
   mutate( minor_pred_30 = as.numeric(minor_prob > 0.3),
          minor_pred_50 = as.numeric(minor_prob > 0.5),
          major_pred_30 = as.numeric(major_prob > 0.3),
          major_pred_50 = as.numeric(major_prob > 0.5))

for(v in c("minor_actual","major_actual")){
   pred_actual[[v]] <- fixna(pred_actual[[v]]) 
}

pred_actual <- pred_actual %>%
   group_by(gwcode) %>%
   onsetAndTerm(minor_actual,major_actual,
                minor_pred_50,minor_pred_30,
                major_pred_50,major_pred_30) %>%
   escalation(actual, pred_30, pred_50)

saveRDS(pred_actual,"data/prepped.rds")

