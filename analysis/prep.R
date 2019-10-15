
## @knitr getFromDB 

# =================================================

# Creates prepped.rds in data, which is a file with
# predictions and results for each country-year.

# This file can be used for further analysis.

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

## @knitr predictionsOccurrence 

pred_actual <- merge(predictions,occurrence,by="cntryyear",all.x = TRUE) %>%
   merge(cinfo,by = "gwcode") %>%
   unique() %>%
   rename(minor_prob = minor,
          major_prob = major) %>%
   mutate(minor_pred_30 = as.numeric(minor_prob > 0.3),
          minor_pred_50 = as.numeric(minor_prob > 0.5),
          major_pred_30 = as.numeric(major_prob > 0.3),
          major_pred_50 = as.numeric(major_prob > 0.5))

## @knitr fixActualNA

for(v in c("minor_actual","major_actual")){
   pred_actual[[v]] <- fixna(pred_actual[[v]]) 
}

## @knitr rollingFlip

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

## @knitr onsetTerm

onsetAndTerm <- function(data,...){
   # Is data grouped?
   if(!is.null(groups(data)[[1]])) {
         grplen <- length(unique(data[[groups(data)[[1]]]]))
   } else {
      grplen <- 0
   }

   # If so... 
   if(grplen > 1){
      grpvar <- groups(data)[[1]] # Only supports one grp. variable 
      prev <<- 0
      res <- lapply(unique(data[[grpvar]]), function(grpval){
         nmbr <- as.character(grpval)
         sub <- arrange(data[data[[grpvar]] == grpval,],year)
         onsetAndTerm(sub, ...)}) %>%
         bind_rows()
      res

   # If not... 
   } else {
      variables <- sapply(substitute(list(...)),as.character)[-1]
      for(v in variables){
         data[[paste0("onset_",v)]] <- rollingFlip(data[[v]],"onset")
         data[[paste0("term_",v)]] <- rollingFlip(data[[v]],"term")
      }
      data
   }

}

pred_actual <- pred_actual %>%
   group_by(gwcode) %>%
   onsetAndTerm(minor_actual,major_actual,
                minor_pred_50,minor_pred_30,
                major_pred_50,major_pred_30) %>%
   escalation(actual, pred_30, pred_50)

