
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

#' Either
#'
#' Saves some typing
either <- function(data_t2,a,b){
   as.numeric(data_t2[[as.character(substitute(a))]] | 
              data_t2[[as.character(substitute(b))]])
}
