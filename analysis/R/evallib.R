
library(dplyr)
library(zoo)

recall <- function(confmat){
   confmat[2,2] / sum(confmat[2,]) 
}

precision <- function(confmat){
   confmat[2,2] / sum(confmat[,2])
}

fallout <- function(confmat){
   confmat[1,2] / sum(confmat[1,])
}

accuracy <- function(confmat){
   (confmat[1,1] + confmat[2,2]) / sum(confmat)
}

confmat <- function(predicted,actual){
   mat <- matrix(nrow = 2, ncol = 2)
   mat[1,1] <- sum(predicted == 0 & actual == 0)
   mat[1,2] <- sum(predicted == 1 & actual == 0)
   mat[2,1] <- sum(predicted == 0 & actual == 1)
   mat[2,2] <- sum(predicted == 1 & actual == 1)
   mat
}

fscore <- function(precision,recall,beta = 1){
   (1+ (beta^2)) * ((precision * recall)/(((beta^2) * precision) + (recall)))
}

withConfmat <- function(p,a,fun){
   fun(confmat(p,a))
}

addConf <- function(data,predicted,actual){
   res <- apply(data,1,function(row){
      mat <- confmat(as.numeric(row[as.character(quote(predicted))]),
                     as.numeric(row[as.character(quote(actual))]))
      list(tn = mat[1,1],fp= mat[1,2],fn = mat[2,1], tp = mat[2,2])
   })
   cbind(data,do.call(rbind,res))
}

categorizeClasf <- function(pred,act){
   sapply(1:length(pred), function(i){
      predicted <- pred[i]
      actual <- act[i]
      if(predicted == 1 & actual == 1){
         'TP'
      } else if(predicted == 1 & actual == 0){
         'FP'
      } else if(predicted == 0 & actual == 0){
         'TN'
      } else if(predicted == 0 & actual == 1){
         'FN'
      }
   })
}

auc <- function(x,y){
   # ? 
   sum(diff(x) * zoo::rollmean(y,2))
}

roc <- function(p,actual,res = 0.01, draws = 100){
      lapply(seq(1,0,-res), function(threshold){
         predicted <- as.numeric(p > threshold)
         list(fallout = withConfmat(predicted,actual,fallout),
              recall = withConfmat(predicted,actual,recall),
              th = threshold)
      }) %>%
         bind_rows()
}

aucFromPA <- function(p,actual, ...){
   roc <- roc(p,actual, ...)
   auc(roc$fallout, roc$recall)
}

either <- function(data,a,b){
   as.numeric(data[[as.character(substitute(a))]] | 
              data[[as.character(substitute(b))]])
}
