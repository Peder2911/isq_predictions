
library(dplyr)
library(zoo)

#' bootstrap

#' confmat
#' 
#' Returns a 2x2 matrix with the results of a classification procedure. Useful
#' for computing further statistics.
#'
#' @param predicted Predicted values 
#' @param actual Actual values 
#' @return A 2x2 matrix with TN,FN,FP,TP
#' @examples
#' confmat(c(1,1,0),c(0,1,0))
#'
#'
#' @export
confmat <- function(predicted,actual){
   mat <- matrix(nrow = 2, ncol = 2)
   mat[1,1] <- sum(predicted == 0 & actual == 0)
   mat[1,2] <- sum(predicted == 1 & actual == 0)
   mat[2,1] <- sum(predicted == 0 & actual == 1)
   mat[2,2] <- sum(predicted == 1 & actual == 1)
   mat
}

#' withConfmat
#' 
#' Helper function. Computes a statistic from predicted and actual values,
#' using an intermediate confusion matrix.
#'
#' @param p Predicted values
#' @param a Actual values
#' @return The result of whatever function is passed as fun (recall/precision/accuracy etc.)
#' @examples
#' battery <- list(precision = precision, recall = recall,
#'                 fallout = fallout, accuracy = accuracy) 
#' pred <- sample(c(1,0),replace = TRUE, size = 150)
#' actual <- sample(c(1,0),replace = TRUE, size = 150)
#' lapply(battery,function(fun){
#'        withConfmat(pred,actual,fun)})
#' @export
withConfmat <- function(p,a,fun){
   fun(confmat(p,a))
}

#' recall
#' 
#'
#' @param confmat A confusion matrix
#' @return The recall value of the predicted and actual values
#' @export
recall <- function(confmat){
   confmat[2,2] / sum(confmat[2,]) 
}

#' precision 
#' 
#'
#' @param confmat A confusion matrix
#' @return The precision value of the predicted and actual values
#' @export
precision <- function(confmat){
   confmat[2,2] / sum(confmat[,2])
}

#' fallout 
#' 
#'
#' @param confmat A confusion matrix
#' @return The fallout value of the predicted and actual values
#' @export
fallout <- function(confmat){
   confmat[1,2] / sum(confmat[1,])
}

#' accuracy 
#' 
#'
#' @param confmat A confusion matrix
#' @return The accuracy value of the predicted and actual values
#' @export
accuracy <- function(confmat){
   (confmat[1,1] + confmat[2,2]) / sum(confmat)
}

#' fscore
#' 
#' The F-score is computed from precision and recall, representing a balance
#' between the two statistics.  
#'
#' @param precision Precision statistic
#' @param recall Recall statistic
#' @return F-statistic
fscore <- function(precision,recall,beta = 1){
   (1+ (beta^2)) * ((precision * recall)/(((beta^2) * precision) + (recall)))
}

#' categorizeClasf
#' 
#' Just names a combination of predicted / actual values as either TP, FP, TN, FN.
#'
#' @param pred Predicted values 
#' @param pred Actual values 
#' @return A character vector
#' @export
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

#' auc
#' 
#' Returns the Area Under Curve statistic, which is often used to interpret an ROC curve.
#'
#' @param x The X dimension of the ROC curve; Fallout scores.
#' @param y The Y dimension of the ROC curve; Recall scores.
#' @return An AUC score
#' @examples
#' predicted <- sample(c(1,0), replace = TRUE, size = 150)
#' actual <- sample(c(1,0), replace = TRUE, size = 150)
#' myRoc <- roc(predicted,actual)
#' auc(myRoc$fallout, myRoc$recall)
#' @export
auc <- function(x,y){
   # ? 
   sum(diff(x) * zoo::rollmean(y,2))
}

#' roc
#' 
#' Creates a data-frame that can be used to plot a ROC curve, or calculate the AUC.
#' Each row contains a fallout (x) and recall (y) score, that is calculated
#' with a given threshold for the predicted probabilities, which determines
#' which predicted values are interpreted as actual positives. 
#'
#' @param p Vector of predicted probabilities
#' @param actual Vector of actual scores
#' @return A data-frame of ROC scores.
#' @examples
#' predicted <- sample(seq(0,1,0.01),replace = TRUE, size = 150)
#' actual <- sample(c(1,0),replace = TRUE, size = 150)
#' roc(predicted,actual)
#' @export
roc <- function(p,actual,res = 0.01){
      lapply(seq(1,0,-res), function(threshold){
         predicted <- as.numeric(p > threshold)
         list(fallout = withConfmat(predicted,actual,fallout),
              recall = withConfmat(predicted,actual,recall),
              th = threshold)
      }) %>%
         bind_rows()
}

#' aucFromPA
#' 
#' Just a helper function that yields an AUC score from predicted and actual values. 
#'
#' @param p Vector of predicted probabilities
#' @param actual Vector of actual scores
#' @return An AUC score
#' @examples
#' predicted <- sample(seq(0,1,0.01),replace = TRUE, size = 150)
#' actual <- sample(c(1,0),replace = TRUE, size = 150)
#' aucFromPA(predicted,actual)
#' @export
aucFromPA <- function(p,actual, ...){
   roc <- roc(p,actual, ...)
   auc(roc$fallout, roc$recall)
}

either <- function(data,a,b){
   as.numeric(data[[as.character(substitute(a))]] | 
              data[[as.character(substitute(b))]])
}

#' ???
addConf <- function(data,predicted,actual){
   res <- apply(data,1,function(row){
      mat <- confmat(as.numeric(row[as.character(quote(predicted))]),
                     as.numeric(row[as.character(quote(actual))]))
      list(tn = mat[1,1],fp= mat[1,2],fn = mat[2,1], tp = mat[2,2])
   })
   cbind(data,do.call(rbind,res))
}
