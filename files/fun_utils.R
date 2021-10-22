# Package holding a few functions to transform variables and to produce ROC and PR plots

#Loading packages
library(ROCR)

# Function to transform a set of selected variables to factor
to_factors <- function(df, columns) {
  for (col in columns) {
    df[[col]] <- as.factor(df[[col]])
  }
  return(df)
}

# Function to transform a set of selected variables to numeric
to_numeric <- function(df, columns) {
  for (col in columns) {
    df[[col]] <- as.numeric(df[[col]])
  }
  return(df)
}

# Function to transform a set of selected variables to integer
to_integer <- function(df, columns) {
  for (col in columns) {
    df[[col]] <- as.integer(df[[col]])
  }
  return(df)
}

# Function to scale a set of selected numeric variables
to_scale <- function(df, columns) {
  for (col in columns) {
    df[[col]] <- scale(x = df[[col]], center = T, scale = T)
  }
  return(df)
}

# Function to produce an ROC plot
plot.roc.curve <- function(predictions, title.text){
  perf <- performance(predictions, "tpr", "fpr")
  plot(perf, col = "black", lty = 1, lwd = 2, 
       main = title.text, cex.main = 0.6, 
       cex.lab = 0.8, xaxs="i", yaxs="i")
  abline(0,1, col = "red")
  auc <- performance(predictions, "auc")
  auc <- unlist(slot(auc, "y.values"))
  auc <- round(auc,2)
  legend(0.4,0.4, legend = c(paste0("AUC: ",auc)), cex = 0.6, bty = "n", box.col = "white")
  
}

# Function to produce a PR plot
plot.pr.curve <- function(predictions, title.text){
  perf <- performance(predictions, "prec", "rec")
  plot(perf, col = "black", lty = 1, lwd = 2,
       main = title.text, cex.main = 0.6, cex.lab = 0.8, xaxs = "i", yaxs = "i")
}