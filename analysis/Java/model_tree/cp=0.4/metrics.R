# Rattle is Copyright (c) 2006-2014 Togaware Pty Ltd.

#============================================================
# Rattle timestamp: 2014-12-06 19:44:15 x86_64-pc-linux-gnu 

# Rattle version 3.3.1 user 'kanashiro'

# Export this log textview to a file using the Export button or the Tools 
# menu to save a log of all activity. This facilitates repeatability. Exporting 
# to file 'myrf01.R', for example, allows us to the type in the R Console 
# the command source('myrf01.R') to repeat the process automatically. 
# Generally, we may want to edit the file to suit our needs. We can also directly 
# edit this current log textview to record additional information before exporting. 
 
# Saving and loading projects also retains this log.

library(rattle)

# This log generally records the process of building a model. However, with very 
# little effort the log can be used to score a new dataset. The logical variable 
# 'building' is used to toggle between generating transformations, as when building 
# a model, and simply using the transformations, as when scoring a dataset.

building <- TRUE
scoring  <- ! building

# The colorspace package is used to generate the colours used in plots, if available.

library(colorspace)

# A pre-defined value is used to reset the random seed so that results are repeatable.

crv$seed <- 42 

#============================================================
# Rattle timestamp: 2014-12-06 19:44:31 x86_64-pc-linux-gnu 

# Load the data.

crs$dataset <- read.csv("file:///home/kanashiro/Documents/UnB/ML/ML_metrics/data/Java/all-data-Java.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")

#============================================================
# Rattle timestamp: 2014-12-06 19:44:34 x86_64-pc-linux-gnu 

# Note the user selections. 

# Build the training/validate/test datasets.

set.seed(crv$seed) 
crs$nobs <- nrow(crs$dataset) # 135846 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.7*crs$nobs) # 95092 observations
crs$validate <- sample(setdiff(seq_len(nrow(crs$dataset)), crs$train), 0.15*crs$nobs) # 20376 observations
crs$test <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate) # 20378 observations

# The following variable selections have been noted.

crs$input <- c("acc", "accm", "amloc", "anpm",
     "cbo", "dit", "lcom4", "loc",
     "noa", "noc", "nom", "npa",
     "npm", "rfc", "sc")

crs$numeric <- c("acc", "accm", "amloc", "anpm",
     "cbo", "dit", "lcom4", "loc",
     "noa", "noc", "nom", "npa",
     "npm", "rfc", "sc")

crs$categoric <- NULL

crs$target  <- "good_design"
crs$risk    <- NULL
crs$ident   <- NULL
crs$ignore  <- NULL
crs$weights <- NULL

#============================================================
# Rattle timestamp: 2014-12-06 19:44:45 x86_64-pc-linux-gnu 

# Transform variables by rescaling. 

# Rescale acc.

crs$dataset[["R10_acc"]] <- crs$dataset[["acc"]]

# Take a log10 transform of the variable - treat -Inf as NA.

if (building)
{
  crs$dataset[["R10_acc"]] <-  log10(crs$dataset[["acc"]]) 
  crs$dataset[crs$dataset[["R10_acc"]] == -Inf & ! is.na(crs$dataset[["R10_acc"]]), "R10_acc"] <- NA
}

# When scoring transform using the training data parameters.

if (scoring)
{
  crs$dataset[["R10_acc"]] <-  log10(crs$dataset[["acc"]]) 
  crs$dataset[crs$dataset[["R10_acc"]] == -Inf & ! is.na(crs$dataset[["R10_acc"]]), "R10_acc"] <- NA
}

# Rescale accm.

crs$dataset[["R10_accm"]] <- crs$dataset[["accm"]]

# Take a log10 transform of the variable - treat -Inf as NA.

if (building)
{
  crs$dataset[["R10_accm"]] <-  log10(crs$dataset[["accm"]]) 
  crs$dataset[crs$dataset[["R10_accm"]] == -Inf & ! is.na(crs$dataset[["R10_accm"]]), "R10_accm"] <- NA
}

# When scoring transform using the training data parameters.

if (scoring)
{
  crs$dataset[["R10_accm"]] <-  log10(crs$dataset[["accm"]]) 
  crs$dataset[crs$dataset[["R10_accm"]] == -Inf & ! is.na(crs$dataset[["R10_accm"]]), "R10_accm"] <- NA
}

# Rescale amloc.

crs$dataset[["R10_amloc"]] <- crs$dataset[["amloc"]]

# Take a log10 transform of the variable - treat -Inf as NA.

if (building)
{
  crs$dataset[["R10_amloc"]] <-  log10(crs$dataset[["amloc"]]) 
  crs$dataset[crs$dataset[["R10_amloc"]] == -Inf & ! is.na(crs$dataset[["R10_amloc"]]), "R10_amloc"] <- NA
}

# When scoring transform using the training data parameters.

if (scoring)
{
  crs$dataset[["R10_amloc"]] <-  log10(crs$dataset[["amloc"]]) 
  crs$dataset[crs$dataset[["R10_amloc"]] == -Inf & ! is.na(crs$dataset[["R10_amloc"]]), "R10_amloc"] <- NA
}

# Rescale anpm.

crs$dataset[["R10_anpm"]] <- crs$dataset[["anpm"]]

# Take a log10 transform of the variable - treat -Inf as NA.

if (building)
{
  crs$dataset[["R10_anpm"]] <-  log10(crs$dataset[["anpm"]]) 
  crs$dataset[crs$dataset[["R10_anpm"]] == -Inf & ! is.na(crs$dataset[["R10_anpm"]]), "R10_anpm"] <- NA
}

# When scoring transform using the training data parameters.

if (scoring)
{
  crs$dataset[["R10_anpm"]] <-  log10(crs$dataset[["anpm"]]) 
  crs$dataset[crs$dataset[["R10_anpm"]] == -Inf & ! is.na(crs$dataset[["R10_anpm"]]), "R10_anpm"] <- NA
}

# Rescale cbo.

crs$dataset[["R10_cbo"]] <- crs$dataset[["cbo"]]

# Take a log10 transform of the variable - treat -Inf as NA.

if (building)
{
  crs$dataset[["R10_cbo"]] <-  log10(crs$dataset[["cbo"]]) 
  crs$dataset[crs$dataset[["R10_cbo"]] == -Inf & ! is.na(crs$dataset[["R10_cbo"]]), "R10_cbo"] <- NA
}

# When scoring transform using the training data parameters.

if (scoring)
{
  crs$dataset[["R10_cbo"]] <-  log10(crs$dataset[["cbo"]]) 
  crs$dataset[crs$dataset[["R10_cbo"]] == -Inf & ! is.na(crs$dataset[["R10_cbo"]]), "R10_cbo"] <- NA
}

# Rescale dit.

crs$dataset[["R10_dit"]] <- crs$dataset[["dit"]]

# Take a log10 transform of the variable - treat -Inf as NA.

if (building)
{
  crs$dataset[["R10_dit"]] <-  log10(crs$dataset[["dit"]]) 
  crs$dataset[crs$dataset[["R10_dit"]] == -Inf & ! is.na(crs$dataset[["R10_dit"]]), "R10_dit"] <- NA
}

# When scoring transform using the training data parameters.

if (scoring)
{
  crs$dataset[["R10_dit"]] <-  log10(crs$dataset[["dit"]]) 
  crs$dataset[crs$dataset[["R10_dit"]] == -Inf & ! is.na(crs$dataset[["R10_dit"]]), "R10_dit"] <- NA
}

# Rescale lcom4.

crs$dataset[["R10_lcom4"]] <- crs$dataset[["lcom4"]]

# Take a log10 transform of the variable - treat -Inf as NA.

if (building)
{
  crs$dataset[["R10_lcom4"]] <-  log10(crs$dataset[["lcom4"]]) 
  crs$dataset[crs$dataset[["R10_lcom4"]] == -Inf & ! is.na(crs$dataset[["R10_lcom4"]]), "R10_lcom4"] <- NA
}

# When scoring transform using the training data parameters.

if (scoring)
{
  crs$dataset[["R10_lcom4"]] <-  log10(crs$dataset[["lcom4"]]) 
  crs$dataset[crs$dataset[["R10_lcom4"]] == -Inf & ! is.na(crs$dataset[["R10_lcom4"]]), "R10_lcom4"] <- NA
}

# Rescale loc.

crs$dataset[["R10_loc"]] <- crs$dataset[["loc"]]

# Take a log10 transform of the variable - treat -Inf as NA.

if (building)
{
  crs$dataset[["R10_loc"]] <-  log10(crs$dataset[["loc"]]) 
  crs$dataset[crs$dataset[["R10_loc"]] == -Inf & ! is.na(crs$dataset[["R10_loc"]]), "R10_loc"] <- NA
}

# When scoring transform using the training data parameters.

if (scoring)
{
  crs$dataset[["R10_loc"]] <-  log10(crs$dataset[["loc"]]) 
  crs$dataset[crs$dataset[["R10_loc"]] == -Inf & ! is.na(crs$dataset[["R10_loc"]]), "R10_loc"] <- NA
}

# Rescale noa.

crs$dataset[["R10_noa"]] <- crs$dataset[["noa"]]

# Take a log10 transform of the variable - treat -Inf as NA.

if (building)
{
  crs$dataset[["R10_noa"]] <-  log10(crs$dataset[["noa"]]) 
  crs$dataset[crs$dataset[["R10_noa"]] == -Inf & ! is.na(crs$dataset[["R10_noa"]]), "R10_noa"] <- NA
}

# When scoring transform using the training data parameters.

if (scoring)
{
  crs$dataset[["R10_noa"]] <-  log10(crs$dataset[["noa"]]) 
  crs$dataset[crs$dataset[["R10_noa"]] == -Inf & ! is.na(crs$dataset[["R10_noa"]]), "R10_noa"] <- NA
}

# Rescale noc.

crs$dataset[["R10_noc"]] <- crs$dataset[["noc"]]

# Take a log10 transform of the variable - treat -Inf as NA.

if (building)
{
  crs$dataset[["R10_noc"]] <-  log10(crs$dataset[["noc"]]) 
  crs$dataset[crs$dataset[["R10_noc"]] == -Inf & ! is.na(crs$dataset[["R10_noc"]]), "R10_noc"] <- NA
}

# When scoring transform using the training data parameters.

if (scoring)
{
  crs$dataset[["R10_noc"]] <-  log10(crs$dataset[["noc"]]) 
  crs$dataset[crs$dataset[["R10_noc"]] == -Inf & ! is.na(crs$dataset[["R10_noc"]]), "R10_noc"] <- NA
}

# Rescale nom.

crs$dataset[["R10_nom"]] <- crs$dataset[["nom"]]

# Take a log10 transform of the variable - treat -Inf as NA.

if (building)
{
  crs$dataset[["R10_nom"]] <-  log10(crs$dataset[["nom"]]) 
  crs$dataset[crs$dataset[["R10_nom"]] == -Inf & ! is.na(crs$dataset[["R10_nom"]]), "R10_nom"] <- NA
}

# When scoring transform using the training data parameters.

if (scoring)
{
  crs$dataset[["R10_nom"]] <-  log10(crs$dataset[["nom"]]) 
  crs$dataset[crs$dataset[["R10_nom"]] == -Inf & ! is.na(crs$dataset[["R10_nom"]]), "R10_nom"] <- NA
}

# Rescale npa.

crs$dataset[["R10_npa"]] <- crs$dataset[["npa"]]

# Take a log10 transform of the variable - treat -Inf as NA.

if (building)
{
  crs$dataset[["R10_npa"]] <-  log10(crs$dataset[["npa"]]) 
  crs$dataset[crs$dataset[["R10_npa"]] == -Inf & ! is.na(crs$dataset[["R10_npa"]]), "R10_npa"] <- NA
}

# When scoring transform using the training data parameters.

if (scoring)
{
  crs$dataset[["R10_npa"]] <-  log10(crs$dataset[["npa"]]) 
  crs$dataset[crs$dataset[["R10_npa"]] == -Inf & ! is.na(crs$dataset[["R10_npa"]]), "R10_npa"] <- NA
}

# Rescale npm.

crs$dataset[["R10_npm"]] <- crs$dataset[["npm"]]

# Take a log10 transform of the variable - treat -Inf as NA.

if (building)
{
  crs$dataset[["R10_npm"]] <-  log10(crs$dataset[["npm"]]) 
  crs$dataset[crs$dataset[["R10_npm"]] == -Inf & ! is.na(crs$dataset[["R10_npm"]]), "R10_npm"] <- NA
}

# When scoring transform using the training data parameters.

if (scoring)
{
  crs$dataset[["R10_npm"]] <-  log10(crs$dataset[["npm"]]) 
  crs$dataset[crs$dataset[["R10_npm"]] == -Inf & ! is.na(crs$dataset[["R10_npm"]]), "R10_npm"] <- NA
}

# Rescale rfc.

crs$dataset[["R10_rfc"]] <- crs$dataset[["rfc"]]

# Take a log10 transform of the variable - treat -Inf as NA.

if (building)
{
  crs$dataset[["R10_rfc"]] <-  log10(crs$dataset[["rfc"]]) 
  crs$dataset[crs$dataset[["R10_rfc"]] == -Inf & ! is.na(crs$dataset[["R10_rfc"]]), "R10_rfc"] <- NA
}

# When scoring transform using the training data parameters.

if (scoring)
{
  crs$dataset[["R10_rfc"]] <-  log10(crs$dataset[["rfc"]]) 
  crs$dataset[crs$dataset[["R10_rfc"]] == -Inf & ! is.na(crs$dataset[["R10_rfc"]]), "R10_rfc"] <- NA
}

# Rescale sc.

crs$dataset[["R10_sc"]] <- crs$dataset[["sc"]]

# Take a log10 transform of the variable - treat -Inf as NA.

if (building)
{
  crs$dataset[["R10_sc"]] <-  log10(crs$dataset[["sc"]]) 
  crs$dataset[crs$dataset[["R10_sc"]] == -Inf & ! is.na(crs$dataset[["R10_sc"]]), "R10_sc"] <- NA
}

# When scoring transform using the training data parameters.

if (scoring)
{
  crs$dataset[["R10_sc"]] <-  log10(crs$dataset[["sc"]]) 
  crs$dataset[crs$dataset[["R10_sc"]] == -Inf & ! is.na(crs$dataset[["R10_sc"]]), "R10_sc"] <- NA
}

#============================================================
# Rattle timestamp: 2014-12-06 19:44:48 x86_64-pc-linux-gnu 

# Note the user selections. 

# The following variable selections have been noted.

crs$input <- c("R10_acc", "R10_accm", "R10_amloc", "R10_anpm",
     "R10_cbo", "R10_dit", "R10_lcom4", "R10_loc",
     "R10_noa", "R10_noc", "R10_nom", "R10_npa",
     "R10_npm", "R10_rfc", "R10_sc")

crs$numeric <- c("R10_acc", "R10_accm", "R10_amloc", "R10_anpm",
     "R10_cbo", "R10_dit", "R10_lcom4", "R10_loc",
     "R10_noa", "R10_noc", "R10_nom", "R10_npa",
     "R10_npm", "R10_rfc", "R10_sc")

crs$categoric <- NULL

crs$target  <- "good_design"
crs$risk    <- NULL
crs$ident   <- NULL
crs$ignore  <- c("acc", "accm", "amloc", "anpm", "cbo", "dit", "lcom4", "loc", "noa", "noc", "nom", "npa", "npm", "rfc", "sc")
crs$weights <- NULL

#============================================================
# Rattle timestamp: 2014-12-06 19:44:54 x86_64-pc-linux-gnu 

# Decision Tree 

# The 'rpart' package provides the 'rpart' function.

require(rpart, quietly=TRUE)

# Reset the random number seed to obtain the same results each time.

set.seed(crv$seed)

# Build the Decision Tree model.

crs$rpart <- rpart(good_design ~ .,
    data=crs$dataset[crs$train, c(crs$input, crs$target)],
    method="class",
    parms=list(split="information"),
      control=rpart.control(cp=0.040000,
        usesurrogate=0, 
        maxsurrogate=0))

# Generate a textual view of the Decision Tree model.

print(crs$rpart)
printcp(crs$rpart)
cat("\n")

# Time taken: 1.65 secs

# List the rules from the tree using a Rattle support function.

asRules(crs$rpart)

#============================================================
# Rattle timestamp: 2014-12-06 19:45:21 x86_64-pc-linux-gnu 

# Plot the resulting Decision Tree. 

# We use the rpart.plot package.

fancyRpartPlot(crs$rpart, main="Decision Tree all-data-Java.csv $ good_design")

#============================================================
# Rattle timestamp: 2014-12-06 19:45:42 x86_64-pc-linux-gnu 

# Save the plot to a file. 

# Save the plot on device 2 to a file.

library(cairoDevice)
savePlotToFile("/home/kanashiro/Documents/UnB/ML/ML_metrics/analysis/Java/model/cp=0.4/decision_tree.pdf", 2)

#============================================================
# Rattle timestamp: 2014-12-06 19:45:53 x86_64-pc-linux-gnu 

# Evaluate model performance. 

# Generate an Error Matrix for the Decision Tree model.

# Obtain the response from the Decision Tree model.

crs$pr <- predict(crs$rpart, newdata=crs$dataset[crs$validate, c(crs$input, crs$target)], type="class")

# Generate the confusion matrix showing counts.

table(crs$dataset[crs$validate, c(crs$input, crs$target)]$good_design, crs$pr,
        dnn=c("Actual", "Predicted"))

# Generate the confusion matrix showing proportions.

pcme <- function(actual, cl)
{
  x <- table(actual, cl)
  tbl <- cbind(round(x/length(actual), 2),
               Error=round(c(x[1,2]/sum(x[1,]),
                             x[2,1]/sum(x[2,])), 2))
  names(attr(tbl, "dimnames")) <- c("Actual", "Predicted")
  return(tbl)
};
pcme(crs$dataset[crs$validate, c(crs$input, crs$target)]$good_design, crs$pr)

# Calculate the overall error percentage.

overall <- function(x)
{
  if (nrow(x) == 2) 
    cat((x[1,2] + x[2,1]) / sum(x)) 
  else
    cat(1 - (x[1,rownames(x)]) / sum(x))
} 
overall(table(crs$pr, crs$dataset[crs$validate, c(crs$input, crs$target)]$good_design,  
        dnn=c("Predicted", "Actual")))

# Calculate the averaged class error percentage.

avgerr <- function(x) 
	cat(mean(c(x[1,2], x[2,1]) / apply(x, 1, sum))) 
avgerr(table(crs$pr, crs$dataset[crs$validate, c(crs$input, crs$target)]$good_design,  
        dnn=c("Predicted", "Actual")))

#============================================================
# Rattle timestamp: 2014-12-06 19:46:15 x86_64-pc-linux-gnu 

# Evaluate model performance. 

# ROC Curve: requires the ROCR package.

library(ROCR)

# ROC Curve: requires the ggplot2 package.

require(ggplot2, quietly=TRUE)

# Generate an ROC Curve for the rpart model on all-data-Java.csv [validate].

crs$pr <- predict(crs$rpart, newdata=crs$dataset[crs$validate, c(crs$input, crs$target)])[,2]

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)]$good_design)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}

pe <- performance(pred, "tpr", "fpr")
au <- performance(pred, "auc")@y.values[[1]]
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve Decision Tree all-data-Java.csv [validate] good_design")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                   label=paste("AUC =", round(au, 2)))
print(p)

# Calculate the area under the curve for the plot.


# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)]$good_design)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
performance(pred, "auc")

#============================================================
# Rattle timestamp: 2014-12-06 19:46:28 x86_64-pc-linux-gnu 

# Save the plot to a file. 

# Save the plot on device 2 to a file.

library(cairoDevice)
savePlotToFile("/home/kanashiro/Documents/UnB/ML/ML_metrics/analysis/Java/model/cp=0.4/ROC.pdf", 2)
