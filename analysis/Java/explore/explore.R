# Rattle is Copyright (c) 2006-2014 Togaware Pty Ltd.

#============================================================
# Rattle timestamp: 2014-12-06 20:44:21 x86_64-pc-linux-gnu 

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
# Rattle timestamp: 2014-12-06 20:44:28 x86_64-pc-linux-gnu 

# Load the data.

crs$dataset <- read.csv("file:///home/kanashiro/Documents/UnB/ML/ML_metrics/data/Java/all-data-Java.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")

#============================================================
# Rattle timestamp: 2014-12-06 20:44:31 x86_64-pc-linux-gnu 

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
# Rattle timestamp: 2014-12-06 20:44:38 x86_64-pc-linux-gnu 

# Generate a correlation plot for the variables. 

# The 'corrplot' package provides the 'corrplot' function.

require(corrplot, quietly=TRUE)

# Correlations work for numeric variables only.

crs$cor <- cor(crs$dataset[crs$sample, crs$numeric], use="pairwise", method="pearson")

# Order the correlations by their strength.

crs$ord <- order(crs$cor[1,])
crs$cor <- crs$cor[crs$ord, crs$ord]

# Display the actual correlations.

print(crs$cor)

# Graphically display the correlations.

corrplot(crs$cor, mar=c(0,0,1,0))
title(main="Correlation all-data-Java.csv using Pearson",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

#============================================================
# Rattle timestamp: 2014-12-06 20:44:58 x86_64-pc-linux-gnu 

# Save the plot to a file. 

# Save the plot on device 2 to a file.

library(cairoDevice)
savePlotToFile("/home/kanashiro/Documents/UnB/ML/ML_metrics/analysis/Java/explore/correlation.pdf", 2)

#============================================================
# Rattle timestamp: 2014-12-06 20:45:32 x86_64-pc-linux-gnu 

# Box Plot 

# The 'ggplot2' package provides the 'ggplot' function.

library(ggplot2)

# Box Plot for noa

p <- ggplot(with(crs, dataset[sample,]), aes(y=noa))
p <- p + geom_boxplot(aes(x="All"), notch=TRUE, fill="grey")
p <- p + stat_summary(aes(x="All"), fun.y=mean, geom="point", shape=8)
p <- p + geom_boxplot(aes(x=good_design, fill=good_design), notch=TRUE)
p <- p + stat_summary(aes(x=good_design), fun.y=mean, geom="point", shape=8)
p <- p + xlab("good_design\n\nRattle 2014-Dec-06 20:45:32 kanashiro")
p <- p + ggtitle("Distribution of noa (sample)\nby good_design")
p <- p + theme(legend.position="none")
print(p)

# Box Plot for nom

p <- ggplot(with(crs, dataset[sample,]), aes(y=nom))
p <- p + geom_boxplot(aes(x="All"), notch=TRUE, fill="grey")
p <- p + stat_summary(aes(x="All"), fun.y=mean, geom="point", shape=8)
p <- p + geom_boxplot(aes(x=good_design, fill=good_design), notch=TRUE)
p <- p + stat_summary(aes(x=good_design), fun.y=mean, geom="point", shape=8)
p <- p + xlab("good_design\n\nRattle 2014-Dec-06 20:45:34 kanashiro")
p <- p + ggtitle("Distribution of nom (sample)\nby good_design")
p <- p + theme(legend.position="none")
print(p)

# Box Plot for sc

p <- ggplot(with(crs, dataset[sample,]), aes(y=sc))
p <- p + geom_boxplot(aes(x="All"), notch=TRUE, fill="grey")
p <- p + stat_summary(aes(x="All"), fun.y=mean, geom="point", shape=8)
p <- p + geom_boxplot(aes(x=good_design, fill=good_design), notch=TRUE)
p <- p + stat_summary(aes(x=good_design), fun.y=mean, geom="point", shape=8)
p <- p + xlab("good_design\n\nRattle 2014-Dec-06 20:45:36 kanashiro")
p <- p + ggtitle("Distribution of sc (sample)\nby good_design")
p <- p + theme(legend.position="none")
print(p)

#============================================================
# Rattle timestamp: 2014-12-06 20:45:47 x86_64-pc-linux-gnu 

# Save the plot to a file. 

# Save the plot on device 4 to a file.

library(cairoDevice)
savePlotToFile("/home/kanashiro/Documents/UnB/ML/ML_metrics/analysis/Java/explore/distributions/sc.pdf", 4)

#============================================================
# Rattle timestamp: 2014-12-06 20:45:58 x86_64-pc-linux-gnu 

# Save the plot to a file. 

# Save the plot on device 3 to a file.

library(cairoDevice)
savePlotToFile("/home/kanashiro/Documents/UnB/ML/ML_metrics/analysis/Java/explore/distributions/nom.pdf", 3)

#============================================================
# Rattle timestamp: 2014-12-06 20:46:12 x86_64-pc-linux-gnu 

# Save the plot to a file. 

# Save the plot on device 2 to a file.

library(cairoDevice)
savePlotToFile("/home/kanashiro/Documents/UnB/ML/ML_metrics/analysis/Java/explore/distributions/noa.pdf", 2)
