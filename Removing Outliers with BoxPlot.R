###---------------------------------------------------------------------------------------------
###   Removing Outliers In Each Column Based On Box Plot
###---------------------------------------------------------------------------------------------

Data <- read.csv("Admission_Predict.csv")
head(Data)

# Remove variable 'Seral.No.'
Data <- Data[,-1]
head(Data)

# Create a function drawing boxplot 
library(ggplot2)
drawBoxPlot <- function(i){
  ggplot(data = Data, mapping = aes(y=Data[,i]))+
    geom_boxplot(fill="slategrey",color='darkslategrey',width=0.2, outlier.color = 'red', outlier.shape = 2)+
    ylab("value")+
    scale_x_discrete(names(Data)[i]) +
    theme_light()
}

# Plot all 8 graphs in the same screen
library("gridExtra")
pl <- lapply(1:8, function(x) drawBoxPlot(.x))
marrangeGrob(pl, nrow=2, ncol=4)


# Makes outliers N/A
removeOutliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  halfRange <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - halfRange)] <- NA
  y[x > (qnt[2] + halfRange)] <- NA
  y
}


# Apply function on all variables 
After = Data
for(i in 1:8){
  After[,i] = removeOutliers(Data[,i])
}

# Remove rows containing N/A
After = na.omit(After)
